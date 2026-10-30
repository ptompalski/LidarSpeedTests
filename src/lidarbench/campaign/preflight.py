"""Read-only readiness check of a campaign configuration (``lidarbench preflight``).

The preflight answers "would ``lidarbench run CONFIG.toml`` start cleanly here,
and what would it record?" without changing anything:

* it never creates the campaign, the workspace or scratch directories, and never
  writes into the workspace or the datasets;
* it never takes a lock; workspace and machine locks are only inspected;
* it runs no benchmarked tool beyond the adapters' normal ``probe()`` /
  ``identify()`` (the same calls ``lidarbench check`` makes);
* it does not hash the inputs unless asked to (``--hash``); the cheap identity
  components of an existing campaign (code, configuration, workloads, tools,
  environment, schedule) are compared with the frozen ones.

Every check produces a :class:`CheckItem` (``ok``, ``warning``, ``error`` or
``skipped``). The report is ready when no item is an error.
"""

from __future__ import annotations

import os
import shutil
import sys
from collections.abc import Callable, Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import psutil

from lidarbench.campaign.identity import (
    IDENTITY_COMPONENTS,
    IdentityError,
    ResolvedTool,
    code_component,
    code_files,
    diff_records,
    environment_component,
    git_provenance,
    hash_dataset,
    package_root,
    resolve_tool,
    workloads_component,
)
from lidarbench.campaign.schedule import campaign_schedule, schedule_record
from lidarbench.campaign.store import (
    CampaignExistsError,
    CampaignIntegrityError,
    CampaignStore,
    campaign_dir,
)
from lidarbench.config.campaign import (
    CampaignConfig,
    ConfigError,
    DatasetConfig,
    WorkloadConfig,
    load_config,
)
from lidarbench.core.canonical import digest
from lidarbench.core.locks import (
    MACHINE_LOCK_NAME,
    WORKSPACE_LOCK_NAME,
    LockReport,
    default_state_dir,
    inspect_lock,
)
from lidarbench.core.platform import PlatformInfo, get_platform_info
from lidarbench.core.scratch import (
    SCRATCH_PREFIX,
    ScratchError,
    paths_overlap,
    validate_scratch_root,
)
from lidarbench.datasets.lasheader import LasHeaderError, read_las_header
from lidarbench.datasets.roots import DataRoots
from lidarbench.datasets.verify import verify_dataset
from lidarbench.execution.adapters.base import Invocation
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.plan import AdapterContractError, plan_invocation

PREFLIGHT_SCHEMA = "lidarbench.preflight/1"
STATUSES = ("ok", "warning", "error", "skipped")
GIB = 1024**3
MIN_FREE_BYTES = GIB
LOAD_BUSY_FRACTION = 0.25
CPU_BUSY_PERCENT = 20.0
LOW_MEMORY_BYTES = 2 * GIB
POINT_CLOUD_SUFFIXES = (".las", ".laz")
LASR_ADAPTER = "lasr"
QUARTO_ENV = "QUARTO"
RSCRIPT_ENV = "LIDARBENCH_RSCRIPT"

GitInfo = Callable[[], dict[str, Any]]


@dataclass(frozen=True)
class CheckItem:
    """Outcome of one preflight check."""

    id: str
    status: str
    message: str
    details: Mapping[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if self.status not in STATUSES:
            raise ValueError(f"status must be one of {STATUSES}, got {self.status!r}")

    @property
    def group(self) -> str:
        """Check group (the part of the ID before the first dot)."""
        return self.id.split(".", 1)[0]

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "id": self.id,
            "group": self.group,
            "status": self.status,
            "message": self.message,
            "details": _jsonable(dict(self.details)),
        }


@dataclass
class PreflightReport:
    """All check items of one preflight."""

    config_path: Path
    campaign_id: str | None = None
    full_hash: bool = False
    items: list[CheckItem] = field(default_factory=list)

    def add(
        self, check_id: str, status: str, message: str, details: Mapping[str, Any] | None = None
    ) -> CheckItem:
        """Append a check item and return it."""
        item = CheckItem(check_id, status, message, dict(details or {}))
        self.items.append(item)
        return item

    def counts(self) -> dict[str, int]:
        """Number of items per status (every status present)."""
        counts = dict.fromkeys(STATUSES, 0)
        for item in self.items:
            counts[item.status] += 1
        return counts

    @property
    def ready(self) -> bool:
        """Whether no check failed (warnings allowed)."""
        return not any(item.status == "error" for item in self.items)

    @property
    def exit_code(self) -> int:
        """``0`` when ready, ``1`` when at least one check is an error."""
        return 0 if self.ready else 1

    def to_record(self) -> dict[str, Any]:
        """``lidarbench.preflight/1`` record."""
        return {
            "schema": PREFLIGHT_SCHEMA,
            "config": os.fspath(self.config_path),
            "campaign_id": self.campaign_id,
            "full_hash": self.full_hash,
            "ready": self.ready,
            "exit_code": self.exit_code,
            "counts": self.counts(),
            "checks": [item.to_record() for item in self.items],
        }


def _jsonable(value: Any) -> Any:
    if isinstance(value, Mapping):
        return {str(k): _jsonable(v) for k, v in value.items()}
    if isinstance(value, list | tuple | set | frozenset):
        return [_jsonable(v) for v in value]
    if isinstance(value, os.PathLike):
        return os.fspath(value)
    if value is None or isinstance(value, str | int | float | bool):
        return value
    return str(value)


def _bytes(n: int | float) -> str:
    size = float(n)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if size < 1024 or unit == "TiB":
            return f"{size:.0f} {unit}" if unit == "B" else f"{size:.1f} {unit}"
        size /= 1024
    return f"{size:.1f} TiB"  # pragma: no cover


def _is_copc(path: Path) -> bool:
    try:
        return read_las_header(path).copc
    except LasHeaderError:
        return False  # unreadable headers are reported by the header check


def _existing_ancestor(path: Path) -> Path | None:
    for candidate in (path, *path.parents):
        if candidate.exists():
            return candidate
    return None


def _writable_dir(path: Path) -> bool:
    return os.access(path, os.W_OK | os.X_OK)


def available_cpus() -> int:
    """Logical CPUs this process may run on (affinity mask on Linux)."""
    if sys.platform == "linux":
        try:
            return len(os.sched_getaffinity(0))
        except OSError:
            pass
    return os.cpu_count() or 1


def affinity_supported() -> bool:
    """Whether per-process CPU pinning is available on this platform."""
    if sys.platform == "darwin":
        return False
    return hasattr(psutil.Process, "cpu_affinity")


def _load_average() -> float | None:
    if sys.platform != "win32":
        try:
            return os.getloadavg()[0]
        except OSError:
            return None
    return None


def _which(env: str, name: str, which: Callable[[str], str | None]) -> str | None:
    candidate = os.environ.get(env) or name
    found = which(candidate)
    if found is None and Path(candidate).is_file():
        found = str(Path(candidate).resolve())
    return found


class _Preflight:
    """State shared by the individual checks of one preflight."""

    def __init__(
        self,
        config: CampaignConfig,
        report: PreflightReport,
        *,
        registry: AdapterRegistry,
        platform_info: PlatformInfo,
        state_dir: Path,
        git_info: GitInfo,
        cpu_sample_s: float,
        which: Callable[[str], str | None],
    ) -> None:
        self.config = config
        self.report = report
        self.registry = registry
        self.platform_info = platform_info
        self.state_dir = state_dir
        self.git_info = git_info
        self.cpu_sample_s = cpu_sample_s
        self.which = which
        self.store = CampaignStore(campaign_dir(config.workspace, config.campaign_id))
        self.campaign_loaded = False
        self.git: dict[str, Any] = {}
        self.inputs: dict[str, list[Path]] = {}
        self.input_bytes: dict[str, int] = {}
        self.dataset_records: dict[str, dict[str, Any]] = {}
        self.tools: dict[str, ResolvedTool] = {}
        self.tool_errors: dict[str, str] = {}

    def add(
        self, check_id: str, status: str, message: str, details: Mapping[str, Any] | None = None
    ) -> None:
        self.report.add(check_id, status, message, details)

    def run(self, full_hash: bool) -> None:
        self.check_code()
        self.check_workspace()
        self.check_locks()
        self.check_datasets(full_hash)
        self.check_scratch()
        self.check_tools()
        self.check_plans()
        self.check_campaign(full_hash)
        self.check_cpu()
        self.check_monitoring()
        self.check_validation()
        self.check_reporting()

    # -- code and workspace ---------------------------------------------------------

    def check_code(self) -> None:
        try:
            self.git = dict(self.git_info())
        except Exception as exc:
            self.git = {"available": False, "reason": f"{type(exc).__name__}: {exc}"}
        if not self.git.get("available"):
            self.add(
                "code.git",
                "ok",
                f"no git provenance ({self.git.get('reason', 'not a git checkout')})",
                {"package_root": package_root()},
            )
            return
        details = {k: self.git.get(k) for k in ("toplevel", "revision", "branch", "dirty")}
        if self.git.get("dirty") and not self.store.exists:
            paths = self.git.get("status_paths") or []
            self.add(
                "code.git",
                "warning",
                f"lidarbench checkout is dirty ({len(paths)} changed path(s)); creating the "
                "campaign needs `run --allow-dirty` (recorded in provenance)",
                {**details, "status_paths": paths[:20]},
            )
            return
        state = "dirty" if self.git.get("dirty") else "clean"
        self.add(
            "code.git", "ok", f"{state} checkout at {str(self.git.get('revision'))[:12]}", details
        )

    def protected_paths(self) -> list[Path]:
        """The runner's protected paths (datasets, workspace, package, git checkout)."""
        cfg = self.config
        paths = [d.path for d in cfg.datasets] + [cfg.workspace, package_root()]
        if self.git.get("available") and self.git.get("toplevel"):
            paths.append(Path(self.git["toplevel"]))
        return paths

    def check_workspace(self) -> None:
        cfg = self.config
        workspace = cfg.workspace
        details: dict[str, Any] = {"path": workspace}
        overlaps = [d.id for d in cfg.datasets if paths_overlap(workspace, d.path)]
        if overlaps:
            self.add(
                "workspace",
                "error",
                f"workspace {workspace} overlaps dataset(s) {overlaps}; use a separate directory",
                details,
            )
            return
        if workspace.exists():
            if not workspace.is_dir():
                self.add("workspace", "error", f"workspace is not a directory: {workspace}")
                return
            if not _writable_dir(workspace):
                self.add("workspace", "error", f"workspace is not writable: {workspace}", details)
                return
            self.add("workspace", "ok", f"{workspace} exists and is writable", details)
            return
        ancestor = _existing_ancestor(workspace)
        details["existing_ancestor"] = ancestor
        if ancestor is None or not ancestor.is_dir() or not _writable_dir(ancestor):
            self.add(
                "workspace",
                "error",
                f"workspace {workspace} cannot be created ({ancestor} is not a writable directory)",
                details,
            )
            return
        self.add("workspace", "ok", f"{workspace} will be created by `run`", details)

    # -- locks ----------------------------------------------------------------------

    def _lock_item(self, check_id: str, label: str, report: LockReport) -> None:
        details = report.to_record()
        if not report.exists:
            self.add(check_id, "ok", f"{label} lock is free ({report.path})", details)
            return
        owner = report.owner or {}
        who = f"pid {owner.get('pid')} on {owner.get('host')} ({owner.get('purpose', '?')})"
        if report.state == "active":
            self.add(
                check_id,
                "error",
                f"{label} lock is held by an active run: {who}; wait for it to finish",
                details,
            )
        elif report.state == "stale":
            self.add(
                check_id,
                "warning",
                f"{label} lock is stale (owner {who} is gone); `run` will refuse until it is "
                f"released: lidarbench lock release --confirm {report.path}",
                details,
            )
        else:
            self.add(
                check_id,
                "warning",
                f"{label} lock cannot be verified from this host ({who}); release it only if "
                f"you are sure: lidarbench lock release --confirm --force {report.path}",
                details,
            )

    def check_locks(self) -> None:
        self._lock_item(
            "lock.workspace", "workspace", inspect_lock(self.config.workspace / WORKSPACE_LOCK_NAME)
        )
        self._lock_item("lock.machine", "machine", inspect_lock(self.state_dir / MACHINE_LOCK_NAME))

    # -- datasets -------------------------------------------------------------------

    def check_datasets(self, full_hash: bool) -> None:
        lasr = any(t.adapter == LASR_ADAPTER for t in self.config.tools)
        for dataset in self.config.datasets:
            prefix = f"dataset.{dataset.id}"
            details: dict[str, Any] = {"path": dataset.path}
            if not dataset.path.is_dir():
                self.add(prefix, "error", f"directory not found: {dataset.path}", details)
                continue
            try:
                files = dataset.input_files()
            except ConfigError as exc:
                self.add(prefix, "error", str(exc), details)
                continue
            total = sum(f.stat().st_size for f in files)
            self.inputs[dataset.id] = files
            self.input_bytes[dataset.id] = total
            details.update(input_count=len(files), total_bytes=total)
            if dataset.manifest is not None:
                details["manifest"] = dataset.manifest.dataset_id
            self.add(
                prefix,
                "ok",
                f"{len(files)} input file(s), {_bytes(total)} in {dataset.path}",
                details,
            )
            self.check_headers(prefix, files)
            self.check_protection(prefix, dataset, files)
            if lasr:
                self.check_lax(prefix, files)
            if dataset.manifest is not None and not full_hash:
                self.check_manifest(prefix, dataset)
            if full_hash:
                self.check_hash(prefix, dataset)

    def check_headers(self, prefix: str, files: list[Path]) -> None:
        clouds = [f for f in files if f.suffix.lower() in POINT_CLOUD_SUFFIXES]
        if not clouds:
            self.add(f"{prefix}.headers", "skipped", "no LAS/LAZ inputs")
            return
        problems: list[str] = []
        points = 0
        for path in clouds:
            try:
                header = read_las_header(path)
            except (LasHeaderError, OSError) as exc:
                problems.append(f"{path.name}: {exc}")
                continue
            if header.point_count <= 0:
                problems.append(f"{path.name}: header declares no points")
            points += header.point_count
        details = {"files": len(clouds), "points": points, "problems": problems}
        if problems:
            self.add(
                f"{prefix}.headers",
                "error",
                f"{len(problems)} of {len(clouds)} LAS/LAZ header(s) unusable: {problems[0]}",
                details,
            )
            return
        self.add(
            f"{prefix}.headers",
            "ok",
            f"{len(clouds)} LAS/LAZ header(s) readable, {points} points",
            details,
        )

    def check_protection(self, prefix: str, dataset: DatasetConfig, files: list[Path]) -> None:
        writable = [f.name for f in files if os.access(f, os.W_OK)]
        directory = os.access(dataset.path, os.W_OK)
        details = {"writable_files": writable[:20], "directory_writable": directory}
        if writable or directory:
            what = []
            if writable:
                what.append(f"{len(writable)} input file(s)")
            if directory:
                what.append("the dataset directory")
            self.add(
                f"{prefix}.protection",
                "warning",
                f"{' and '.join(what)} writable by this user; benchmark a read-only copy "
                "so no tool can modify its inputs",
                details,
            )
            return
        self.add(f"{prefix}.protection", "ok", "inputs and directory are read-only", details)

    def check_lax(self, prefix: str, files: list[Path]) -> None:
        clouds = [f for f in files if f.suffix.lower() in POINT_CLOUD_SUFFIXES]
        missing = []
        copc = 0
        for path in clouds:
            if _is_copc(path):
                copc += 1  # COPC carries its own spatial index; lasR writes no .lax for it
                continue
            index = path.with_suffix(".lax")
            if not index.is_file() or index.stat().st_size <= 0:
                missing.append(path.name)
        details = {"checked": len(clouds), "missing": missing, "copc": copc}
        if missing:
            self.add(
                f"{prefix}.lax",
                "error",
                f"lasR needs a nonempty .lax index next to every input; missing for "
                f"{len(missing)} file(s) ({', '.join(missing[:5])}); lasR would otherwise "
                "write them into the input tree (index a copy with lasindex, then register it)",
                details,
            )
            return
        indexed = f"{len(clouds) - copc} .lax" + (f" + {copc} COPC" if copc else "")
        self.add(
            f"{prefix}.lax", "ok", f"spatial index present for every input ({indexed})", details
        )

    def check_manifest(self, prefix: str, dataset: DatasetConfig) -> None:
        assert dataset.manifest is not None
        result = verify_dataset(dataset.manifest, DataRoots(), mode="quick", directory=dataset.path)
        details = result.to_record()
        if result.ok:
            self.add(
                f"{prefix}.manifest",
                "ok",
                f"{result.files_checked} file(s) match manifest {dataset.manifest.dataset_id} "
                "(sizes and headers; content not hashed, use --hash)",
                details,
            )
            return
        self.add(
            f"{prefix}.manifest",
            "error",
            f"does not match manifest {dataset.manifest.dataset_id}: "
            + "; ".join(result.problems[:5]),
            details,
        )

    def check_hash(self, prefix: str, dataset: DatasetConfig) -> None:
        try:
            identity = hash_dataset(dataset)
        except (IdentityError, ConfigError, OSError) as exc:
            self.add(f"{prefix}.hash", "error", str(exc))
            return
        self.dataset_records[dataset.id] = identity.record
        record = identity.record
        what = (
            f"matches manifest {dataset.manifest.dataset_id}"
            if dataset.manifest is not None
            else "hashed"
        )
        self.add(
            f"{prefix}.hash",
            "ok",
            f"SHA-256 of {record['file_count']} file(s) ({_bytes(record['total_bytes'])}) {what}",
            {"file_count": record["file_count"], "total_bytes": record["total_bytes"]},
        )

    # -- scratch --------------------------------------------------------------------

    def check_scratch(self) -> None:
        protected = self.protected_paths()
        for label, root in sorted(self.config.scratch.roots.items()):
            check_id = f"scratch.{label}"
            users = [d.id for d in self.config.datasets if d.scratch == label]
            need = max(MIN_FREE_BYTES, 2 * sum(self.input_bytes.get(d, 0) for d in users))
            details: dict[str, Any] = {"path": root, "datasets": users, "required_free": need}
            if not root.exists():
                ancestor = _existing_ancestor(root)
                creatable = ancestor is not None and ancestor.is_dir() and _writable_dir(ancestor)
                hint = f"create it first: mkdir {root}" if creatable else "and cannot be created"
                self.add(
                    check_id,
                    "error",
                    f"scratch root {root} does not exist (`run` needs an existing directory); "
                    + hint,
                    {**details, "creatable": creatable},
                )
                continue
            try:
                real = validate_scratch_root(root, protected)
            except ScratchError as exc:
                self.add(check_id, "error", f"scratch root {label!r}: {exc}", details)
                continue
            details["resolved"] = real
            free = shutil.disk_usage(real).free
            details["free_bytes"] = free
            if free < need:
                self.add(
                    check_id,
                    "warning",
                    f"{real}: only {_bytes(free)} free (want at least {_bytes(need)}: "
                    "1 GiB or twice the inputs)",
                    details,
                )
                continue
            self.add(check_id, "ok", f"{real}: valid, {_bytes(free)} free", details)

    # -- tools and plans ------------------------------------------------------------

    def check_tools(self) -> None:
        for tool in self.config.tools:
            check_id = f"tool.{tool.id}"
            try:
                resolved = resolve_tool(tool, self.registry, self.platform_info)
            except ConfigError as exc:
                self.tool_errors[tool.id] = str(exc)
                self.add(check_id, "error", str(exc), {"adapter": tool.adapter})
                continue
            self.tools[tool.id] = resolved
            record = resolved.record
            details = {
                "adapter": tool.adapter,
                "available": record["available"],
                "version": record["version"],
                "reason": record["unavailable_reason"],
                "detail": record["unavailable_detail"],
                "executables": {
                    role: entry.get("path") for role, entry in record["executables"].items()
                },
            }
            if resolved.availability.available:
                self.add(
                    check_id,
                    "ok",
                    f"{tool.adapter} {record['version'] or '(version unknown)'} available",
                    details,
                )
            else:
                self.add(
                    check_id,
                    "warning",
                    f"{tool.adapter} unavailable ({record['unavailable_reason']}: "
                    f"{record['unavailable_detail']}); its entries will be recorded as skipped",
                    details,
                )

    def check_plans(self) -> None:
        cfg = self.config
        for tool in cfg.tools:
            resolved = self.tools.get(tool.id)
            for workload in cfg.workloads:
                check_id = f"plan.{tool.id}.{workload.name}"
                if resolved is None:
                    self.add(check_id, "skipped", "tool could not be resolved")
                    continue
                rows: list[dict[str, Any]] = []
                for dataset_id in cfg.workload_datasets(workload):
                    inputs = self.inputs.get(dataset_id)
                    for cpus in cfg.cpu_counts:
                        row: dict[str, Any] = {"dataset": dataset_id, "cpus": cpus}
                        if inputs is None:
                            row.update(status="not_planned", reason="dataset is not usable")
                        else:
                            row.update(self._plan(resolved, workload, dataset_id, inputs, cpus))
                        rows.append(row)
                self.add_plan_item(check_id, rows)

    def _plan(
        self,
        tool: ResolvedTool,
        workload: WorkloadConfig,
        dataset_id: str,
        inputs: list[Path],
        cpus: int,
    ) -> dict[str, Any]:
        dataset = self.config.dataset(dataset_id)
        fake = self.config.scratch.roots[dataset.scratch] / f"{SCRATCH_PREFIX}PREFLIGHT"
        inv = Invocation(
            workload.id,
            tuple(inputs),
            fake / "out",
            fake / "work",
            threads=cpus,
            parameters=dict(workload.parameters),
        )
        try:
            plan = plan_invocation(tool.adapter, inv, availability=tool.availability)
        except AdapterContractError as exc:
            return {"status": "adapter_error", "reason": str(exc)}
        record: dict[str, Any] = {"status": str(plan.status), "reason": plan.reason}
        if plan.command is not None:
            record["argv"] = list(plan.command.argv)
        record["expected_outputs"] = len(plan.expected_outputs)
        return record

    def add_plan_item(self, check_id: str, rows: list[dict[str, Any]]) -> None:
        counts: dict[str, int] = {}
        for row in rows:
            counts[row["status"]] = counts.get(row["status"], 0) + 1
        summary = ", ".join(f"{n} {s}" for s, n in sorted(counts.items()))
        reasons = sorted({r["reason"] for r in rows if r.get("reason")})
        message = summary + (f" ({reasons[0]})" if reasons else "")
        details = {"counts": counts, "plans": rows}
        statuses = set(counts)
        if statuses & {"adapter_error", "not_planned"}:
            status = "error"
        elif "skipped_unavailable" in statuses:
            status = "warning"
        elif statuses == {"ready"}:
            status = "ok"
        elif "ready" in statuses:
            status = "warning"
        else:
            status = "skipped"
        self.add(check_id, status, message, details)

    # -- existing campaign ----------------------------------------------------------

    def check_campaign(self, full_hash: bool) -> None:
        path = self.store.path
        details: dict[str, Any] = {"path": path}
        if not path.exists():
            self.add("campaign", "ok", f"new campaign; `run` will create {path}", details)
            return
        try:
            self.store.load()
        except (CampaignExistsError, CampaignIntegrityError, OSError, ValueError) as exc:
            self.add("campaign", "error", f"existing campaign is not usable: {exc}", details)
            return
        self.campaign_loaded = True
        details["identity_sha256"] = self.store.identity_sha256
        try:
            attempts = self.store.attempts()
        except (CampaignIntegrityError, OSError, ValueError) as exc:
            self.add("campaign", "error", f"attempt records failed verification: {exc}", details)
            return
        states: dict[str, int] = {}
        for info in attempts:
            states[info.state] = states.get(info.state, 0) + 1
        eligible = {a.entry_key for a in attempts if a.eligible}
        measurements = [e for e in self.store.schedule if e.phase == "measurement"]
        done = sum(1 for e in measurements if e.entry_key in eligible)
        details.update(
            attempts=states,
            measurement_entries=len(measurements),
            measurement_entries_complete=done,
        )
        message = (
            f"existing campaign {path}: {len(attempts)} attempt record(s) "
            f"({', '.join(f'{k}={v}' for k, v in sorted(states.items())) or 'none'}); "
            f"{done}/{len(measurements)} measurement entries complete; `run` resumes it"
        )
        if states.get("incomplete"):
            self.add(
                "campaign",
                "warning",
                message + f"; {states['incomplete']} incomplete attempt(s) will be tombstoned",
                details,
            )
        else:
            self.add("campaign", "ok", message, details)
        self.compare_identity(full_hash)

    def compare_identity(self, full_hash: bool) -> None:
        if self.tool_errors or len(self.tools) != len(self.config.tools):
            self.add("campaign.identity", "skipped", "not compared: some tools did not resolve")
            return
        cfg = self.config
        schedule = campaign_schedule(cfg)
        current: dict[str, Any] = {
            "code": code_component(code_files()),
            "config": cfg.to_record(),
            "workloads": workloads_component(cfg, self.tools),
            "tools": {"tools": [self.tools[t.id].record for t in cfg.tools]},
            "environment": environment_component(self.platform_info),
            "schedule": schedule_record(schedule, cfg.scheduling),
        }
        hashed = full_hash and all(d.id in self.dataset_records for d in cfg.datasets)
        if hashed:
            current["datasets"] = {"datasets": [self.dataset_records[d.id] for d in cfg.datasets]}
        frozen = self.store.component_digests()
        compared = [n for n in IDENTITY_COMPONENTS if n in current]
        changed = [n for n in compared if digest(current[n]) != frozen[n]]
        differences = {
            n: diff_records(self.store.components.get(n), current[n], n, limit=10) for n in changed
        }
        details = {
            "compared": compared,
            "not_compared": [n for n in IDENTITY_COMPONENTS if n not in current],
            "changed": changed,
            "differences": differences,
        }
        if changed:
            first = next(iter(differences.values()), [])
            self.add(
                "campaign.identity",
                "error",
                f"differs from the frozen campaign in {', '.join(changed)}; `run` would refuse "
                "to resume (use a new campaign_id)" + (f": {first[0]}" if first else ""),
                details,
            )
            return
        note = "" if hashed else " (datasets not hashed; use --hash)"
        self.add(
            "campaign.identity",
            "ok",
            f"matches the frozen identity in {', '.join(compared)}{note}",
            details,
        )

    # -- machine resources ----------------------------------------------------------

    def check_cpu(self) -> None:
        cfg = self.config
        logical = psutil.cpu_count(logical=True) or self.platform_info.cpu_count
        available = available_cpus()
        details: dict[str, Any] = {
            "logical_cpus": logical,
            "available_cpus": available,
            "cpu_counts": list(cfg.cpu_counts),
        }
        too_many = [c for c in cfg.cpu_counts if c > available]
        if too_many:
            self.add(
                "cpu.counts",
                "error",
                f"cpu_counts {too_many} exceed the {available} CPU(s) available to this process",
                details,
            )
        else:
            self.add(
                "cpu.counts",
                "ok",
                f"cpu_counts {list(cfg.cpu_counts)} fit {available} available CPU(s) "
                f"({logical} logical)",
                details,
            )

        mode = cfg.scheduling.cpu_affinity
        affinity = {"cpu_affinity": mode, "platform": sys.platform}
        if mode == "none":
            self.add("cpu.affinity", "skipped", "cpu_affinity = none: no pinning", affinity)
        elif sys.platform == "darwin":
            self.add(
                "cpu.affinity",
                "warning",
                "cpu_affinity = pinned, but affinity is not enforced on macOS",
                affinity,
            )
        elif affinity_supported():
            self.add("cpu.affinity", "ok", "cpu_affinity = pinned is supported here", affinity)
        else:
            self.add(
                "cpu.affinity",
                "warning",
                f"cpu_affinity = pinned, but CPU affinity is not supported on {sys.platform}",
                affinity,
            )

        load = _load_average()
        percent = psutil.cpu_percent(interval=self.cpu_sample_s) if self.cpu_sample_s > 0 else None
        busy = []
        if load is not None and load > LOAD_BUSY_FRACTION * logical:
            busy.append(f"1-min load average {load:.2f} > {LOAD_BUSY_FRACTION:g} x {logical}")
        if percent is not None and percent > CPU_BUSY_PERCENT:
            busy.append(f"CPU use {percent:.0f}% > {CPU_BUSY_PERCENT:.0f}%")
        load_details = {"load_1min": load, "cpu_percent": percent, "sample_s": self.cpu_sample_s}
        shown = ", ".join(
            part
            for part in (
                f"load {load:.2f}" if load is not None else "",
                f"CPU {percent:.0f}%" if percent is not None else "",
            )
            if part
        )
        if busy:
            self.add(
                "cpu.load",
                "warning",
                "machine looks busy (" + "; ".join(busy) + "); timings would be disturbed",
                load_details,
            )
        else:
            self.add(
                "cpu.load", "ok", f"machine idle enough ({shown or 'not sampled'})", load_details
            )

        memory = psutil.virtual_memory()
        mem_details = {"total_bytes": memory.total, "available_bytes": memory.available}
        message = f"{_bytes(memory.available)} available of {_bytes(memory.total)}"
        if memory.available < LOW_MEMORY_BYTES:
            self.add("memory", "warning", f"low memory: {message}", mem_details)
        else:
            self.add("memory", "ok", message, mem_details)

    def check_monitoring(self) -> None:
        from lidarbench.campaign.runner import RunError, load_monitor_backend

        mon = self.config.monitoring
        if not mon.enabled:
            self.add("monitoring", "skipped", "resource monitoring is disabled")
            return
        try:
            backend = load_monitor_backend(self.config)
        except RunError as exc:
            self.add("monitoring", "error", str(exc), mon.to_record())
            return
        assert backend is not None
        ok, reason = backend.probe()
        if not ok:
            self.add(
                "monitoring",
                "warning",
                f"{mon.backend} unusable ({reason}); attempts run unmonitored",
                mon.to_record(),
            )
            return
        self.add("monitoring", "ok", f"{mon.backend} every {mon.interval_s:g} s", mon.to_record())

    # -- validation and reporting ---------------------------------------------------

    def check_validation(self) -> None:
        from lidarbench.workloads.contract import ContractError, load_suite

        cfg = self.config
        numerical = cfg.validation_mode == "numerical"
        suite_dir = cfg.validation.suite_dir()
        settings = {**cfg.validation.to_record(), "validation_mode": cfg.validation_mode}
        try:
            suite = load_suite(suite_dir)
        except (ContractError, OSError) as exc:
            self.add(
                "validation.suite",
                "error" if numerical else "warning",
                f"workload contract suite unusable: {exc}",
                settings,
            )
            return
        self.add(
            "validation.suite",
            "ok",
            f"suite {suite.name} ({len(suite.contracts)} contract(s)), "
            f"validation_mode = {cfg.validation_mode}, "
            f"comparability = {cfg.validation.comparability}",
            settings,
        )
        with_contract = []
        for workload in cfg.workloads:
            check_id = f"validation.{workload.name}"
            contract = suite.contracts.get(workload.id)
            if contract is None:
                self.add(
                    check_id,
                    "skipped",
                    f"no contract for {workload.id} in {suite.name}: outputs are not validated",
                )
                continue
            with_contract.append(workload.name)
            tools: dict[str, Any] = {}
            for tool in cfg.tools:
                resolved = self.tools.get(tool.id)
                adapter = resolved.adapter if resolved is not None else None
                contract_tool = str(getattr(adapter, "contract_tool", None) or tool.adapter)
                impl = contract.implementation(contract_tool)
                tools[tool.id] = {
                    "contract_tool": contract_tool,
                    "implementation": str(impl.status) if impl is not None else None,
                    "equivalence": impl.equivalence if impl is not None else None,
                    "comparability": contract.declared_comparability(contract_tool),
                }
            summary = ", ".join(f"{t}: {v['comparability']}" for t, v in tools.items())
            self.add(
                check_id,
                "ok",
                f"contract {contract.key} ({summary})",
                {"contract": contract.key, "digest": contract.digest, "tools": tools},
            )
        if not numerical:
            self.add(
                "validation.backends",
                "skipped",
                "validation_mode = performance-only: numerical validation is not run",
            )
            return
        if not with_contract:
            self.add("validation.backends", "skipped", "no workload has a contract")
            return
        from lidarbench.validation.io import probe_point_backend, probe_raster_backend

        statuses = [probe_raster_backend(), probe_point_backend()]
        details = {s.name: s.to_record() for s in statuses}
        missing = [s for s in statuses if not s.available]
        if missing:
            self.add(
                "validation.backends",
                "error",
                f"numerical validation of {with_contract} would be unavailable: "
                + "; ".join(s.detail for s in missing),
                details,
            )
            return
        versions = ", ".join(f"{k} {v}" for s in statuses for k, v in sorted(s.versions.items()))
        self.add("validation.backends", "ok", f"raster and point backends ({versions})", details)

    def check_reporting(self) -> None:
        for check_id, env, name, purpose in (
            ("reporting.quarto", QUARTO_ENV, "quarto", "the Quarto report site"),
            ("reporting.rscript", RSCRIPT_ENV, "Rscript", "the knitr engine of the report site"),
        ):
            found = _which(env, name, self.which)
            if found is None:
                self.add(
                    check_id,
                    "warning",
                    f"{name} not found (needed only to render {purpose}; set ${env} or PATH)",
                )
            else:
                self.add(check_id, "ok", f"{name} at {found}", {"path": found})


def run_preflight(
    config_path: str | os.PathLike[str],
    *,
    full_hash: bool = False,
    registry: AdapterRegistry | None = None,
    platform_info: PlatformInfo | None = None,
    state_dir: Path | None = None,
    git_info: GitInfo = git_provenance,
    cpu_sample_s: float = 0.5,
    which: Callable[[str], str | None] = shutil.which,
) -> PreflightReport:
    """Check whether a campaign configuration is ready to run, changing nothing.

    Args:
        config_path: Campaign configuration (TOML).
        full_hash: Hash every input file (SHA-256) and compare with the manifest
            and, for an existing campaign, with the frozen dataset identity.
        registry: Adapter registry (default: installed entry points).
        platform_info: Platform facts (default: detected).
        state_dir: Directory of the per-user machine lock (default:
            :func:`~lidarbench.core.locks.default_state_dir`).
        git_info: Provider of git provenance (testing hook).
        cpu_sample_s: Duration of the CPU-use sample; ``0`` skips it.
        which: Executable lookup used for the reporting tools (testing hook).

    Returns:
        The report; a configuration that does not parse yields a single
        ``config`` error item.
    """
    report = PreflightReport(Path(config_path), full_hash=full_hash)
    try:
        config = load_config(config_path)
    except ConfigError as exc:
        report.add("config", "error", str(exc))
        return report
    report.campaign_id = config.campaign_id
    report.config_path = config.source_path or report.config_path
    report.add(
        "config",
        "ok",
        f"campaign {config.campaign_id}: {len(config.datasets)} dataset(s), "
        f"{len(config.workloads)} workload(s), {len(config.tools)} tool(s), "
        f"cpu_counts {list(config.cpu_counts)}",
        {"source_sha256": config.source_sha256, "workspace": config.workspace},
    )
    checks = _Preflight(
        config,
        report,
        registry=registry if registry is not None else AdapterRegistry.discover(),
        platform_info=platform_info or get_platform_info(),
        state_dir=Path(state_dir) if state_dir else default_state_dir(),
        git_info=git_info,
        cpu_sample_s=cpu_sample_s,
        which=which,
    )
    checks.run(full_hash)
    return report


_MARKERS = {"ok": "ok", "warning": "WARN", "error": "ERROR", "skipped": "skip"}


def render_report(report: PreflightReport) -> str:
    """Human-readable summary grouped by check group, one line per check."""
    head = f"Preflight of {report.config_path}"
    if report.campaign_id:
        head += f" (campaign {report.campaign_id})"
    lines = [head + (" [--hash]" if report.full_hash else "")]
    width = max((len(item.id) for item in report.items), default=0)
    groups: dict[str, list[CheckItem]] = {}
    for item in report.items:
        groups.setdefault(item.group, []).append(item)
    for group, items in groups.items():
        lines.append(f"{group}:")
        lines += [
            f"  {_MARKERS[item.status]:<5} {item.id:<{width}}  {item.message}" for item in items
        ]
    counts = report.counts()
    verdict = "ready" if report.ready else "NOT ready"
    lines.append(
        f"Result: {verdict} ({counts['error']} error(s), {counts['warning']} warning(s), "
        f"{counts['ok']} ok, {counts['skipped']} skipped)"
    )
    return "\n".join(lines) + "\n"
