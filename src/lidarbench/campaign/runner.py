"""Campaign runner: create or resume a campaign and execute its frozen schedule.

One ``run`` invocation:

1. validates paths (scratch roots, workspace/dataset overlap) and takes the
   workspace lock and the per-user machine lock;
2. resolves tools, builds the schedule and computes the full identity once
   (every input file is hashed here, never inside a timed attempt);
3. creates the campaign (refusing a dirty checkout unless allowed) or compares
   the identity with the frozen one, refusing any difference;
4. tombstones attempts left incomplete by an earlier run;
5. decides, per frozen schedule row, whether to run, retry or skip it;
6. executes the selected rows strictly sequentially. Before each row the frozen
   records, code files, tool executables and dataset files are re-checked
   cheaply (digests of small files and stat fingerprints); after each row the
   inputs are re-checked and the campaign stops if they changed;
7. optionally rehashes all inputs in full at the end.

Resume rules (per measurement row): complete if any published attempt is
*eligible* (``succeeded`` with all required outputs, or a recorded skip);
failed or timed-out rows (a launched process that did not succeed) are retried
only with ``retry_failed``; rows with only interrupted, abandoned or
pre-launch infrastructure failures (scratch, inputs, monitor start, planning)
are retried automatically. Warm-up rows
never count as complete: they rerun in every invocation for each unit that
still has measurement rows to run, and are skipped for finished units.

Execution success never implies numerical validity: every result records
``structural_status`` and ``numerical_status`` separately (``not_run`` or
``unverified`` until a validator produces a verdict).
"""

from __future__ import annotations

import contextlib
import os
import socket
import sys
import threading
from collections.abc import Callable, Iterable, Iterator, Mapping
from dataclasses import dataclass, field
from enum import StrEnum
from importlib.metadata import entry_points
from pathlib import Path
from typing import Any

from lidarbench.campaign.identity import (
    IDENTITY_COMPONENTS,
    CampaignIdentity,
    Fingerprint,
    HashingCancelledError,
    ResolvedTool,
    compute_identity,
    fingerprint_problems,
    git_provenance,
    hash_dataset,
    identity_differences,
    package_root,
    provenance_record,
    resolve_tools,
    tree_problems,
)
from lidarbench.campaign.schedule import (
    ScheduleEntry,
    campaign_schedule,
    schedule_csv,
    schedule_record,
)
from lidarbench.campaign.store import (
    INVOCATION,
    RESULT_SCHEMA,
    STARTED,
    STARTED_SCHEMA,
    AttemptInfo,
    CampaignStore,
    campaign_dir,
    timestamp,
)
from lidarbench.config.campaign import (
    CACHE_POLICY_DESCRIPTIONS,
    CampaignConfig,
    ConfigError,
    WorkloadConfig,
)
from lidarbench.core.canonical import sha256_bytes, sha256_file
from lidarbench.core.locks import (
    MACHINE_LOCK_NAME,
    WORKSPACE_LOCK_NAME,
    ExclusiveLock,
    LockError,
    LockHeldError,
    default_state_dir,
    inspect_lock,
)
from lidarbench.core.platform import PlatformInfo, get_platform_info
from lidarbench.core.scratch import (
    SCRATCH_PREFIX,
    ScratchDir,
    ScratchError,
    cleanup_scratch,
    create_scratch,
    find_owned_scratch,
    paths_overlap,
    release_ownership,
    validate_scratch_root,
)
from lidarbench.core.store import iter_files, new_sortable_id, publish_json
from lidarbench.execution.adapters.base import Invocation
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.attempt import AttemptExecution, execute_plan
from lidarbench.execution.plan import AdapterContractError, InvocationPlan, plan_invocation
from lidarbench.execution.process import (
    CancellationToken,
    ExecutionInterrupted,
    ProcessResult,
    ThreadBudget,
    cancel_on_signals,
)
from lidarbench.monitoring.base import MonitorBackend

MONITOR_ENTRY_POINT_GROUP = "lidarbench.monitors"
NUMERICAL_NOTE = (
    "Execution success does not imply numerical validity. No structural or numerical "
    "validator has produced a verdict for this attempt."
)


class RunError(RuntimeError):
    """The run cannot start or must stop; nothing already published is modified."""

    exit_code = 1


class IdentityMismatchError(RunError):
    """The current identity differs from the frozen campaign identity."""

    def __init__(self, campaign_id: str, differences: Mapping[str, list[str]]) -> None:
        self.differences = dict(differences)
        lines = [
            f"campaign {campaign_id!r} was frozen with a different identity; changed "
            f"component(s): {', '.join(differences)}. Nothing was run. Use a new campaign_id "
            "for changed definitions, tools, inputs or environment."
        ]
        for name, diffs in differences.items():
            lines += [f"  {name}: {d}" for d in diffs[:10]]
        super().__init__("\n".join(lines))


class RunInterruptedError(RunError):
    """Cancellation was requested before any attempt ran."""

    exit_code = 130


class IdentityChangedError(RunError):
    """A per-entry check found that frozen material changed during the run."""

    exit_code = 4


class Action(StrEnum):
    """What a run does with one schedule row."""

    RUN = "run"
    WARMUP = "warmup"
    RETRY_INTERRUPTED = "retry-interrupted"
    RETRY_FAILED = "retry-failed"
    SKIP_COMPLETE = "skip-complete"
    SKIP_FAILED = "skip-failed"
    SKIP_WARMUP = "skip-warmup"
    SKIP_UNSELECTED = "skip-unselected"
    SKIP_LIMIT = "skip-limit"

    @property
    def dispatches(self) -> bool:
        """Whether a new attempt is started."""
        return self in (
            Action.RUN,
            Action.WARMUP,
            Action.RETRY_INTERRUPTED,
            Action.RETRY_FAILED,
        )


@dataclass(frozen=True)
class Selection:
    """Optional restriction of which frozen schedule rows this run dispatches.

    A selection never changes the campaign identity or schedule; unselected rows
    simply stay pending for a later run.
    """

    workloads: frozenset[str] = frozenset()
    datasets: frozenset[str] = frozenset()
    tools: frozenset[str] = frozenset()
    cpus: frozenset[int] = frozenset()
    phases: frozenset[str] = frozenset()
    limit: int | None = None

    def matches(self, entry: ScheduleEntry) -> bool:
        """Whether ``entry`` is selected (empty filters select everything)."""
        return (
            (not self.workloads or entry.workload in self.workloads)
            and (not self.datasets or entry.dataset in self.datasets)
            and (not self.tools or entry.tool in self.tools)
            and (not self.cpus or entry.cpus in self.cpus)
            and (not self.phases or entry.phase in self.phases)
        )

    def check(self, config: CampaignConfig) -> None:
        """Reject filters naming things that are not in the campaign."""
        known: dict[str, tuple[frozenset[Any], set[Any]]] = {
            "workload": (self.workloads, {w.name for w in config.workloads}),
            "dataset": (self.datasets, {d.id for d in config.datasets}),
            "tool": (self.tools, {t.id for t in config.tools}),
            "cpus": (self.cpus, set(config.cpu_counts)),
            "phase": (self.phases, {"warmup", "measurement"}),
        }
        for label, (chosen, available) in known.items():
            unknown = sorted(map(str, chosen - available))
            if unknown:
                raise ConfigError(
                    f"unknown {label} selection {unknown}; campaign has "
                    f"{sorted(map(str, available))}"
                )
        if self.limit is not None and self.limit < 1:
            raise ConfigError("limit must be >= 1")

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "workloads": sorted(self.workloads),
            "datasets": sorted(self.datasets),
            "tools": sorted(self.tools),
            "cpus": sorted(self.cpus),
            "phases": sorted(self.phases),
            "limit": self.limit,
        }


@dataclass(frozen=True)
class RunOptions:
    """Options of one run invocation (recorded in the event log, not in identity)."""

    selection: Selection = field(default_factory=Selection)
    retry_failed: bool = False
    allow_dirty: bool = False
    require_existing: bool = False

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "selection": self.selection.to_record(),
            "retry_failed": self.retry_failed,
            "allow_dirty": self.allow_dirty,
            "require_existing": self.require_existing,
        }


@dataclass(frozen=True)
class Decision:
    """What happens to one schedule row in this run."""

    entry: ScheduleEntry
    action: Action
    previous_attempts: tuple[str, ...] = ()
    note: str = ""

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            **self.entry.to_record(),
            "action": str(self.action),
            "previous_attempts": list(self.previous_attempts),
            "note": self.note,
        }


def decide(
    schedule: Iterable[ScheduleEntry],
    attempts: Iterable[AttemptInfo],
    selection: Selection,
    retry_failed: bool,
) -> list[Decision]:
    """Decide the action for every schedule row (see module docstring)."""
    entries = list(schedule)
    by_key: dict[str, list[AttemptInfo]] = {}
    for info in attempts:
        if info.entry_key is not None:
            by_key.setdefault(info.entry_key, []).append(info)

    decisions: dict[int, Decision] = {}
    dispatched = 0
    for entry in entries:
        if entry.phase != "measurement":
            continue
        previous = by_key.get(entry.entry_key, [])
        ids = tuple(a.attempt_id for a in previous)
        if not selection.matches(entry):
            action, note = Action.SKIP_UNSELECTED, ""
        elif any(a.eligible for a in previous):
            action, note = Action.SKIP_COMPLETE, "an eligible attempt is already published"
        elif any(a.is_verdict for a in previous):
            if retry_failed:
                action, note = Action.RETRY_FAILED, "explicit retry of a failed entry"
            else:
                action, note = Action.SKIP_FAILED, "failed earlier; rerun with --retry-failed"
        elif previous:
            action, note = Action.RETRY_INTERRUPTED, "earlier attempts were interrupted"
        else:
            action, note = Action.RUN, ""
        if action.dispatches and selection.limit is not None:
            dispatched += 1
            if dispatched > selection.limit:
                action, note = Action.SKIP_LIMIT, f"beyond --limit {selection.limit}"
        decisions[entry.schedule_index] = Decision(entry, action, ids, note)

    pending_units = {d.entry.unit_key for d in decisions.values() if d.action.dispatches}
    for entry in entries:
        if entry.phase != "warmup":
            continue
        ids = tuple(a.attempt_id for a in by_key.get(entry.entry_key, []))
        if not selection.matches(entry):
            decisions[entry.schedule_index] = Decision(entry, Action.SKIP_UNSELECTED, ids)
        elif entry.unit_key in pending_units:
            note = "warm-ups rerun in every run for units with pending measurements"
            decisions[entry.schedule_index] = Decision(entry, Action.WARMUP, ids, note)
        else:
            note = "no measurement of this unit is pending"
            decisions[entry.schedule_index] = Decision(entry, Action.SKIP_WARMUP, ids, note)

    return [decisions[e.schedule_index] for e in entries]


@dataclass
class RunSummary:
    """Outcome of one run invocation."""

    campaign_id: str
    campaign_path: Path
    run_id: str | None
    dry_run: bool
    campaign_state: str
    """``created``, ``resumed``, ``new`` / ``matches`` / ``mismatch`` (dry run)."""
    identity_sha256: str
    decisions: list[Decision] = field(default_factory=list)
    statuses: dict[str, int] = field(default_factory=dict)
    stop_reason: str | None = None
    warnings: list[str] = field(default_factory=list)
    differences: dict[str, list[str]] = field(default_factory=dict)
    previews: dict[int, dict[str, Any]] = field(default_factory=dict)

    @property
    def exit_code(self) -> int:
        """0 when the run ended normally (attempt failures are data), else non-zero."""
        if self.campaign_state == "mismatch":
            return 1
        return {None: 0, "interrupted": 130, "inputs_changed": 4}.get(self.stop_reason, 1)

    def action_counts(self) -> dict[str, int]:
        """Number of schedule rows per action."""
        counts: dict[str, int] = {}
        for d in self.decisions:
            counts[str(d.action)] = counts.get(str(d.action), 0) + 1
        return counts

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        rows = []
        for d in self.decisions:
            row = d.to_record()
            if d.entry.schedule_index in self.previews:
                row["plan"] = self.previews[d.entry.schedule_index]
            rows.append(row)
        return {
            "schema": "lidarbench.run-summary/1",
            "campaign_id": self.campaign_id,
            "campaign_path": os.fspath(self.campaign_path),
            "run_id": self.run_id,
            "dry_run": self.dry_run,
            "campaign_state": self.campaign_state,
            "identity_sha256": self.identity_sha256,
            "action_counts": self.action_counts(),
            "execution_statuses": dict(self.statuses),
            "stop_reason": self.stop_reason,
            "warnings": list(self.warnings),
            "identity_differences": self.differences,
            "entries": rows,
        }


@dataclass
class _Outcome:
    status: str
    launched: bool
    stop_reason: str | None = None


def load_monitor_backend(config: CampaignConfig) -> MonitorBackend | None:
    """Instantiate the configured monitor backend (``None`` when disabled).

    Raises:
        RunError: The backend is unknown, or required but unusable here.
    """
    mon = config.monitoring
    if not mon.enabled:
        return None
    matches = [ep for ep in entry_points(group=MONITOR_ENTRY_POINT_GROUP) if ep.name == mon.backend]
    if not matches:
        raise RunError(f"monitor backend {mon.backend!r} is not installed")
    try:
        backend: MonitorBackend = matches[0].load()(interval_s=mon.interval_s)
    except Exception as exc:
        raise RunError(f"monitor backend {mon.backend!r} failed to load: {exc}") from exc
    ok, reason = backend.probe()
    if not ok and mon.required:
        raise RunError(f"monitoring is required but {mon.backend!r} is unusable: {reason}")
    return backend


class CampaignRunner:
    """Run (or dry-run) one campaign configuration."""

    def __init__(
        self,
        config: CampaignConfig,
        *,
        options: RunOptions | None = None,
        registry: AdapterRegistry | None = None,
        platform_info: PlatformInfo | None = None,
        cancel: CancellationToken | None = None,
        state_dir: Path | None = None,
        monitor_backend: MonitorBackend | str | None = "config",
        git_info: Callable[[], dict[str, Any]] = git_provenance,
        report: Callable[[str], None] | None = None,
        handle_signals: bool = True,
    ) -> None:
        """Configure a run.

        Args:
            config: Validated campaign configuration.
            options: Selection, retry and safety options.
            registry: Adapter registry (default: installed entry points).
            platform_info: Platform facts (default: detected).
            cancel: Cancellation token; Ctrl+C/SIGTERM cancel it while running.
            state_dir: Directory of the per-user machine lock.
            monitor_backend: ``"config"`` to load the configured backend, or an
                explicit backend (``None`` disables monitoring).
            git_info: Provider of git provenance (testing hook).
            report: Progress callback (default: stderr).
            handle_signals: Convert termination signals into cancellation.
        """
        self.config = config
        self.options = options or RunOptions()
        self.registry = registry if registry is not None else AdapterRegistry.discover()
        self.platform_info = platform_info or get_platform_info()
        self.cancel = cancel or CancellationToken()
        self.state_dir = Path(state_dir) if state_dir else default_state_dir()
        self._monitor_choice = monitor_backend
        self._git_info = git_info
        self._report = report or (lambda line: print(line, file=sys.stderr, flush=True))
        self._handle_signals = handle_signals
        self._warnings: list[str] = []
        self._protected: list[Path] | None = None
        self.store = CampaignStore(campaign_dir(config.workspace, config.campaign_id))

    # -- shared preparation -------------------------------------------------------

    def _protected_paths(self) -> list[Path]:
        if self._protected is not None:
            return self._protected
        cfg = self.config
        paths = [d.path for d in cfg.datasets] + [cfg.workspace, package_root()]
        top = self._git_toplevel()
        if top is not None:
            paths.append(top)
        self._protected = paths
        return paths

    def _git_toplevel(self) -> Path | None:
        info = self._git_info()
        return Path(info["toplevel"]) if info.get("available") and info.get("toplevel") else None

    def _preflight(self) -> None:
        cfg = self.config
        self.options.selection.check(cfg)
        for dataset in cfg.datasets:
            dataset.input_files()
            if paths_overlap(cfg.workspace, dataset.path):
                raise ConfigError(
                    f"workspace {cfg.workspace} overlaps dataset {dataset.id!r} ({dataset.path})"
                )
        protected = self._protected_paths()
        for label, root in cfg.scratch.roots.items():
            try:
                validate_scratch_root(root, protected)
            except ScratchError as exc:
                raise ConfigError(f"scratch root {label!r}: {exc}") from None
        for count in cfg.cpu_counts:
            if count > self.platform_info.cpu_count:
                self._warn(
                    f"cpu count {count} exceeds the {self.platform_info.cpu_count} logical CPUs "
                    "of this machine; such attempts oversubscribe the CPU"
                )

    def _warn(self, message: str) -> None:
        self._warnings.append(message)
        self._report(f"WARNING: {message}")

    def _identity(self) -> tuple[CampaignIdentity, tuple[ScheduleEntry, ...]]:
        tools = resolve_tools(self.config, self.registry, self.platform_info)
        for tool in tools.values():
            if not tool.availability.available:
                self._warn(
                    f"tool {tool.config.id!r} is unavailable ({tool.availability.reason}: "
                    f"{tool.availability.detail}); its entries are recorded as skipped"
                )
        schedule = campaign_schedule(self.config)
        component = schedule_record(schedule, self.config.scheduling)
        self._report(
            "Computing campaign identity (full SHA-256 of every input file; done once per run)"
        )
        identity = compute_identity(
            self.config, tools, self.platform_info, component, lambda: self.cancel.cancelled
        )
        return identity, schedule

    def _compare(self, identity: CampaignIdentity) -> dict[str, list[str]]:
        frozen = self.store.component_digests()
        changed = [n for n in IDENTITY_COMPONENTS if frozen[n] != identity.digests[n]]
        return identity_differences(self.store.components, identity.components, changed)

    # -- dry run ------------------------------------------------------------------

    def dry_run(self) -> RunSummary:
        """Show what a run would do without writing anything or taking locks.

        Refuses while the machine lock is held by an active run, because hashing
        inputs would disturb that run's measurements.
        """
        self._warnings = []
        machine = inspect_lock(self.state_dir / MACHINE_LOCK_NAME)
        if machine.exists and machine.state == "active":
            raise LockHeldError(machine.path, machine.owner, "active")
        self._preflight()
        identity, schedule = self._identity()
        summary = RunSummary(
            self.config.campaign_id,
            self.store.path,
            None,
            True,
            "new",
            identity.identity_sha256,
            warnings=self._warnings,
        )
        attempts: list[AttemptInfo] = []
        if self.store.path.exists():
            self.store.load()
            summary.differences = self._compare(identity)
            if summary.differences:
                summary.campaign_state = "mismatch"
                return summary
            summary.campaign_state = "matches"
            attempts = self.store.attempts()
            schedule = self.store.schedule
        summary.decisions = decide(
            schedule, attempts, self.options.selection, self.options.retry_failed
        )
        for decision in summary.decisions:
            if decision.action.dispatches:
                summary.previews[decision.entry.schedule_index] = self._preview(
                    decision.entry, identity
                )
        return summary

    def _preview(self, entry: ScheduleEntry, identity: CampaignIdentity) -> dict[str, Any]:
        cfg = self.config
        dataset = cfg.dataset(entry.dataset)
        workload = cfg.workload(entry.workload)
        tool = identity.tools[entry.tool]
        fake = cfg.scratch.roots[dataset.scratch] / f"{SCRATCH_PREFIX}DRY-RUN"
        inv = Invocation(
            workload.id,
            identity.datasets[dataset.id].inputs,
            fake / "out",
            fake / "work",
            threads=entry.cpus,
            parameters=dict(workload.parameters),
        )
        try:
            plan = plan_invocation(tool.adapter, inv, availability=tool.availability)
        except AdapterContractError as exc:
            return {"status": "adapter_error", "reason": str(exc)}
        record: dict[str, Any] = {"status": str(plan.status), "reason": plan.reason}
        if plan.command is not None:
            record["argv"] = list(plan.command.argv)
            record["env"] = dict(plan.command.env)
        return record

    # -- real run -----------------------------------------------------------------

    def run(self) -> RunSummary:
        """Create or resume the campaign and execute the selected schedule rows.

        Raises:
            ConfigError: Invalid configuration or selection.
            LockHeldError: Another run holds the workspace or machine lock.
            IdentityMismatchError: The campaign exists with a different identity.
            RunError: Other conditions that prevent a safe run.
        """
        self._warnings = []
        self._preflight()
        self.config.workspace.mkdir(parents=True, exist_ok=True)
        workspace_lock = ExclusiveLock(self.config.workspace / WORKSPACE_LOCK_NAME, "workspace")
        machine_lock = ExclusiveLock(self.state_dir / MACHINE_LOCK_NAME, "benchmark-execution")
        workspace_lock.acquire()
        try:
            machine_lock.acquire()
            try:
                with self._signals():
                    try:
                        return self._run_locked()
                    except HashingCancelledError:
                        raise RunInterruptedError(
                            "interrupted while hashing inputs; nothing was executed"
                        ) from None
            finally:
                self._release(machine_lock)
        finally:
            self._release(workspace_lock)

    def _release(self, lock: ExclusiveLock) -> None:
        try:
            lock.release()
        except (LockError, OSError) as exc:
            self._warn(f"could not release lock: {exc}")

    @contextlib.contextmanager
    def _signals(self) -> Iterator[None]:
        if self._handle_signals and threading.current_thread() is threading.main_thread():
            with cancel_on_signals(self.cancel):
                yield
        else:
            yield

    def _run_locked(self) -> RunSummary:
        cfg, store, options = self.config, self.store, self.options
        run_id = new_sortable_id()
        identity, schedule = self._identity()
        created = False
        if store.path.exists():
            store.load()
            differences = self._compare(identity)
            if differences:
                store.log(
                    "identity_mismatch",
                    run_id=run_id,
                    current_identity_sha256=identity.identity_sha256,
                    components=sorted(differences),
                    differences=differences,
                )
                raise IdentityMismatchError(cfg.campaign_id, differences)
        else:
            if options.require_existing:
                raise RunError(f"campaign {cfg.campaign_id!r} does not exist at {store.path}")
            git = self._git_info()
            if git.get("dirty") and not options.allow_dirty:
                raise RunError(
                    "the lidarbench source checkout has uncommitted changes "
                    f"({', '.join(git.get('status_paths', [])[:5])}); commit them or pass "
                    "--allow-dirty (recorded in provenance)"
                )
            provenance = provenance_record(self.platform_info, git)
            provenance.update(
                allow_dirty=options.allow_dirty,
                config_source={
                    "path": os.fspath(cfg.source_path) if cfg.source_path else None,
                    "sha256": cfg.source_sha256,
                },
            )
            store.create(
                cfg.campaign_id,
                identity.components,
                identity.digests,
                identity.identity_sha256,
                schedule_csv(schedule),
                provenance,
            )
            created = True
            self._report(f"Created campaign {cfg.campaign_id} at {store.path}")

        hashed_files = sum(d.record["file_count"] for d in identity.datasets.values())
        hashed_bytes = sum(d.record["total_bytes"] for d in identity.datasets.values())
        store.publish_verification(
            run_id,
            {
                "verified_at": timestamp(),
                "result": "match" if not created else "created",
                "identity_sha256": identity.identity_sha256,
                "component_digests": identity.digests,
                "input_files_hashed": hashed_files,
                "input_bytes_hashed": hashed_bytes,
                "checks_before_each_entry": [
                    "SHA-256 of the frozen campaign records",
                    "SHA-256 of the lidarbench source files",
                    "stat fingerprints of the entry's tool executables",
                    "stat fingerprints of the entry's dataset files",
                ],
            },
        )
        store.log(
            "run_started",
            run_id=run_id,
            campaign_created=created,
            options=options.to_record(),
            host=socket.gethostname(),
            pid=os.getpid(),
        )
        store.log(
            "identity_verified",
            run_id=run_id,
            identity_sha256=identity.identity_sha256,
            input_files_hashed=hashed_files,
            input_bytes_hashed=hashed_bytes,
        )
        summary = RunSummary(
            cfg.campaign_id,
            store.path,
            run_id,
            False,
            "created" if created else "resumed",
            identity.identity_sha256,
            warnings=self._warnings,
        )
        try:
            self._execute(run_id, identity, summary)
        except BaseException as exc:
            store.log("run_aborted", run_id=run_id, error=f"{type(exc).__name__}: {exc}")
            raise
        return summary

    def _execute(self, run_id: str, identity: CampaignIdentity, summary: RunSummary) -> None:
        cfg, store = self.config, self.store
        for info in store.attempts():
            if info.state == "incomplete":
                store.tombstone(
                    info,
                    run_id,
                    "no completion receipt when a later run started (the earlier run "
                    "crashed, was killed or lost power); the attempt is retained unchanged",
                )
                store.log(
                    "attempt_abandoned",
                    run_id=run_id,
                    attempt_id=info.attempt_id,
                    entry_key=info.entry_key,
                )
                self._warn(f"attempt {info.attempt_id} ({info.entry_key}) was incomplete")
        for root in cfg.scratch.roots.values():
            for orphan in find_owned_scratch(root, cfg.campaign_id):
                store.log("orphan_scratch", run_id=run_id, **orphan)
                self._warn(
                    f"orphaned run-owned scratch {orphan['path']} (attempt "
                    f"{orphan['attempt_id']}); not deleted automatically"
                )

        summary.decisions = decide(
            store.schedule,
            store.attempts(),
            self.options.selection,
            self.options.retry_failed,
        )
        guard = _Guard(store, identity)
        backend = (
            load_monitor_backend(cfg)
            if isinstance(self._monitor_choice, str)
            else self._monitor_choice
        )
        todo = [d for d in summary.decisions if d.action.dispatches]
        self._report(f"{len(todo)} of {len(summary.decisions)} schedule entries to run")
        for number, decision in enumerate(todo, start=1):
            if self.cancel.cancelled:
                summary.stop_reason = "interrupted"
                break
            entry = decision.entry
            problems = guard.check(entry)
            if problems:
                store.log(
                    "identity_changed",
                    run_id=run_id,
                    entry_key=entry.entry_key,
                    problems=problems,
                )
                raise IdentityChangedError(
                    "frozen campaign material changed during the run; stopping before "
                    f"{entry.entry_key}. Use a new campaign_id.\n  " + "\n  ".join(problems)
                )
            self._report(
                f"[{number}/{len(todo)}] #{entry.schedule_index} {entry.entry_key} "
                f"({decision.action})"
            )
            outcome = self._attempt(run_id, decision, identity, guard, backend)
            summary.statuses[outcome.status] = summary.statuses.get(outcome.status, 0) + 1
            self._report(f"    -> {outcome.status}")
            if outcome.stop_reason:
                summary.stop_reason = outcome.stop_reason
                break
            if outcome.launched and number < len(todo) and cfg.scheduling.cooldown_s > 0:
                if self.cancel.wait(cfg.scheduling.cooldown_s):
                    summary.stop_reason = "interrupted"
                    break

        if summary.stop_reason is None and cfg.integrity.verify_inputs_at_end and todo:
            self._final_integrity(run_id, identity, summary)
        event = "run_interrupted" if summary.stop_reason == "interrupted" else "run_finished"
        store.log(
            event,
            run_id=run_id,
            stop_reason=summary.stop_reason,
            execution_statuses=summary.statuses,
            action_counts=summary.action_counts(),
        )

    def _final_integrity(
        self, run_id: str, identity: CampaignIdentity, summary: RunSummary
    ) -> None:
        self._report("Re-hashing inputs to confirm they did not change during the run")
        problems = []
        for dataset in self.config.datasets:
            try:
                again = hash_dataset(dataset, lambda: self.cancel.cancelled)
            except HashingCancelledError:
                summary.stop_reason = "interrupted"
                self._warn("final input rehash interrupted; inputs not re-verified")
                return
            except Exception as exc:
                problems.append(f"{dataset.id}: {exc}")
                continue
            if again.record != identity.datasets[dataset.id].record:
                problems.append(f"{dataset.id}: content differs from the frozen identity")
        self.store.log(
            "integrity_checked",
            run_id=run_id,
            scope="full input rehash at end of run",
            result="match" if not problems else "mismatch",
            problems=problems,
        )
        if problems:
            summary.stop_reason = "inputs_changed"
            self._warn("inputs changed during the run: " + "; ".join(problems))

    # -- one attempt --------------------------------------------------------------

    def _attempt(
        self,
        run_id: str,
        decision: Decision,
        identity: CampaignIdentity,
        guard: _Guard,
        backend: MonitorBackend | None,
    ) -> _Outcome:
        cfg, store = self.config, self.store
        entry = decision.entry
        workload = cfg.workload(entry.workload)
        dataset = cfg.dataset(entry.dataset)
        tool = identity.tools[entry.tool]
        attempt_id = new_sortable_id()
        attempt_dir = store.new_attempt_dir(attempt_id)
        started_event = store.log(
            "attempt_started",
            run_id=run_id,
            attempt_id=attempt_id,
            entry_key=entry.entry_key,
            reason=str(decision.action),
        )
        started = {
            "schema": STARTED_SCHEMA,
            "campaign_id": store.campaign_id,
            "identity_sha256": store.identity_sha256,
            "attempt_id": attempt_id,
            "run_id": run_id,
            "sequence": started_event["seq"],
            "entry": entry.to_record(),
            "reason": str(decision.action),
            "previous_attempts": list(decision.previous_attempts),
            "host": socket.gethostname(),
            "pid": os.getpid(),
            "started_at": timestamp(),
        }
        publish_json(attempt_dir / STARTED, started)

        state = _AttemptState()
        scratch: ScratchDir | None = None
        try:
            try:
                scratch = create_scratch(
                    cfg.scratch.roots[dataset.scratch],
                    campaign_id=cfg.campaign_id,
                    attempt_id=attempt_id,
                    protected=self._protected_paths(),
                )
            except (ScratchError, OSError) as exc:
                state.fail("scratch", f"{type(exc).__name__}: {exc}")
                state.stop_reason = "scratch_unavailable"
            if scratch is not None:
                inputs = identity.datasets[dataset.id].inputs
                self._dispatch(state, attempt_dir, entry, workload, inputs, tool, scratch, backend)
                after = guard.input_problems(dataset.id)
                state.input_check_after = after or "fingerprints unchanged"
                if after:
                    state.warnings.append("inputs changed during the attempt: " + "; ".join(after))
                    state.stop_reason = "inputs_changed"
                state.outputs = self._inventory(scratch.output_dir)
        finally:
            if scratch is not None:
                state.scratch = self._dispose_scratch(scratch, state.status)
        result = self._result(run_id, decision, started, workload, tool, attempt_dir, state)
        result_sha = store.seal_attempt(attempt_dir, result)
        store.log(
            "attempt_published",
            run_id=run_id,
            attempt_id=attempt_id,
            entry_key=entry.entry_key,
            execution_status=state.status,
            elapsed_ns=result["timing"]["elapsed_ns"] if result["timing"] else None,
            result_sha256=result_sha,
        )
        return _Outcome(
            state.status, state.process is not None and state.process.launched, state.stop_reason
        )

    def _dispatch(
        self,
        state: _AttemptState,
        attempt_dir: Path,
        entry: ScheduleEntry,
        workload: WorkloadConfig,
        inputs: tuple[Path, ...],
        tool: ResolvedTool,
        scratch: ScratchDir,
        backend: MonitorBackend | None,
    ) -> None:
        cfg = self.config
        inv = Invocation(
            workload.id,
            inputs,
            scratch.output_dir,
            scratch.work_dir,
            threads=entry.cpus,
            parameters=dict(workload.parameters),
        )
        try:
            plan = plan_invocation(tool.adapter, inv, availability=tool.availability)
        except AdapterContractError as exc:
            state.fail("planning", str(exc))
            plan = None
        state.plan = plan
        try:
            support = tool.adapter.support_for(workload.id, entry.cpus)
            state.threading_model = str(support.threading.model)
        except Exception:
            state.threading_model = None
        publish_json(attempt_dir / INVOCATION, _invocation_record(inv, plan, scratch))
        if plan is None:
            return
        if not plan.ready:
            state.status = str(plan.status)
            return
        try:
            execution = execute_plan(
                plan,
                inv,
                attempt_dir,
                timeout_s=cfg.timeout_for(workload),
                cancel=self.cancel,
                monitor_backend=backend,
                monitor_required=cfg.monitoring.required,
                thread_budget=ThreadBudget(entry.cpus, state.threading_model),
            )
        except ExecutionInterrupted as exc:
            state.process = exc.result
            state.status = "interrupted"
            state.stop_reason = "interrupted"
            return
        state.execution = execution
        state.process = execution.process
        state.status = str(execution.execution_status)
        state.failure_phase = execution.failure_phase
        state.failure_reason = execution.failure_reason
        state.warnings.extend(execution.warnings)
        state.missing = list(execution.missing_required_outputs)
        if state.status == "interrupted":
            state.stop_reason = "interrupted"

    def _inventory(self, output_dir: Path) -> list[dict[str, Any]]:
        if not output_dir.is_dir():
            return []
        items = []
        for path in iter_files(output_dir):
            items.append(
                {
                    "path": path.relative_to(output_dir).as_posix(),
                    "bytes": path.stat().st_size,
                    "sha256": (
                        sha256_file(path) if self.config.integrity.record_output_digests else None
                    ),
                }
            )
        return items

    def _dispose_scratch(self, scratch: ScratchDir, status: str) -> dict[str, Any]:
        retain = self.config.scratch.retain
        keep = retain == "always" or (
            retain == "on-failure"
            and status not in ("succeeded",)
            and not status.startswith("skipped")
        )
        if keep:
            release_ownership(scratch)
            return {**scratch.to_record(), "retained": True, "cleanup": None}
        cleanup = cleanup_scratch(scratch)
        if not cleanup.complete:
            self._warn(f"scratch cleanup: {cleanup.reason} ({cleanup.path})")
        return {**scratch.to_record(), "retained": False, "cleanup": cleanup.to_record()}

    def _result(
        self,
        run_id: str,
        decision: Decision,
        started: dict[str, Any],
        workload: WorkloadConfig,
        tool: ResolvedTool,
        attempt_dir: Path,
        state: _AttemptState,
    ) -> dict[str, Any]:
        cfg = self.config
        process = state.process
        timing = None
        if process is not None and process.start is not None:
            timing = {
                "timing_boundary": process.timing_boundary,
                "execution_mode": process.execution_mode,
                "elapsed_ns": process.elapsed_ns,
                "elapsed_s": process.elapsed_s,
                "start": process.start.to_record(),
                "end": process.end.to_record() if process.end else None,
                "timeout_s": process.timeout_s,
            }
        if process is not None and process.monitoring:
            monitoring_status = process.monitoring.get("monitoring_status")
        elif not cfg.monitoring.enabled:
            monitoring_status = "disabled"
        else:
            monitoring_status = "not_run"
        plan = state.plan
        numerical = "unverified" if cfg.validation_mode == "performance-only" else "not_run"
        return {
            "schema": RESULT_SCHEMA,
            "campaign_id": self.store.campaign_id,
            "identity_sha256": self.store.identity_sha256,
            "attempt_id": attempt_dir.name,
            "run_id": run_id,
            "sequence": started["sequence"],
            "entry": started["entry"],
            "attempt_reason": str(decision.action),
            "previous_attempts": list(decision.previous_attempts),
            "workload_id": workload.id,
            "parameters": dict(workload.parameters),
            "tool_adapter": tool.config.adapter,
            "tool_version": tool.record.get("version"),
            "execution_status": state.status,
            "failure_phase": state.failure_phase,
            "failure_reason": state.failure_reason,
            "plan_status": str(plan.status) if plan else None,
            "implementation_status": (
                str(plan.implementation_status) if plan and plan.implementation_status else None
            ),
            "skip_reason": plan.reason if plan and not plan.ready else None,
            "launched": bool(process is not None and process.launched),
            "exit_code": process.exit_code if process else None,
            "timing": timing,
            "monitoring_status": monitoring_status,
            "structural_status": "not_run",
            "numerical_status": numerical,
            "validation_mode": cfg.validation_mode,
            "validation_note": NUMERICAL_NOTE,
            "threads": {"requested": decision.entry.cpus, "threading_model": state.threading_model},
            "execution_policy": "sequential; one attempt at a time on this machine",
            "cache_policy": cfg.scheduling.cache_policy,
            "cache_policy_description": CACHE_POLICY_DESCRIPTIONS[cfg.scheduling.cache_policy],
            "cooldown_s": cfg.scheduling.cooldown_s,
            "missing_required_outputs": state.missing,
            "outputs": state.outputs,
            "input_check": {
                "before": "fingerprints unchanged since identity verification",
                "after": state.input_check_after,
            },
            "scratch": state.scratch,
            "warnings": state.warnings,
            "files": self.store.attempt_files(attempt_dir),
            "published_at": timestamp(),
        }


@dataclass
class _AttemptState:
    status: str = "failed"
    failure_phase: str | None = None
    failure_reason: str | None = None
    plan: InvocationPlan | None = None
    execution: AttemptExecution | None = None
    process: ProcessResult | None = None
    threading_model: str | None = None
    missing: list[str] = field(default_factory=list)
    outputs: list[dict[str, Any]] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    scratch: dict[str, Any] | None = None
    input_check_after: Any = None
    stop_reason: str | None = None

    def fail(self, phase: str, reason: str) -> None:
        self.status, self.failure_phase, self.failure_reason = "failed", phase, reason


def _invocation_record(
    inv: Invocation, plan: InvocationPlan | None, scratch: ScratchDir
) -> dict[str, Any]:
    command = plan.command if plan else None
    return {
        "schema": "lidarbench.attempt-invocation/1",
        "workload_id": inv.workload_id,
        "inputs": [os.fspath(p) for p in inv.inputs],
        "output_dir": os.fspath(inv.output_dir),
        "work_dir": os.fspath(inv.work_dir),
        "threads": inv.threads,
        "parameters": dict(inv.parameters),
        "scratch": scratch.to_record(),
        "plan": (
            None
            if plan is None
            else {
                "status": str(plan.status),
                "reason": plan.reason,
                "implementation_status": (
                    str(plan.implementation_status) if plan.implementation_status else None
                ),
                "metadata": dict(plan.metadata),
                "expected_outputs": [
                    {
                        "path": Path(a.path).relative_to(inv.output_dir).as_posix(),
                        "kind": str(a.kind),
                        "required": a.required,
                    }
                    for a in plan.expected_outputs
                ],
            }
        ),
        "command": (
            None
            if command is None
            else {
                "argv": list(command.argv),
                "cwd": os.fspath(command.cwd),
                "env_delta": dict(command.env),
                "config_files": [
                    {"name": c.name, "sha256": sha256_bytes(c.content.encode("utf-8"))}
                    for c in command.config_files
                ],
            }
        ),
    }


class _Guard:
    """Cheap per-entry checks against the identity verified at the start of the run."""

    def __init__(self, store: CampaignStore, identity: CampaignIdentity) -> None:
        self.store = store
        self.identity = identity

    def check(self, entry: ScheduleEntry) -> list[str]:
        problems = self.store.frozen_file_problems()
        for path, sha in self.identity.code_files.items():
            try:
                if sha256_file(path) != sha:
                    problems.append(f"source file changed: {path}")
            except OSError:
                problems.append(f"source file missing: {path}")
        tool = self.identity.tools[entry.tool]
        problems += [
            f"tool {entry.tool}: {p}" for p in fingerprint_problems(tool.executable_fingerprints)
        ]
        problems += self.input_problems(entry.dataset)
        return problems

    def input_problems(self, dataset_id: str) -> list[str]:
        dataset = self.identity.datasets[dataset_id]
        fingerprints: dict[str, Fingerprint] = dataset.fingerprints
        problems = tree_problems(dataset.root, (f["path"] for f in dataset.record["files"]))
        problems += fingerprint_problems(fingerprints)
        return [f"dataset {dataset_id}: {p}" for p in problems]
