"""Campaign identity: what is frozen, how it is computed and how it is re-checked.

Identity components (each a canonical JSON record with its own SHA-256; the
campaign's ``identity_sha256`` is the digest of the component digests):

========== ==================================================================
code       SHA-256 of every ``lidarbench`` package source file + version
config     the fully resolved campaign configuration (all defaults)
workloads  selected workloads with parameters, timeouts and every tool's
           declared implementation status, reason and threading range
tools      per tool instance: adapter class and module digest, options,
           availability, version, executables (path, size, SHA-256), details
datasets   per dataset: every file (relative path, size, SHA-256) and the
           ordered list of input files given to tools; for manifest datasets
           also the manifest reference (files must equal the manifest)
environment OS, release, architecture, CPU model, logical/physical CPUs,
           total memory, Python implementation and version
schedule   the frozen schedule and its policy (seed, order mode, counts)
========== ==================================================================

Provenance that is recorded but never compared (it may legitimately change
without changing results): git revision/branch/dirty state and diff digest,
host name, boot time, process ID, interpreter path, available memory.

Verification schedule (L3 in ARCHITECTURE.md): the full identity, including a
full SHA-256 of every input file, is computed at campaign creation and once at
the start of every later ``run``. Before each schedule entry only cheap checks
run, outside the timed interval: digests of the frozen campaign records,
digests of the (small) code files, and stat fingerprints (size, mtime, file
ID) of the entry's tool executables and dataset files. The inputs are checked
again by fingerprint right after each attempt, and optionally rehashed in full
when a run ends.
"""

from __future__ import annotations

import fnmatch
import inspect
import os
import platform
import re
import socket
import subprocess
import sys
from collections.abc import Callable, Iterable, Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import psutil

import lidarbench
from lidarbench.config.campaign import (
    CampaignConfig,
    ConfigError,
    DatasetConfig,
    ToolConfig,
    manifest_reference,
)
from lidarbench.core.canonical import digest, sha256_bytes, sha256_file
from lidarbench.core.platform import PlatformInfo
from lidarbench.core.store import iter_files
from lidarbench.datasets.manifest import DatasetManifest
from lidarbench.execution.adapters.base import (
    Availability,
    ToolAdapter,
    UnavailableReason,
)
from lidarbench.execution.adapters.registry import AdapterRegistry

IDENTITY_COMPONENTS = (
    "code",
    "config",
    "workloads",
    "tools",
    "datasets",
    "environment",
    "schedule",
)


class IdentityError(RuntimeError):
    """Identity cannot be computed or does not match the frozen campaign."""


class HashingCancelledError(IdentityError):
    """Hashing stopped because cancellation was requested."""


@dataclass(frozen=True)
class Fingerprint:
    """Cheap change detector for a file: size, modification time and file ID."""

    size: int
    mtime_ns: int
    device: int
    inode: int

    @classmethod
    def of(cls, path: str | os.PathLike[str]) -> Fingerprint:
        """Fingerprint of ``path`` (links followed)."""
        st = os.stat(path)
        return cls(st.st_size, st.st_mtime_ns, st.st_dev, st.st_ino)

    def to_record(self) -> dict[str, int]:
        """JSON-ready representation."""
        return {
            "size": self.size,
            "mtime_ns": self.mtime_ns,
            "device": self.device,
            "inode": self.inode,
        }


def tree_problems(root: Path, expected: Iterable[str]) -> list[str]:
    """Files added to or removed from ``root`` relative to ``expected`` (POSIX paths)."""
    current = {p.relative_to(root).as_posix() for p in iter_files(root)}
    frozen = set(expected)
    return [f"{root / rel}: added" for rel in sorted(current - frozen)] + [
        f"{root / rel}: removed" for rel in sorted(frozen - current)
    ]


def fingerprint_problems(expected: Mapping[str, Fingerprint]) -> list[str]:
    """Compare current fingerprints with ``expected`` (path -> fingerprint)."""
    problems = []
    for path, before in expected.items():
        try:
            now = Fingerprint.of(path)
        except OSError as exc:
            problems.append(f"{path}: {exc.strerror or exc}")
            continue
        if now != before:
            changed = [
                name
                for name in ("size", "mtime_ns", "device", "inode")
                if getattr(now, name) != getattr(before, name)
            ]
            problems.append(f"{path}: changed ({', '.join(changed)})")
    return problems


@dataclass
class ResolvedTool:
    """A configured tool instance with its adapter, probe and identity."""

    config: ToolConfig
    adapter: ToolAdapter
    availability: Availability
    record: dict[str, Any]
    executable_fingerprints: dict[str, Fingerprint] = field(default_factory=dict)


@dataclass(frozen=True)
class DatasetIdentity:
    """Content identity of one dataset plus the fingerprints observed while hashing."""

    record: dict[str, Any]
    inputs: tuple[Path, ...]
    fingerprints: dict[str, Fingerprint]
    root: Path


@dataclass(frozen=True)
class CampaignIdentity:
    """All identity components, their digests and non-frozen check material."""

    components: dict[str, Any]
    digests: dict[str, str]
    identity_sha256: str
    tools: dict[str, ResolvedTool]
    datasets: dict[str, DatasetIdentity]
    code_files: dict[str, str]
    """Absolute path -> SHA-256 of each package source file (cheap re-check)."""


def _cpu_model() -> str | None:
    try:
        if sys.platform.startswith("linux"):
            text = Path("/proc/cpuinfo").read_text(encoding="utf-8", errors="replace")
            for key in ("model name", "Hardware", "Processor", "cpu model"):
                match = re.search(rf"^{key}\s*:\s*(.+)$", text, re.MULTILINE)
                if match:
                    return match.group(1).strip()
        elif sys.platform == "darwin":
            out = subprocess.run(
                ["sysctl", "-n", "machdep.cpu.brand_string"],
                capture_output=True,
                text=True,
                timeout=10,
                check=False,
            )
            if out.returncode == 0 and out.stdout.strip():
                return out.stdout.strip()
    except (OSError, subprocess.SubprocessError):
        pass
    return platform.processor() or None


def environment_component(platform_info: PlatformInfo) -> dict[str, Any]:
    """Stable machine facts frozen into identity."""
    return {
        "system": platform_info.system,
        "release": platform_info.platform_release,
        "architecture": platform_info.architecture,
        "cpu_model": _cpu_model(),
        "logical_cpus": platform_info.cpu_count,
        "physical_cpus": psutil.cpu_count(logical=False),
        "total_memory_bytes": platform_info.total_memory_bytes,
        "python_implementation": platform.python_implementation(),
        "python_version": platform_info.python_version,
    }


def package_root() -> Path:
    """Directory of the installed ``lidarbench`` package."""
    return Path(lidarbench.__file__).resolve().parent


def code_files() -> dict[str, str]:
    """Absolute path -> SHA-256 of every ``lidarbench`` source file."""
    root = package_root()
    return {
        os.fspath(path): sha256_file(path)
        for path in iter_files(root)
        if path.suffix == ".py" and "__pycache__" not in path.parts
    }


def code_component(files: Mapping[str, str]) -> dict[str, Any]:
    """Code identity: package version and per-file digests (relative paths)."""
    root = package_root()
    return {
        "lidarbench_version": lidarbench.__version__,
        "dependencies": {"psutil": psutil.__version__},
        "package_files": {
            Path(path).relative_to(root).as_posix(): sha for path, sha in sorted(files.items())
        },
    }


def _git(args: list[str], cwd: Path) -> str | None:
    try:
        out = subprocess.run(
            ["git", *args],
            cwd=cwd,
            capture_output=True,
            timeout=30,
            check=False,
            stdin=subprocess.DEVNULL,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return out.stdout.decode("utf-8", errors="replace") if out.returncode == 0 else None


def git_provenance(path: Path | None = None) -> dict[str, Any]:
    """Git revision and dirty state of the checkout containing the package, if any."""
    path = path or package_root()
    top = _git(["rev-parse", "--show-toplevel"], path)
    if top is None:
        return {"available": False, "reason": "package is not inside a git checkout"}
    revision = (_git(["rev-parse", "HEAD"], path) or "").strip() or None
    branch = (_git(["rev-parse", "--abbrev-ref", "HEAD"], path) or "").strip() or None
    status = _git(["status", "--porcelain", "--untracked-files=all", "--", "."], path)
    diff = _git(["diff", "HEAD", "--", "."], path)
    return {
        "available": True,
        "toplevel": top.strip(),
        "revision": revision,
        "branch": branch,
        "dirty": bool(status and status.strip()),
        "status_paths": sorted(line[3:] for line in (status or "").splitlines() if line),
        "diff_sha256": sha256_bytes(diff.encode("utf-8")) if diff else None,
        "scope": os.fspath(path),
    }


def provenance_record(platform_info: PlatformInfo, git: Mapping[str, Any]) -> dict[str, Any]:
    """Recorded-but-not-compared facts about where and how a campaign was created."""
    try:
        boot = psutil.boot_time()
    except Exception:
        boot = None
    return {
        "host": socket.gethostname(),
        "boot_time_epoch_s": boot,
        "pid": os.getpid(),
        "python_executable": sys.executable,
        "available_memory_bytes": psutil.virtual_memory().available,
        "platform": platform.platform(),
        "git": dict(git),
    }


def _hash_executable(path: Path) -> tuple[dict[str, Any], Fingerprint | None]:
    try:
        fp = Fingerprint.of(path)
        return {"path": os.fspath(path), "bytes": fp.size, "sha256": sha256_file(path)}, fp
    except OSError as exc:
        return {"path": os.fspath(path), "error": exc.strerror or str(exc)}, None


def resolve_tool(
    tool: ToolConfig, registry: AdapterRegistry, platform_info: PlatformInfo
) -> ResolvedTool:
    """Instantiate, probe and identify one configured tool.

    Raises:
        ConfigError: Unknown adapter or invalid options.
    """
    try:
        cls = registry.adapter_class(tool.adapter)
    except KeyError as exc:
        raise ConfigError(f"tool {tool.id!r}: {exc.args[0]}") from None
    try:
        adapter = cls(**dict(tool.options))
    except TypeError as exc:
        raise ConfigError(f"tool {tool.id!r}: invalid options for {tool.adapter}: {exc}") from None
    try:
        module_file = inspect.getsourcefile(cls)
        module_sha = sha256_file(module_file) if module_file else None
    except (OSError, TypeError):
        module_file, module_sha = None, None
    try:
        availability = adapter.probe(platform_info)
        if not isinstance(availability, Availability):
            raise TypeError(f"probe returned {type(availability).__name__}")
    except Exception as exc:
        availability = Availability.unavailable(
            UnavailableReason.PROBE_FAILED, f"probe raised {type(exc).__name__}: {exc}"
        )
    record: dict[str, Any] = {
        "id": tool.id,
        "adapter": tool.adapter,
        "adapter_class": f"{cls.__module__}.{cls.__qualname__}",
        "adapter_module_sha256": module_sha,
        "options": dict(tool.options),
        "available": availability.available,
        "unavailable_reason": str(availability.reason) if availability.reason else None,
        "unavailable_detail": availability.detail or None,
        "version": None,
        "executables": {},
        "details": {},
    }
    fingerprints: dict[str, Fingerprint] = {}
    if availability.available:
        executables = {role: Path(p) for role, p in availability.executables.items()}
        try:
            ident = adapter.identify()
            record["version"] = ident.version
            record["details"] = dict(ident.details)
            executables.update({role: Path(p) for role, p in ident.executables.items()})
        except Exception as exc:
            availability = Availability.unavailable(
                UnavailableReason.PROBE_FAILED, f"identify raised {type(exc).__name__}: {exc}"
            )
            record.update(
                available=False,
                unavailable_reason=str(availability.reason),
                unavailable_detail=availability.detail,
            )
        for role, path in sorted(executables.items()):
            entry, fp = _hash_executable(path)
            record["executables"][role] = entry
            if fp is not None:
                fingerprints[os.fspath(path)] = fp
    return ResolvedTool(tool, adapter, availability, record, fingerprints)


def contract_records(config: CampaignConfig) -> dict[str, dict[str, Any] | None]:
    """Workload id -> frozen contract reference (``None`` if the suite defines none).

    A suite that cannot be loaded yields an ``error`` entry per workload, so
    identity records why no contract applies.
    """
    from lidarbench.workloads.contract import ContractError, load_suite

    suite_dir = config.validation.suite_dir()
    try:
        suite = load_suite(suite_dir)
    except ContractError as exc:
        return {w.id: {"error": str(exc)} for w in config.workloads}
    out: dict[str, dict[str, Any] | None] = {}
    for workload in config.workloads:
        contract = suite.contracts.get(workload.id)
        out[workload.id] = (
            None
            if contract is None
            else {"suite": suite.name, "key": contract.key, "digest": contract.digest}
        )
    return out


def workloads_component(
    config: CampaignConfig, tools: Mapping[str, ResolvedTool]
) -> dict[str, Any]:
    """Selected workloads, their contracts and each tool's declared implementation."""
    contracts = contract_records(config)
    out = []
    for workload in config.workloads:
        implementations: dict[str, Any] = {}
        for tool_id, resolved in sorted(tools.items()):
            try:
                declared = [
                    s for s in resolved.adapter.workload_support() if s.workload_id == workload.id
                ]
            except Exception as exc:
                implementations[tool_id] = {"error": f"{type(exc).__name__}: {exc}"}
                continue
            if not declared:
                implementations[tool_id] = {"status": "undeclared"}
                continue
            support = declared[0]
            implementations[tool_id] = {
                "status": str(support.status),
                "reason": support.reason,
                "threading_model": str(support.threading.model),
                "min_threads": support.threading.min_threads,
                "max_threads": support.threading.max_threads,
            }
        out.append(
            {
                **workload.to_record(),
                "datasets": list(config.workload_datasets(workload)),
                "effective_timeout_s": config.timeout_for(workload),
                "contract": contracts.get(workload.id),
                "implementations": implementations,
            }
        )
    return {"workloads": out}


def hash_dataset(
    dataset: DatasetConfig, cancelled: Callable[[], bool] | None = None
) -> DatasetIdentity:
    """Hash every file of a dataset in full (SHA-256) and record fingerprints.

    Fingerprints are taken before and after hashing; a file that changed while
    being hashed is an error. A dataset bound to a manifest must match it
    exactly (paths, sizes, SHA-256; files excluded by the manifest layout are
    ignored for the comparison), and the manifest reference is frozen into the
    record.
    """
    inputs = tuple(dataset.input_files())
    files: list[dict[str, Any]] = []
    fingerprints: dict[str, Fingerprint] = {}
    for path in iter_files(dataset.path):
        if cancelled is not None and cancelled():
            raise HashingCancelledError(f"hashing of dataset {dataset.id!r} cancelled")
        before = Fingerprint.of(path)
        sha = sha256_file(path)
        if Fingerprint.of(path) != before:
            raise IdentityError(f"dataset {dataset.id!r}: {path} changed while being hashed")
        fingerprints[os.fspath(path)] = before
        files.append(
            {
                "path": path.relative_to(dataset.path).as_posix(),
                "bytes": before.size,
                "sha256": sha,
            }
        )
    record: dict[str, Any] = {
        "id": dataset.id,
        "inputs": [p.relative_to(dataset.path).as_posix() for p in inputs],
        "files": files,
        "file_count": len(files),
        "total_bytes": sum(f["bytes"] for f in files),
    }
    if dataset.manifest is not None:
        problems = manifest_problems(dataset.manifest, files)
        if problems:
            raise IdentityError(
                f"dataset {dataset.id!r} does not match manifest "
                f"{dataset.manifest.dataset_id!r}: " + "; ".join(problems[:10])
            )
        record["manifest"] = manifest_reference(dataset.manifest)
    return DatasetIdentity(record, inputs, fingerprints, dataset.path)


def manifest_problems(manifest: DatasetManifest, files: Iterable[Mapping[str, Any]]) -> list[str]:
    """Differences between hashed files (path, bytes, sha256) and a manifest."""
    excluded = manifest.layout.exclude
    actual = {
        f["path"]: f
        for f in files
        if not any(fnmatch.fnmatchcase(f["path"].rsplit("/", 1)[-1], p) for p in excluded)
    }
    expected = {f.path: f for f in manifest.files}
    problems = [f"{p}: missing" for p in sorted(set(expected) - set(actual))]
    problems += [f"{p}: not in the manifest" for p in sorted(set(actual) - set(expected))]
    for path in sorted(set(expected) & set(actual)):
        if (actual[path]["bytes"], actual[path]["sha256"]) != (
            expected[path].bytes,
            expected[path].sha256,
        ):
            problems.append(f"{path}: content differs")
    return problems


def resolve_tools(
    config: CampaignConfig, registry: AdapterRegistry, platform_info: PlatformInfo
) -> dict[str, ResolvedTool]:
    """Resolve every configured tool."""
    return {t.id: resolve_tool(t, registry, platform_info) for t in config.tools}


def compute_identity(
    config: CampaignConfig,
    tools: Mapping[str, ResolvedTool],
    platform_info: PlatformInfo,
    schedule_component: Mapping[str, Any],
    cancelled: Callable[[], bool] | None = None,
) -> CampaignIdentity:
    """Compute the full campaign identity (hashes every input file once)."""
    files = code_files()
    datasets = {d.id: hash_dataset(d, cancelled) for d in config.datasets}
    components: dict[str, Any] = {
        "code": code_component(files),
        "config": config.to_record(),
        "workloads": workloads_component(config, tools),
        "tools": {"tools": [tools[t.id].record for t in config.tools]},
        "datasets": {"datasets": [datasets[d.id].record for d in config.datasets]},
        "environment": environment_component(platform_info),
        "schedule": dict(schedule_component),
    }
    digests = {name: digest(components[name]) for name in IDENTITY_COMPONENTS}
    return CampaignIdentity(
        components=components,
        digests=digests,
        identity_sha256=digest(digests),
        tools=dict(tools),
        datasets=datasets,
        code_files=files,
    )


def diff_records(a: Any, b: Any, prefix: str = "$", limit: int = 20) -> list[str]:
    """Human-readable list of differences between two JSON values (bounded)."""
    out: list[str] = []

    def walk(x: Any, y: Any, where: str) -> None:
        if len(out) >= limit:
            return
        if isinstance(x, dict) and isinstance(y, dict):
            for key in sorted(set(x) | set(y)):
                if key not in x:
                    out.append(f"{where}.{key}: added")
                elif key not in y:
                    out.append(f"{where}.{key}: removed")
                else:
                    walk(x[key], y[key], f"{where}.{key}")
                if len(out) >= limit:
                    return
        elif isinstance(x, list) and isinstance(y, list):
            if len(x) != len(y):
                out.append(f"{where}: length {len(x)} -> {len(y)}")
            for i, (p, q) in enumerate(zip(x, y, strict=False)):
                walk(p, q, f"{where}[{i}]")
        elif x != y:
            out.append(f"{where}: {_short(x)} -> {_short(y)}")

    walk(a, b, prefix)
    return out


def _short(value: Any) -> str:
    text = repr(value)
    return text if len(text) <= 80 else text[:77] + "..."


def identity_differences(
    frozen: Mapping[str, Any], current: Mapping[str, Any], names: Iterable[str]
) -> dict[str, list[str]]:
    """Per-component differences for the named components."""
    return {name: diff_records(frozen.get(name), current.get(name), name) for name in names}
