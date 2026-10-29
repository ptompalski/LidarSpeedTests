"""Campaign configuration (``lidarbench.campaign-config/1``, TOML).

The configuration is data, never executed. Unknown keys are errors, every
value is type-checked, and all defaults are resolved so that the complete
effective configuration can be frozen into campaign identity
(:meth:`CampaignConfig.to_record`). Relative paths resolve against the
directory containing the configuration file.

Example::

    schema = "lidarbench.campaign-config/1"
    campaign_id = "synthetic-demo-001"
    workspace = "runs"
    cpu_counts = [1]
    validation_mode = "performance-only"

    [scheduling]
    seed = 20261020
    repetitions = 3
    warmups = 1
    tool_order = "balanced"
    cooldown_s = 2.0
    cache_policy = "uncontrolled-warm"

    [monitoring]
    enabled = true
    interval_s = 0.5

    [scratch]
    roots = { local = "scratch" }

    [[datasets]]
    id = "tiny"
    path = "data/tiny"

    # A registered dataset (design/DATASETS.md): the directory is resolved from
    # the manifest's data root and every file is checked against the manifest.
    # [data]
    # roots = { fixtures = "fixtures" }
    # [[datasets]]
    # id = "pair"
    # manifest = "synthetic-tiles-v1/pair"

    [[workloads]]
    id = "synthetic.hashchain"
    parameters = { iterations = 20000 }

    [[tools]]
    id = "synthetic"
"""

from __future__ import annotations

import fnmatch
import math
import os
import re
import tomllib
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import sha256_bytes
from lidarbench.datasets.manifest import (
    SHA256_PATTERN,
    DatasetManifest,
    ManifestError,
    manifest_sha256,
)
from lidarbench.datasets.registry import DatasetRegistry, RegistryError, default_registry_dir
from lidarbench.datasets.roots import DataRootError, DataRoots, load_data_roots
from lidarbench.execution.adapters.base import ADAPTER_ID_PATTERN, WORKLOAD_ID_PATTERN

CONFIG_SCHEMA = "lidarbench.campaign-config/1"

NAME_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,63}$")
CAMPAIGN_ID_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$")
_WINDOWS_RESERVED = re.compile(r"^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\..*)?$", re.IGNORECASE)

VALIDATION_MODES = ("numerical", "performance-only")
TOOL_ORDERS = ("balanced", "randomized")
CPU_AFFINITY_MODES = ("pinned", "none")
CACHE_POLICIES = ("uncontrolled-warm",)
SCRATCH_RETENTION = ("never", "on-failure", "always")
DEFAULT_SEED = 20261020
MAX_SEED = 2**64 - 1

CACHE_POLICY_DESCRIPTIONS = {
    "uncontrolled-warm": (
        "Operating-system file caches are not controlled or dropped. Warm-up attempts "
        "run first and inputs read by earlier attempts may still be cached; timings "
        "describe warm-cache behaviour, not cold-cache behaviour."
    ),
}

ParameterValue = str | int | float | bool


class ConfigError(ValueError):
    """The campaign configuration is invalid."""


def _name(value: Any, where: str, pattern: re.Pattern[str] = NAME_PATTERN) -> str:
    if not isinstance(value, str) or not pattern.match(value):
        raise ConfigError(f"{where} must match {pattern.pattern}, got {value!r}")
    if _WINDOWS_RESERVED.match(value) or value.endswith("."):
        raise ConfigError(f"{where} is not a portable file name: {value!r}")
    return value


def _table(
    value: Any, where: str, allowed: set[str] | None, required: Iterable[str] = ()
) -> dict[str, Any]:
    """Check a TOML table; ``allowed=None`` accepts any key."""
    if not isinstance(value, dict):
        raise ConfigError(f"{where} must be a table")
    if allowed is not None:
        unknown = sorted(set(value) - allowed)
        if unknown:
            raise ConfigError(f"{where}: unknown key(s) {unknown}; allowed: {sorted(allowed)}")
    missing = sorted(set(required) - set(value))
    if missing:
        raise ConfigError(f"{where}: missing required key(s) {missing}")
    return value


def _int(value: Any, where: str, minimum: int, maximum: int | None = None) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ConfigError(f"{where} must be an integer, got {value!r}")
    if value < minimum or (maximum is not None and value > maximum):
        bound = f">= {minimum}" if maximum is None else f"in {minimum}..{maximum}"
        raise ConfigError(f"{where} must be {bound}, got {value}")
    return value


def _number(value: Any, where: str, minimum: float, *, positive: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, int | float):
        raise ConfigError(f"{where} must be a number, got {value!r}")
    number = float(value)
    if not math.isfinite(number) or number < minimum or (positive and number <= 0):
        raise ConfigError(f"{where} must be a finite number {'>' if positive else '>='} {minimum}")
    return number


def _bool(value: Any, where: str) -> bool:
    if not isinstance(value, bool):
        raise ConfigError(f"{where} must be true or false, got {value!r}")
    return value


def _choice(value: Any, where: str, choices: tuple[str, ...], hint: str = "") -> str:
    if value not in choices:
        raise ConfigError(f"{where} must be one of {list(choices)}, got {value!r}{hint}")
    return str(value)


def _params(value: Any, where: str) -> dict[str, ParameterValue]:
    table = _table(value, where, None)
    for key, item in table.items():
        if not isinstance(item, str | int | float | bool) or (
            isinstance(item, float) and not math.isfinite(item)
        ):
            raise ConfigError(f"{where}.{key} must be a string, number or boolean")
    return dict(table)


def _path(value: Any, where: str, base: Path) -> Path:
    if not isinstance(value, str) or not value.strip():
        raise ConfigError(f"{where} must be a non-empty path string")
    path = Path(os.path.expanduser(value))
    return Path(os.path.abspath(path if path.is_absolute() else base / path))


@dataclass(frozen=True)
class SchedulingConfig:
    """Scheduling policy (``schedule-v2``)."""

    seed: int = DEFAULT_SEED
    repetitions: int = 3
    warmups: int = 1
    tool_order: str = "balanced"
    cooldown_s: float = 5.0
    cache_policy: str = "uncontrolled-warm"
    cpu_affinity: str = "pinned"

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation, including the cache-policy statement."""
        return {
            "seed": self.seed,
            "repetitions": self.repetitions,
            "warmups": self.warmups,
            "tool_order": self.tool_order,
            "cooldown_s": self.cooldown_s,
            "cache_policy": self.cache_policy,
            "cache_policy_description": CACHE_POLICY_DESCRIPTIONS[self.cache_policy],
            "cpu_affinity": self.cpu_affinity,
            "execution": "sequential: one attempt at a time; parallelism only inside a tool",
        }


@dataclass(frozen=True)
class ValidationConfig:
    """How the runner validates outputs (``design/VALIDATION.md``)."""

    suite: Path | None = None
    """Workload contract suite directory; ``None`` = the checkout's default suite."""
    accept_known_extent_discrepancies: bool = False
    reuse_identical_outputs: bool = True

    def suite_dir(self) -> Path:
        """The effective suite directory."""
        if self.suite is not None:
            return self.suite
        from lidarbench.workloads.contract import default_suite_dir

        return default_suite_dir()

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "suite": os.fspath(self.suite_dir()),
            "accept_known_extent_discrepancies": self.accept_known_extent_discrepancies,
            "reuse_identical_outputs": self.reuse_identical_outputs,
        }


@dataclass(frozen=True)
class MonitoringConfig:
    """Resource monitoring settings."""

    enabled: bool = True
    backend: str = "psutil"
    interval_s: float = 0.5
    required: bool = True

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "enabled": self.enabled,
            "backend": self.backend,
            "interval_s": self.interval_s,
            "required": self.required,
        }


@dataclass(frozen=True)
class ScratchConfig:
    """Scratch roots (label -> directory) and retention policy."""

    roots: Mapping[str, Path] = field(default_factory=dict)
    retain: str = "never"

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "roots": {k: os.fspath(v) for k, v in sorted(self.roots.items())},
            "retain": self.retain,
        }


@dataclass(frozen=True)
class IntegrityConfig:
    """When identities and digests are verified (see ``design/CAMPAIGNS.md``)."""

    record_output_digests: bool = True
    verify_inputs_at_end: bool = True

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "record_output_digests": self.record_output_digests,
            "verify_inputs_at_end": self.verify_inputs_at_end,
            "full_input_hash": "campaign creation and once at the start of every run "
            "invocation (resume); optionally at the end of a run",
            "per_attempt_input_check": "stat fingerprint (size, mtime_ns, file id), untimed",
        }


@dataclass(frozen=True)
class DatasetConfig:
    """A directory of input files, optionally described by a registered manifest."""

    id: str
    path: Path
    include: tuple[str, ...] = ("*",)
    scratch: str = ""
    manifest: DatasetManifest | None = None

    def input_files(self) -> list[Path]:
        """Input files, sorted.

        Without a manifest: top-level regular files whose names match ``include``.
        With a manifest: its ``input`` files (which must exist).
        """
        if not self.path.is_dir():
            raise ConfigError(f"dataset {self.id!r}: directory not found: {self.path}")
        if self.manifest is not None:
            files = [self.path.joinpath(*f.path.split("/")) for f in self.manifest.inputs]
            missing = [os.fspath(f) for f in files if not f.is_file()]
            if missing:
                raise ConfigError(
                    f"dataset {self.id!r}: manifest inputs missing: {missing[:5]} "
                    f"(run `lidarbench data verify {self.manifest.dataset_id}`)"
                )
            return files
        files = sorted(
            entry
            for entry in self.path.iterdir()
            if entry.is_file()
            and any(fnmatch.fnmatchcase(entry.name, pattern) for pattern in self.include)
        )
        if not files:
            raise ConfigError(
                f"dataset {self.id!r}: no files in {self.path} match {list(self.include)}"
            )
        return files

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        if self.manifest is not None:
            return {
                "id": self.id,
                "path": os.fspath(self.path),
                "manifest": manifest_reference(self.manifest),
                "scratch": self.scratch,
            }
        return {
            "id": self.id,
            "path": os.fspath(self.path),
            "include": list(self.include),
            "scratch": self.scratch,
        }


def manifest_reference(manifest: DatasetManifest) -> dict[str, Any]:
    """The portable part of a manifest frozen into campaign identity."""
    return {
        "dataset_id": manifest.dataset_id,
        "version": manifest.version,
        "kind": manifest.kind,
        "location": manifest.location.to_record(),
        "content_sha256": manifest.content_sha256,
        "manifest_sha256": manifest_sha256(manifest),
    }


@dataclass(frozen=True)
class WorkloadConfig:
    """A workload selected for the campaign."""

    name: str
    id: str
    parameters: Mapping[str, ParameterValue] = field(default_factory=dict)
    datasets: tuple[str, ...] = ()
    timeout_s: float | None = None

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "name": self.name,
            "id": self.id,
            "parameters": dict(sorted(self.parameters.items())),
            "datasets": list(self.datasets),
            "timeout_s": self.timeout_s,
        }


@dataclass(frozen=True)
class ToolConfig:
    """A configured tool instance (an adapter plus constructor options)."""

    id: str
    adapter: str
    options: Mapping[str, ParameterValue] = field(default_factory=dict)

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {"id": self.id, "adapter": self.adapter, "options": dict(self.options)}


@dataclass(frozen=True)
class CampaignConfig:
    """Validated campaign configuration with all defaults resolved."""

    campaign_id: str
    workspace: Path
    datasets: tuple[DatasetConfig, ...]
    workloads: tuple[WorkloadConfig, ...]
    tools: tuple[ToolConfig, ...]
    cpu_counts: tuple[int, ...]
    scheduling: SchedulingConfig = field(default_factory=SchedulingConfig)
    monitoring: MonitoringConfig = field(default_factory=MonitoringConfig)
    scratch: ScratchConfig = field(default_factory=ScratchConfig)
    integrity: IntegrityConfig = field(default_factory=IntegrityConfig)
    validation: ValidationConfig = field(default_factory=ValidationConfig)
    validation_mode: str = "numerical"
    timeout_s: float | None = None
    description: str = ""
    source_path: Path | None = None
    source_sha256: str | None = None

    def dataset(self, dataset_id: str) -> DatasetConfig:
        """Look up a dataset by ID."""
        for dataset in self.datasets:
            if dataset.id == dataset_id:
                return dataset
        raise KeyError(dataset_id)

    def workload(self, name: str) -> WorkloadConfig:
        """Look up a workload by name."""
        for workload in self.workloads:
            if workload.name == name:
                return workload
        raise KeyError(name)

    def tool(self, tool_id: str) -> ToolConfig:
        """Look up a tool by ID."""
        for tool in self.tools:
            if tool.id == tool_id:
                return tool
        raise KeyError(tool_id)

    def workload_datasets(self, workload: WorkloadConfig) -> tuple[str, ...]:
        """Datasets a workload runs on (all datasets if none were listed)."""
        return workload.datasets or tuple(d.id for d in self.datasets)

    def timeout_for(self, workload: WorkloadConfig) -> float | None:
        """Per-attempt timeout: the workload's own or the campaign default."""
        return workload.timeout_s if workload.timeout_s is not None else self.timeout_s

    def to_record(self) -> dict[str, Any]:
        """Frozen form (identity component ``config``); excludes the source file location."""
        return {
            "schema": CONFIG_SCHEMA,
            "campaign_id": self.campaign_id,
            "description": self.description,
            "workspace": os.fspath(self.workspace),
            "validation_mode": self.validation_mode,
            "timeout_s": self.timeout_s,
            "cpu_counts": list(self.cpu_counts),
            "scheduling": self.scheduling.to_record(),
            "monitoring": self.monitoring.to_record(),
            "scratch": self.scratch.to_record(),
            "integrity": self.integrity.to_record(),
            "validation": self.validation.to_record(),
            "datasets": [d.to_record() for d in self.datasets],
            "workloads": [w.to_record() for w in self.workloads],
            "tools": [t.to_record() for t in self.tools],
        }


_TOP_KEYS = {
    "schema",
    "campaign_id",
    "description",
    "workspace",
    "validation_mode",
    "timeout_s",
    "cpu_counts",
    "scheduling",
    "monitoring",
    "scratch",
    "integrity",
    "validation",
    "data",
    "datasets",
    "workloads",
    "tools",
}


def _unique(items: list[str], where: str) -> None:
    seen: set[str] = set()
    for item in items:
        key = item.casefold()
        if key in seen:
            raise ConfigError(f"{where}: duplicate {item!r} (names are compared ignoring case)")
        seen.add(key)


def _list(value: Any, where: str) -> list[Any]:
    if not isinstance(value, list) or not value:
        raise ConfigError(f"{where} must be a non-empty array")
    return value


def parse_config(data: Mapping[str, Any], base_dir: Path) -> CampaignConfig:
    """Validate a parsed TOML document.

    Raises:
        ConfigError: With a message naming the offending key.
    """
    top = _table(
        dict(data),
        "campaign config",
        _TOP_KEYS,
        {"schema", "campaign_id", "cpu_counts", "datasets", "workloads", "tools", "scratch"},
    )
    if top["schema"] != CONFIG_SCHEMA:
        raise ConfigError(f"schema must be {CONFIG_SCHEMA!r}, got {top['schema']!r}")
    campaign_id = _name(top["campaign_id"], "campaign_id", CAMPAIGN_ID_PATTERN)
    description = top.get("description", "")
    if not isinstance(description, str):
        raise ConfigError("description must be a string")
    workspace = _path(top.get("workspace", "lidarbench-workspace"), "workspace", base_dir)
    validation_mode = _choice(
        top.get("validation_mode", "numerical"), "validation_mode", VALIDATION_MODES
    )
    timeout_s = (
        _number(top["timeout_s"], "timeout_s", 0, positive=True) if "timeout_s" in top else None
    )

    cpus = [
        _int(v, f"cpu_counts[{i}]", 1, 4096)
        for i, v in enumerate(_list(top["cpu_counts"], "cpu_counts"))
    ]
    if len(set(cpus)) != len(cpus):
        raise ConfigError("cpu_counts must not contain duplicates")

    sched_raw = _table(
        top.get("scheduling", {}),
        "scheduling",
        {
            "seed",
            "repetitions",
            "warmups",
            "tool_order",
            "cooldown_s",
            "cache_policy",
            "cpu_affinity",
        },
    )
    scheduling = SchedulingConfig(
        seed=_int(sched_raw.get("seed", DEFAULT_SEED), "scheduling.seed", 0, MAX_SEED),
        repetitions=_int(sched_raw.get("repetitions", 3), "scheduling.repetitions", 1, 10_000),
        warmups=_int(sched_raw.get("warmups", 1), "scheduling.warmups", 0, 10_000),
        tool_order=_choice(
            sched_raw.get("tool_order", "balanced"), "scheduling.tool_order", TOOL_ORDERS
        ),
        cooldown_s=_number(sched_raw.get("cooldown_s", 5.0), "scheduling.cooldown_s", 0),
        cache_policy=_choice(
            sched_raw.get("cache_policy", "uncontrolled-warm"),
            "scheduling.cache_policy",
            CACHE_POLICIES,
            "; cold-cache control is not implemented, so no cold-cache policy can be claimed",
        ),
        cpu_affinity=_choice(
            sched_raw.get("cpu_affinity", "pinned"), "scheduling.cpu_affinity", CPU_AFFINITY_MODES
        ),
    )

    mon_raw = _table(
        top.get("monitoring", {}), "monitoring", {"enabled", "backend", "interval_s", "required"}
    )
    backend = mon_raw.get("backend", "psutil")
    if not isinstance(backend, str) or not ADAPTER_ID_PATTERN.match(backend):
        raise ConfigError(f"monitoring.backend must be a plugin name, got {backend!r}")
    monitoring = MonitoringConfig(
        enabled=_bool(mon_raw.get("enabled", True), "monitoring.enabled"),
        backend=backend,
        interval_s=_number(mon_raw.get("interval_s", 0.5), "monitoring.interval_s", 0.05),
        required=_bool(mon_raw.get("required", True), "monitoring.required"),
    )

    scratch_raw = _table(top["scratch"], "scratch", {"roots", "retain"}, {"roots"})
    roots_raw = _table(scratch_raw["roots"], "scratch.roots", None)
    if not roots_raw:
        raise ConfigError("scratch.roots must name at least one directory")
    roots = {
        _name(label, f"scratch.roots label {label!r}"): _path(p, f"scratch.roots.{label}", base_dir)
        for label, p in roots_raw.items()
    }
    _unique(list(roots), "scratch.roots")
    scratch = ScratchConfig(
        roots=roots,
        retain=_choice(scratch_raw.get("retain", "never"), "scratch.retain", SCRATCH_RETENTION),
    )

    integ_raw = _table(
        top.get("integrity", {}), "integrity", {"record_output_digests", "verify_inputs_at_end"}
    )
    integrity = IntegrityConfig(
        record_output_digests=_bool(
            integ_raw.get("record_output_digests", True), "integrity.record_output_digests"
        ),
        verify_inputs_at_end=_bool(
            integ_raw.get("verify_inputs_at_end", True), "integrity.verify_inputs_at_end"
        ),
    )

    val_raw = _table(
        top.get("validation", {}),
        "validation",
        {"suite", "accept_known_extent_discrepancies", "reuse_identical_outputs"},
    )
    validation = ValidationConfig(
        suite=_path(val_raw["suite"], "validation.suite", base_dir) if "suite" in val_raw else None,
        accept_known_extent_discrepancies=_bool(
            val_raw.get("accept_known_extent_discrepancies", False),
            "validation.accept_known_extent_discrepancies",
        ),
        reuse_identical_outputs=_bool(
            val_raw.get("reuse_identical_outputs", True), "validation.reuse_identical_outputs"
        ),
    )

    data_raw = _table(top.get("data", {}), "data", {"registry", "roots_file", "roots"})
    data_context: tuple[DatasetRegistry, DataRoots] | None = None

    def data_context_for() -> tuple[DatasetRegistry, DataRoots]:
        nonlocal data_context
        if data_context is None:
            registry = DatasetRegistry(
                _path(data_raw["registry"], "data.registry", base_dir)
                if "registry" in data_raw
                else default_registry_dir()
            )
            inline = {
                label: _path(p, f"data.roots.{label}", base_dir)
                for label, p in _table(data_raw.get("roots", {}), "data.roots", None).items()
            }
            roots_file = (
                _path(data_raw["roots_file"], "data.roots_file", base_dir)
                if "roots_file" in data_raw
                else None
            )
            try:
                data_context = (registry, load_data_roots(roots_file, inline))
            except DataRootError as exc:
                raise ConfigError(f"data: {exc}") from None
        return data_context

    datasets = []
    for i, raw in enumerate(_list(top["datasets"], "datasets")):
        where = f"datasets[{i}]"
        d = _table(
            raw,
            where,
            {"id", "path", "include", "scratch", "manifest", "manifest_sha256"},
            {"id"},
        )
        label = d.get("scratch")
        if label is None:
            if len(roots) != 1:
                raise ConfigError(f"{where}.scratch must name one of {sorted(roots)}")
            label = next(iter(roots))
        if label not in roots:
            raise ConfigError(f"{where}.scratch {label!r} is not in scratch.roots {sorted(roots)}")
        dataset_id = _name(d["id"], f"{where}.id")
        if ("path" in d) == ("manifest" in d):
            raise ConfigError(f"{where}: give exactly one of 'path' or 'manifest'")
        if "manifest" in d:
            if "include" in d:
                raise ConfigError(f"{where}.include is not allowed with 'manifest'")
            manifest, path = _manifest_dataset(d, where, *data_context_for())
            datasets.append(DatasetConfig(dataset_id, path, (), label, manifest))
            continue
        if "manifest_sha256" in d:
            raise ConfigError(f"{where}.manifest_sha256 requires 'manifest'")
        include = d.get("include", ["*"])
        if (
            not isinstance(include, list)
            or not include
            or not all(isinstance(p, str) and p and "/" not in p and "\\" not in p for p in include)
        ):
            raise ConfigError(f"{where}.include must be a non-empty array of file-name patterns")
        datasets.append(
            DatasetConfig(
                id=dataset_id,
                path=_path(d["path"], f"{where}.path", base_dir),
                include=tuple(include),
                scratch=label,
            )
        )
    _unique([d.id for d in datasets], "datasets")
    dataset_ids = {d.id for d in datasets}

    workloads = []
    for i, raw in enumerate(_list(top["workloads"], "workloads")):
        where = f"workloads[{i}]"
        w = _table(raw, where, {"id", "name", "parameters", "datasets", "timeout_s"}, {"id"})
        workload_id = w["id"]
        if not isinstance(workload_id, str) or not WORKLOAD_ID_PATTERN.match(workload_id):
            raise ConfigError(f"{where}.id is not a valid workload id: {workload_id!r}")
        selected = w.get("datasets", [])
        if not isinstance(selected, list) or not all(s in dataset_ids for s in selected):
            raise ConfigError(f"{where}.datasets must list dataset ids from {sorted(dataset_ids)}")
        workloads.append(
            WorkloadConfig(
                name=_name(w.get("name", workload_id), f"{where}.name"),
                id=workload_id,
                parameters=_params(w.get("parameters", {}), f"{where}.parameters"),
                datasets=tuple(sorted(set(selected))),
                timeout_s=(
                    _number(w["timeout_s"], f"{where}.timeout_s", 0, positive=True)
                    if "timeout_s" in w
                    else None
                ),
            )
        )
    _unique([w.name for w in workloads], "workloads (name)")

    tools = []
    for i, raw in enumerate(_list(top["tools"], "tools")):
        where = f"tools[{i}]"
        t = _table(raw, where, {"id", "adapter", "options"}, {"id"})
        tool_id = _name(t["id"], f"{where}.id")
        adapter = t.get("adapter", tool_id)
        if not isinstance(adapter, str) or not ADAPTER_ID_PATTERN.match(adapter):
            raise ConfigError(f"{where}.adapter is not a valid adapter id: {adapter!r}")
        tools.append(
            ToolConfig(tool_id, adapter, _params(t.get("options", {}), f"{where}.options"))
        )
    _unique([t.id for t in tools], "tools")

    return CampaignConfig(
        campaign_id=campaign_id,
        workspace=workspace,
        datasets=tuple(datasets),
        workloads=tuple(workloads),
        tools=tuple(tools),
        cpu_counts=tuple(sorted(cpus)),
        scheduling=scheduling,
        monitoring=monitoring,
        scratch=scratch,
        integrity=integrity,
        validation=validation,
        validation_mode=validation_mode,
        timeout_s=timeout_s,
        description=description,
    )


def _manifest_dataset(
    d: Mapping[str, Any], where: str, registry: DatasetRegistry, data_roots: DataRoots
) -> tuple[DatasetManifest, Path]:
    """Resolve a ``manifest = "<dataset id>"`` dataset to its manifest and local directory."""
    dataset_id = d["manifest"]
    if not isinstance(dataset_id, str):
        raise ConfigError(f"{where}.manifest must be a dataset ID string")
    try:
        manifest = registry.manifest(dataset_id)
    except (RegistryError, ManifestError) as exc:
        raise ConfigError(f"{where}.manifest: {exc}") from None
    pinned = d.get("manifest_sha256")
    if pinned is not None:
        if not isinstance(pinned, str) or not SHA256_PATTERN.match(pinned):
            raise ConfigError(f"{where}.manifest_sha256 must be a lowercase SHA-256 hex digest")
        if pinned != manifest_sha256(manifest):
            raise ConfigError(
                f"{where}.manifest_sha256 does not match the registered manifest of "
                f"{dataset_id!r} ({manifest_sha256(manifest)})"
            )
    try:
        path = data_roots.resolve(manifest.location.root, manifest.location.path)
    except DataRootError as exc:
        raise ConfigError(f"{where}.manifest: {exc}") from None
    return manifest, Path(os.path.abspath(path))


def load_config(path: str | os.PathLike[str]) -> CampaignConfig:
    """Read and validate a TOML campaign configuration file.

    Raises:
        ConfigError: The file cannot be read or is invalid.
    """
    source = Path(path).resolve()
    try:
        raw = source.read_bytes()
    except OSError as exc:
        raise ConfigError(f"cannot read campaign config {source}: {exc}") from exc
    try:
        data = tomllib.loads(raw.decode("utf-8"))
    except (UnicodeDecodeError, tomllib.TOMLDecodeError) as exc:
        raise ConfigError(f"{source}: invalid TOML: {exc}") from exc
    try:
        config = parse_config(data, source.parent)
    except ConfigError as exc:
        raise ConfigError(f"{source}: {exc}") from None
    return replace(config, source_path=source, source_sha256=sha256_bytes(raw))
