"""R bridge adapters: run legacy lidR/lasR workloads through ``Rscript``.

The Python orchestrator never embeds R. An adapter describes one fresh
``Rscript --vanilla bridges/r/runner.R`` process per attempt and reads the
bridge's result record afterwards. The R side reuses the legacy harness
unchanged (``R/1_functions.R`` is sourced, task calls come from ``R/0_setup.R``)
so the algorithms are not reimplemented. See ``design/R_BRIDGE.md``.

Protocol (``lidarbench.r-bridge-invocation/1`` → ``lidarbench.r-bridge-result/1``):

* the orchestrator writes ``<work_dir>/r-invocation.json`` (a :class:`ConfigFile`);
* ``runner.R`` activates the project's renv library, loads the packages,
  checks that the legacy call equals the definition pinned here, configures the
  catalog and native threads exactly as the legacy harness does, runs and times
  the call, and writes ``<work_dir>/r-bridge-result.json``;
* :meth:`RBridgeAdapter.interpret` turns that record into an
  :class:`Interpretation`: errors, any R warning during the workload, a missing
  or inconsistent record, or package versions that differ from the probed
  identity all fail the attempt.

Concrete adapters (:mod:`.lidr`, :mod:`.lasr`) only declare their package,
dependencies, threading model and pinned workload calls.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any, ClassVar

import lidarbench
from lidarbench.core.canonical import sha256_bytes, sha256_file
from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters.base import (
    AcceptedException,
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ConfigFile,
    ImplementationStatus,
    Interpretation,
    Invocation,
    MetadataValue,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    ToolTiming,
    UnavailableReason,
    UnsupportedCombinationError,
    WorkloadSupport,
)
from lidarbench.execution.adapters.launchers import resolve_executable, rscript_argv

if TYPE_CHECKING:
    from lidarbench.execution.process import ProcessResult

INVOCATION_SCHEMA = "lidarbench.r-bridge-invocation/1"
RESULT_SCHEMA = "lidarbench.r-bridge-result/1"
PROBE_SCHEMA = "lidarbench.r-bridge-probe/1"
INVOCATION_FILE = "r-invocation.json"
RESULT_FILE = "r-bridge-result.json"
PROBE_MARKER = "LIDARBENCH_R_PROBE "
RSCRIPT_ENV = "LIDARBENCH_RSCRIPT"
"""Environment variable naming the Rscript executable when no option is given."""

EXIT_CODES: Mapping[int, str] = {
    0: "succeeded",
    1: "R error during preparation, resource setup or the workload",
    2: "invalid invocation or legacy definition mismatch",
    3: "R warning during the workload",
    4: "renv activation or package failure",
    5: "result record could not be written",
}

LEGACY_THREAD_VARIABLES = (
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "RCPP_PARALLEL_NUM_THREADS",
    "OMP_THREAD_LIMIT",
    "OMP_MAX_ACTIVE_LEVELS",
)
"""Thread caps set by the legacy ``benchmark_with_resources()`` (all ``1`` by default)."""

INNER_BOUNDARY = (
    "legacy benchmark_execute_call(): evaluation of the task call only (catalog setup, "
    "package loading, preloading of namespaces the call would load lazily (sf, terra, ...) "
    "and thread/worker setup are outside)"
)
SUPPORTED_PARAMETERS = ("epsg",)
LAS_SUFFIXES = (".las", ".laz")
_STDERR_TAIL = 2000


@dataclass(frozen=True)
class RWorkload:
    """A workload implemented by a pinned legacy call of ``R/0_setup.R``.

    ``call`` is the exact ``task_call_<tool>`` string; the bridge refuses to run
    when the legacy file differs, because a changed definition needs a new
    workload version.
    """

    workload_id: str
    call: str
    buffer: float
    output_extension: str = "tif"
    artifact_pattern: str = "{stem}.tif"
    kind: ArtifactKind = ArtifactKind.RASTER

    @property
    def call_sha256(self) -> str:
        """SHA-256 of the pinned call text."""
        return sha256_bytes(self.call.encode("utf-8"))


@dataclass(frozen=True)
class KnownWarning:
    """A documented R warning that an explicit opt-in policy accepts.

    The bridge exits 3 when the pinned call succeeded but raised R warnings. The
    attempt is accepted (as :class:`AcceptedException` ``name``) only if *every*
    workload-phase warning fully matches ``pattern``; anything else still fails.
    """

    name: str
    policy: str
    pattern: re.Pattern[str]


class ProbeError(RuntimeError):
    """The R probe could not produce a usable record."""


def default_project_root() -> Path:
    """Repository checkout containing this package (``bridges/``, ``R/``, ``renv/``)."""
    return Path(lidarbench.__file__).resolve().parents[2]


def exit_code_meaning(code: int | None) -> str:
    """Human-readable meaning of a bridge exit status."""
    if code is None:
        return "no exit status"
    if code < 0:
        return f"terminated by signal {-code}"
    return EXIT_CODES.get(code, "unexpected exit status (R crashed or was killed?)")


def _tail(text: str, limit: int = _STDERR_TAIL) -> str:
    text = text.strip()
    return text if len(text) <= limit else "…" + text[-limit:]


class RBridgeAdapter(ToolAdapter):
    """Base class of adapters that run one R package through the R bridge.

    Subclasses set :attr:`adapter_id`, :attr:`display_name`, :attr:`r_package`,
    :attr:`contract_tool`, :attr:`required_packages`, :attr:`threading_model`,
    :attr:`workloads` and optionally :attr:`other_workloads` and
    :attr:`native_thread_variables`.

    Constructor options (campaign ``[[tools]] options``):

    * ``rscript``: Rscript executable (name or path). Default: ``$LIDARBENCH_RSCRIPT``
      or ``Rscript`` on ``PATH``.
    * ``project``: directory containing ``bridges/r``, ``R/`` and ``renv/``.
      Default: the repository checkout of this package.
    * ``renv``: activate the project's renv library (default ``True``).
    * ``require_locked_versions``: with renv, refuse R or package versions that
      differ from ``renv.lock`` (default ``True``).
    * ``probe_timeout_s``: timeout of the availability probe (default 600).
    """

    r_package: ClassVar[str]
    """The benchmarked R package (also the reported tool name)."""
    contract_tool: ClassVar[str]
    """Implementation name in the workload contracts (``[implementations.<name>]``)."""
    required_packages: tuple[str, ...]
    """Packages that must be installed and loadable (the bridge needs ``jsonlite``);
    a class default that an adapter instance may extend from its options."""
    threading_model: ClassVar[ThreadingModel]
    workloads: ClassVar[Mapping[str, RWorkload]]
    other_workloads: ClassVar[tuple[WorkloadSupport, ...]] = ()
    """Explicitly declared unsupported/blocked legacy workloads."""
    native_thread_variables: ClassVar[tuple[str, ...]] = ()
    """Variables of :data:`LEGACY_THREAD_VARIABLES` set to the thread budget, not 1."""

    def __init__(
        self,
        rscript: str = "",
        project: str = "",
        renv: bool = True,
        require_locked_versions: bool = True,
        probe_timeout_s: float = 600.0,
    ) -> None:
        for name, value, kind in (
            ("rscript", rscript, str),
            ("project", project, str),
            ("renv", renv, bool),
            ("require_locked_versions", require_locked_versions, bool),
        ):
            if not isinstance(value, kind):
                raise TypeError(f"option {name!r} must be {kind.__name__}, got {value!r}")
        if isinstance(probe_timeout_s, bool) or not isinstance(probe_timeout_s, int | float):
            raise TypeError(f"option 'probe_timeout_s' must be a number, got {probe_timeout_s!r}")
        if probe_timeout_s <= 0:
            raise TypeError("option 'probe_timeout_s' must be > 0")
        self.rscript = rscript
        self.project = Path(project).absolute() if project else default_project_root()
        self.use_renv = renv
        self.require_locked_versions = require_locked_versions
        self.probe_timeout_s = float(probe_timeout_s)
        self._probe_record: dict[str, Any] | None = None
        self._rscript_path: Path | None = None

    # -- files ----------------------------------------------------------------------

    @property
    def bridge_files(self) -> dict[str, Path]:
        """R code executed by the bridge, keyed by role (fingerprinted as executables)."""
        bridge = self.project / "bridges" / "r"
        return {
            "bridge_runner": bridge / "runner.R",
            "bridge_probe": bridge / "probe.R",
            "bridge_helpers": bridge / "bridge.R",
            "legacy_functions": self.project / "R" / "1_functions.R",
            "legacy_setup": self.project / "R" / "0_setup.R",
        }

    @property
    def lockfile(self) -> Path:
        """The project's ``renv.lock``."""
        return self.project / "renv.lock"

    def _required_files(self) -> list[Path]:
        files = list(self.bridge_files.values())
        if self.use_renv:
            files += [self.project / "renv" / "activate.R", self.lockfile]
        return files

    def resolve_rscript(self) -> Path | None:
        """Absolute Rscript path from the option, ``$LIDARBENCH_RSCRIPT`` or ``PATH``."""
        name = self.rscript or os.environ.get(RSCRIPT_ENV, "") or "Rscript"
        return resolve_executable(name)

    # -- probe and identity ---------------------------------------------------------

    def run_probe(self, rscript: Path) -> dict[str, Any]:
        """Run ``bridges/r/probe.R`` and return its record.

        Raises:
            ProbeError: The probe failed, timed out or printed no valid record.
        """
        argv = rscript_argv(
            rscript,
            self.bridge_files["bridge_probe"],
            "--project",
            os.fspath(self.project),
            "--renv",
            "on" if self.use_renv else "off",
            "--packages",
            ",".join(self.required_packages),
        )
        try:
            done = subprocess.run(
                list(argv),
                cwd=self.project,
                capture_output=True,
                timeout=self.probe_timeout_s,
                check=False,
                shell=False,
                stdin=subprocess.DEVNULL,
            )
        except subprocess.TimeoutExpired as exc:
            raise ProbeError(f"R probe timed out after {self.probe_timeout_s:g} s") from exc
        except OSError as exc:
            raise ProbeError(f"cannot start {rscript}: {exc}") from exc
        stdout = done.stdout.decode("utf-8", errors="replace")
        stderr = done.stderr.decode("utf-8", errors="replace")
        lines = [line for line in stdout.splitlines() if line.startswith(PROBE_MARKER)]
        if not lines:
            raise ProbeError(
                f"R probe exited with status {done.returncode} without a record: {_tail(stderr)}"
            )
        try:
            record = json.loads(lines[-1][len(PROBE_MARKER) :])
        except json.JSONDecodeError as exc:
            raise ProbeError(f"R probe printed invalid JSON: {exc}") from exc
        if not isinstance(record, dict) or record.get("schema") != PROBE_SCHEMA:
            raise ProbeError(f"R probe record is not {PROBE_SCHEMA}")
        if record.get("error"):
            raise ProbeError(f"R probe failed: {record['error']}")
        return record

    def assess_probe(self, record: Mapping[str, Any]) -> Availability | None:
        """Turn a probe record into an unavailability, or ``None`` if usable."""
        renv = record.get("renv") or {}
        if self.use_renv and not renv.get("activated"):
            return Availability.unavailable(
                UnavailableReason.MISCONFIGURED,
                f"renv activation in {self.project} failed: {renv.get('error') or 'unknown'}",
            )
        packages = record.get("packages") or {}
        problems = []
        for name in self.required_packages:
            entry = packages.get(name) or {}
            if not entry.get("installed"):
                problems.append(f"{name} (not installed)")
            elif not entry.get("loadable"):
                problems.append(f"{name} (cannot be loaded: {entry.get('error') or 'unknown'})")
        if problems:
            where = "the renv library" if self.use_renv else "the R library"
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED,
                f"R package(s) unavailable in {where}: {', '.join(problems)}. "
                f"Run renv::restore() in {self.project}.",
            )
        if self.use_renv and self.require_locked_versions:
            lock = record.get("lockfile") or {}
            mismatches = []
            if lock.get("r_version") != record.get("r_version"):
                mismatches.append(
                    f"R {record.get('r_version')} (renv.lock: {lock.get('r_version')})"
                )
            locked = lock.get("packages") or {}
            for name in self.required_packages:
                installed = (packages.get(name) or {}).get("installed")
                if locked.get(name) != installed:
                    mismatches.append(f"{name} {installed} (renv.lock: {locked.get(name)})")
            if mismatches:
                return Availability.unavailable(
                    UnavailableReason.VERSION_UNSUPPORTED,
                    "installed versions differ from renv.lock: "
                    + ", ".join(mismatches)
                    + ". Run renv::restore() or set require_locked_versions = false.",
                )
        return None

    def probe(self, platform_info: PlatformInfo) -> Availability:
        """Check bridge files, Rscript, renv and the required packages."""
        self._probe_record = None
        self._rscript_path = None
        missing = [os.fspath(p) for p in self._required_files() if not p.is_file()]
        if missing:
            return Availability.unavailable(
                UnavailableReason.MISCONFIGURED,
                f"R bridge project {self.project} lacks: {', '.join(missing)} "
                "(set the 'project' option to the LidarSpeedTests checkout)",
            )
        rscript = self.resolve_rscript()
        if rscript is None:
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED,
                f"Rscript not found (set the 'rscript' option or ${RSCRIPT_ENV}, or put "
                "Rscript on PATH)",
            )
        try:
            record = self.run_probe(rscript)
        except ProbeError as exc:
            return Availability.unavailable(UnavailableReason.PROBE_FAILED, str(exc))
        problem = self.assess_probe(record)
        if problem is not None:
            return problem
        self._probe_record = record
        self._rscript_path = rscript
        return Availability.ok({"rscript": rscript})

    def _probed(self) -> tuple[dict[str, Any], Path]:
        if self._probe_record is None or self._rscript_path is None:
            from lidarbench.core.platform import get_platform_info

            availability = self.probe(get_platform_info())
            if not availability.available:
                raise RuntimeError(f"{self.adapter_id} is unavailable: {availability.detail}")
        assert self._probe_record is not None and self._rscript_path is not None
        return self._probe_record, self._rscript_path

    def identify(self) -> ToolIdentity:
        """R and package versions from the probe; bridge and legacy R files as executables."""
        record, rscript = self._probed()
        packages = record.get("packages") or {}
        locked = (record.get("lockfile") or {}).get("packages") or {}
        details: dict[str, MetadataValue] = {
            "r_package": self.r_package,
            "contract_tool": self.contract_tool,
            "bridge_protocol": INVOCATION_SCHEMA,
            "project": os.fspath(self.project),
            "r_version": record.get("r_version"),
            "r_platform": record.get("platform"),
            "blas": record.get("blas"),
            "lapack": record.get("lapack"),
            "renv": self.use_renv,
            "renv_version": (record.get("renv") or {}).get("version"),
            "renv_lockfile_sha256": sha256_file(self.lockfile) if self.lockfile.is_file() else None,
            "lockfile_r_version": (record.get("lockfile") or {}).get("r_version"),
            "require_locked_versions": self.require_locked_versions,
            "library_paths": os.pathsep.join(record.get("library_paths") or []),
        }
        for name in self.required_packages:
            details[f"package.{name}"] = (packages.get(name) or {}).get("installed")
            details[f"locked.{name}"] = locked.get(name)
        return ToolIdentity(
            tool=self.r_package,
            version=(packages.get(self.r_package) or {}).get("installed"),
            executables={"rscript": rscript, **self.bridge_files},
            details=details,
        )

    # -- workloads and commands -----------------------------------------------------

    def workload_support(self) -> list[WorkloadSupport]:
        """Pinned workloads are runnable; other legacy workloads are declared explicitly."""
        threading = ThreadingSupport(self.threading_model, 1, None)
        runnable = [
            WorkloadSupport(w.workload_id, ImplementationStatus.RUNNABLE, threading=threading)
            for w in self.workloads.values()
        ]
        return runnable + list(self.other_workloads)

    def _workload(self, invocation: Invocation) -> RWorkload:
        try:
            return self.workloads[invocation.workload_id]
        except KeyError:
            raise UnsupportedCombinationError(
                f"{self.adapter_id} has no pinned R call for {invocation.workload_id!r}"
            ) from None

    def _epsg(self, invocation: Invocation) -> int | None:
        unknown = sorted(set(invocation.parameters) - set(SUPPORTED_PARAMETERS))
        if unknown:
            raise UnsupportedCombinationError(
                f"{self.adapter_id} does not accept parameter(s) {unknown} for "
                f"{invocation.workload_id}; supported: {list(SUPPORTED_PARAMETERS)}"
            )
        epsg = invocation.parameters.get("epsg")
        if epsg is None:
            return None
        if isinstance(epsg, bool) or not isinstance(epsg, int) or epsg <= 0:
            raise UnsupportedCombinationError(
                f"parameter epsg must be a positive integer: {epsg!r}"
            )
        return epsg

    def _check_inputs(self, invocation: Invocation) -> None:
        stems: dict[str, Path] = {}
        for path in invocation.inputs:
            if path.suffix.lower() not in LAS_SUFFIXES:
                raise UnsupportedCombinationError(
                    f"{self.adapter_id} reads LAS/LAZ point clouds only, got {path.name}"
                )
            key = path.stem.casefold()
            if key in stems:
                raise UnsupportedCombinationError(
                    f"inputs {stems[key].name} and {path.name} would write the same output"
                )
            stems[key] = path

    def thread_env(self, threads: int) -> dict[str, str]:
        """Environment delta: the legacy thread caps, set before R starts."""
        env = dict.fromkeys(LEGACY_THREAD_VARIABLES, "1")
        env.update(dict.fromkeys(self.native_thread_variables, str(threads)))
        return env

    def bridge_invocation(self, invocation: Invocation) -> dict[str, Any]:
        """The ``r-invocation.json`` document for ``invocation``."""
        workload = self._workload(invocation)
        return {
            "schema": INVOCATION_SCHEMA,
            "adapter_id": self.adapter_id,
            "tool": self.r_package,
            "workload_id": invocation.workload_id,
            "task_id": workload.workload_id,
            "expected_call": workload.call,
            "output_extension": workload.output_extension,
            "buffer": workload.buffer,
            "inputs": [os.fspath(p) for p in invocation.inputs],
            "output_dir": os.fspath(invocation.output_dir),
            "work_dir": os.fspath(invocation.work_dir),
            "threads": invocation.threads,
            "threading_model": str(self.threading_model),
            "epsg": self._epsg(invocation),
            "required_packages": list(self.required_packages),
        }

    def build_command(self, invocation: Invocation) -> CommandSpec:
        """``Rscript --vanilla runner.R --project … --invocation … --result …``."""
        self._check_inputs(invocation)
        document = self.bridge_invocation(invocation)
        rscript = self._rscript_path or self.resolve_rscript()
        if rscript is None:
            raise RuntimeError("Rscript not found; probe the adapter first")
        argv = rscript_argv(
            rscript,
            self.bridge_files["bridge_runner"],
            "--project",
            os.fspath(self.project),
            "--renv",
            "on" if self.use_renv else "off",
            "--invocation",
            invocation.config_path(INVOCATION_FILE),
            "--result",
            invocation.work_dir / RESULT_FILE,
        )
        content = json.dumps(document, indent=2, sort_keys=True, ensure_ascii=False) + "\n"
        known = self.known_warning(invocation)
        return CommandSpec(
            argv=argv,
            cwd=invocation.work_dir,
            env=self.thread_env(invocation.threads),
            config_files=(ConfigFile(INVOCATION_FILE, content),),
            accepted_exceptions=(known.name,) if known else (),
        )

    def known_warning(self, invocation: Invocation) -> KnownWarning | None:
        """The opted-in accepted R warning for this invocation (none by default)."""
        return None

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        """One raster per input tile, named after the tile (``{stem}.tif``)."""
        workload = self._workload(invocation)
        return [
            ArtifactSpec(
                invocation.output_dir / workload.artifact_pattern.replace("{stem}", path.stem),
                workload.kind,
                required=True,
                description=f"{workload.workload_id} output of {self.r_package} for {path.name}",
            )
            for path in invocation.inputs
        ]

    def invocation_metadata(self, invocation: Invocation) -> Mapping[str, MetadataValue]:
        """Bridge protocol, pinned call digest, buffer and inner timing boundary."""
        workload = self._workload(invocation)
        return {
            "bridge_protocol": INVOCATION_SCHEMA,
            "r_package": self.r_package,
            "legacy_task_id": workload.workload_id,
            "legacy_call_sha256": workload.call_sha256,
            "buffer": workload.buffer,
            "epsg": self._epsg(invocation),
            "renv": self.use_renv,
            "threading_model": str(self.threading_model),
            "inner_timing_boundary": INNER_BOUNDARY,
        }

    # -- interpretation -------------------------------------------------------------

    def interpret(self, invocation: Invocation, process: ProcessResult) -> Interpretation:
        """Read ``r-bridge-result.json``; anything but a clean, consistent success fails."""
        path = invocation.work_dir / RESULT_FILE
        meaning = exit_code_meaning(process.exit_code)
        if not path.is_file():
            stderr = _tail(process.stderr_text()) if process.stderr_path.exists() else ""
            return Interpretation(
                failure_reason=(
                    f"R bridge wrote no result record (exit {process.exit_code}: {meaning})"
                    + (f": {stderr}" if stderr else "")
                ),
                details={"exit_code_meaning": meaning},
            )
        try:
            record = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
            return Interpretation(failure_reason=f"unreadable R bridge record: {exc}")
        if not isinstance(record, dict) or record.get("schema") != RESULT_SCHEMA:
            return Interpretation(failure_reason=f"R bridge record is not {RESULT_SCHEMA}")

        warnings = [
            (w.get("phase"), str(w.get("message")))
            for w in record.get("warnings") or []
            if isinstance(w, dict)
        ]
        known = self.known_warning(invocation)
        evidence = tuple(m for p, m in warnings if p == "workload")
        accepting = (
            known is not None
            and bool(evidence)
            and all(known.pattern.fullmatch(m) for m in evidence)
            and record.get("status") == "failed"
            and record.get("phase") == "workload"
            and record.get("exit_code") == 3
            and (record.get("legacy") or {}).get("execution_status") == "succeeded"
        )
        failure = self._failure(record, invocation, process, meaning, accepting)
        accepted = None
        if accepting and failure is None:
            assert known is not None
            accepted = AcceptedException(known.name, 3, known.policy, evidence)
        return Interpretation(
            failure_reason=failure,
            warnings=tuple(
                f"R warning ({p}): {m}" for p, m in warnings if not (accepted and p == "workload")
            ),
            tool_timing=_tool_timing(record),
            details={"exit_code_meaning": meaning, "bridge_record": record},
            accepted_exception=accepted,
        )

    def _failure(
        self,
        record: Mapping[str, Any],
        invocation: Invocation,
        process: ProcessResult,
        meaning: str,
        accepting: bool = False,
    ) -> str | None:
        status, phase = record.get("status"), record.get("phase")
        if record.get("tool") not in (None, self.r_package) or record.get("workload_id") not in (
            None,
            invocation.workload_id,
        ):
            return (
                f"R bridge record describes {record.get('tool')}/{record.get('workload_id')}, "
                f"not {self.r_package}/{invocation.workload_id}"
            )
        if status != "succeeded" and not accepting:
            return (
                f"R bridge failed in phase {phase} (exit {process.exit_code}: {meaning}): "
                f"{record.get('diagnostic')}"
            )
        expected_exit, expected_phase = (3, "workload") if accepting else (0, "complete")
        if (
            process.exit_code != expected_exit
            or record.get("exit_code") != expected_exit
            or phase != expected_phase
        ):
            return (
                f"R bridge record says {status} but the process exited with "
                f"{process.exit_code} ({meaning}); not trusted"
            )
        if record.get("tool") != self.r_package:
            return "R bridge success record does not name the tool"
        timing = record.get("timing_workload") or {}
        if not isinstance(timing.get("start_wall_ns"), int) or not isinstance(
            timing.get("end_wall_ns"), int
        ):
            return "R bridge success record has no workload interval"
        if self._probe_record is not None:
            probed = self._probe_record.get("packages") or {}
            ran = record.get("packages") or {}
            changed = [
                f"{name} {(probed.get(name) or {}).get('installed')} -> {ran.get(name)}"
                for name in self.required_packages
                if (probed.get(name) or {}).get("installed") != ran.get(name)
            ]
            if (record.get("r") or {}).get("version") != self._probe_record.get("r_version"):
                changed.append(
                    f"R {self._probe_record.get('r_version')} -> "
                    f"{(record.get('r') or {}).get('version')}"
                )
            if changed:
                return (
                    "R or package versions at run time differ from the probed tool identity: "
                    + ", ".join(changed)
                )
        return None


def _tool_timing(record: Mapping[str, Any]) -> ToolTiming | None:
    workload = record.get("timing_workload") or {}
    start, end = workload.get("start_wall_ns"), workload.get("end_wall_ns")
    if not isinstance(start, int) or not isinstance(end, int) or end < start:
        return None
    marks = {
        name: value
        for name, value in (record.get("timing") or {}).items()
        if isinstance(value, int) and not isinstance(value, bool)
    }
    return ToolTiming(
        start_wall_ns=start,
        end_wall_ns=end,
        clock=str((record.get("timing") or {}).get("clock") or "R Sys.time()"),
        boundary=INNER_BOUNDARY,
        marks=marks,
    )
