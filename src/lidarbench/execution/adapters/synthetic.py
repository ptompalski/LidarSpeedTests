"""Synthetic test adapter: deterministic work with known outputs.

It implements its own ``synthetic.*`` workload IDs and never claims a real LiDAR
workload, so its results cannot be confused with benchmark evidence. It runs
:mod:`lidarbench.execution.adapters.synthetic_worker` in a child Python process.

Declared workloads:

- ``synthetic.checksum`` (runnable, argv only): one ``<input name>.checksum.json``
  per input with byte count and SHA-256.
- ``synthetic.hashchain`` (runnable, config-file style): reads a JSON job file
  written by the orchestrator and iterates SHA-256 ``iterations`` times
  (parameter, default 100000); writes ``hashchain.json``.
- ``synthetic.blocked``: blocked, to exercise skip handling.
- ``synthetic.unsupported``: unsupported, to exercise skip handling.
"""

from __future__ import annotations

import json
import os
import re
import sys
from collections.abc import Mapping
from pathlib import Path

import lidarbench
from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters import synthetic_worker as worker
from lidarbench.execution.adapters.base import (
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ConfigFile,
    ImplementationStatus,
    Invocation,
    MetadataValue,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    UnavailableReason,
    UnsupportedCombinationError,
    WorkloadSupport,
    thread_cap_env,
)
from lidarbench.execution.adapters.launchers import (
    probe_version_output,
    python_module_argv,
    resolve_executable,
)

CHECKSUM = "synthetic.checksum"
HASHCHAIN = "synthetic.hashchain"
BLOCKED = "synthetic.blocked"
UNSUPPORTED = "synthetic.unsupported"

JOB_FILE_NAME = "synthetic-job.json"
HASHCHAIN_OUTPUT_NAME = "hashchain.json"
DEFAULT_ITERATIONS = 100_000
WORKER_MODULE = "lidarbench.execution.adapters.synthetic_worker"

_SINGLE = ThreadingSupport(ThreadingModel.SINGLE_THREADED, 1, 1)


class SyntheticAdapter(ToolAdapter):
    """Deterministic test adapter that needs only a Python interpreter."""

    adapter_id = "synthetic"
    display_name = "Synthetic test adapter"

    def __init__(self, python: str | os.PathLike[str] | None = None) -> None:
        """Create the adapter.

        Args:
            python: Interpreter to run the worker; defaults to the orchestrator's.
        """
        self._python = os.fspath(python) if python is not None else sys.executable

    def _interpreter(self) -> Path | None:
        return resolve_executable(self._python)

    def probe(self, platform_info: PlatformInfo) -> Availability:
        interpreter = self._interpreter()
        if interpreter is None:
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED,
                f"Python interpreter not found: {self._python}",
            )
        return Availability.ok({"python": interpreter})

    def identify(self) -> ToolIdentity:
        interpreter = self._interpreter()
        executables = {"python": interpreter} if interpreter else {}
        python_version = None
        if interpreter is not None:
            text = probe_version_output([os.fspath(interpreter), "--version"]) or ""
            match = re.search(r"Python\s+(\S+)", text)
            python_version = match.group(1) if match else None
        return ToolIdentity(
            tool="synthetic",
            version=lidarbench.__version__,
            executables=executables,
            details={
                "python_version": python_version,
                "output_schema": worker.OUTPUT_SCHEMA,
                "job_schema": worker.JOB_SCHEMA,
            },
        )

    def workload_support(self) -> list[WorkloadSupport]:
        return [
            WorkloadSupport(CHECKSUM, ImplementationStatus.RUNNABLE, threading=_SINGLE),
            WorkloadSupport(HASHCHAIN, ImplementationStatus.RUNNABLE, threading=_SINGLE),
            WorkloadSupport(
                BLOCKED,
                ImplementationStatus.BLOCKED,
                reason="example blocked implementation; exercises skipped_blocked handling",
            ),
            WorkloadSupport(
                UNSUPPORTED,
                ImplementationStatus.UNSUPPORTED,
                reason="example unsupported workload; exercises skipped_unsupported handling",
            ),
        ]

    def _interpreter_or_fail(self) -> Path:
        interpreter = self._interpreter()
        if interpreter is None:
            raise UnsupportedCombinationError(f"Python interpreter not found: {self._python}")
        return interpreter

    @staticmethod
    def _iterations(invocation: Invocation) -> int:
        unknown = sorted(set(invocation.parameters) - {"iterations"})
        if unknown:
            raise UnsupportedCombinationError(f"unknown parameters for {HASHCHAIN}: {unknown}")
        value = invocation.parameters.get("iterations", DEFAULT_ITERATIONS)
        if isinstance(value, bool) or not isinstance(value, int):
            raise UnsupportedCombinationError("parameter 'iterations' must be an integer")
        if not 1 <= value <= worker.MAX_ITERATIONS:
            raise UnsupportedCombinationError(
                f"parameter 'iterations' must be in 1..{worker.MAX_ITERATIONS}"
            )
        return value

    def build_command(self, invocation: Invocation) -> CommandSpec:
        env = {**thread_cap_env(invocation.threads), "PYTHONUTF8": "1"}
        interpreter = self._interpreter_or_fail()

        if invocation.workload_id == CHECKSUM:
            if invocation.parameters:
                raise UnsupportedCombinationError(f"{CHECKSUM} takes no parameters")
            names = [p.name.casefold() for p in invocation.inputs]
            if len(set(names)) != len(names):
                raise UnsupportedCombinationError(
                    f"{CHECKSUM} needs input file names that are unique ignoring case; "
                    "outputs would collide"
                )
            argv = python_module_argv(
                interpreter,
                WORKER_MODULE,
                "checksum",
                "--output-dir",
                invocation.output_dir,
                *invocation.inputs,
            )
            return CommandSpec(argv=argv, cwd=invocation.work_dir, env=env)

        if invocation.workload_id == HASHCHAIN:
            job = {
                "schema": worker.JOB_SCHEMA,
                "inputs": [os.fspath(p) for p in invocation.inputs],
                "output": os.fspath(invocation.output_dir / HASHCHAIN_OUTPUT_NAME),
                "iterations": self._iterations(invocation),
            }
            config = ConfigFile(JOB_FILE_NAME, json.dumps(job, indent=2, sort_keys=True) + "\n")
            argv = python_module_argv(
                interpreter,
                WORKER_MODULE,
                "hashchain",
                "--job",
                invocation.config_path(JOB_FILE_NAME),
            )
            return CommandSpec(argv=argv, cwd=invocation.work_dir, env=env, config_files=(config,))

        raise UnsupportedCombinationError(
            f"{self.adapter_id} cannot build a command for {invocation.workload_id!r}"
        )

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        if invocation.workload_id == CHECKSUM:
            return [
                ArtifactSpec(
                    invocation.output_dir / worker.checksum_output_name(p),
                    ArtifactKind.TABLE,
                    description=f"checksum of {p.name}",
                )
                for p in invocation.inputs
            ]
        if invocation.workload_id == HASHCHAIN:
            return [
                ArtifactSpec(
                    invocation.output_dir / HASHCHAIN_OUTPUT_NAME,
                    ArtifactKind.TABLE,
                    description="iterated SHA-256 digest of all inputs",
                )
            ]
        return []

    def invocation_metadata(self, invocation: Invocation) -> Mapping[str, MetadataValue]:
        if invocation.workload_id == HASHCHAIN:
            return {"iterations": self._iterations(invocation)}
        return {}
