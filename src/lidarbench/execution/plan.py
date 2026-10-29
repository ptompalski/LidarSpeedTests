"""Turn an adapter + invocation into a validated command or an explicit skip.

This is the boundary the runner uses: it never needs tool-specific code. Missing
software, unsupported and blocked combinations become skip plans with reasons
(the ``skipped_*`` execution statuses of ARCHITECTURE.md §8.1), not failures.
Adapter output is validated here so that a defective adapter cannot smuggle a
shell string, a relative executable or an output outside the attempt directory
into a benchmark.
"""

from __future__ import annotations

import os
import re
from collections.abc import Mapping
from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path

from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters.base import (
    EXCEPTION_NAME_PATTERN,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ImplementationStatus,
    Invocation,
    MetadataValue,
    ToolAdapter,
    UnavailableReason,
    UnsupportedCombinationError,
)


class PlanStatus(StrEnum):
    """Outcome of planning one attempt."""

    READY = "ready"
    SKIPPED_UNAVAILABLE = "skipped_unavailable"
    SKIPPED_UNSUPPORTED = "skipped_unsupported"
    SKIPPED_BLOCKED = "skipped_blocked"


class AdapterContractError(Exception):
    """An adapter returned something that violates the adapter contract."""


@dataclass(frozen=True)
class InvocationPlan:
    """Planned attempt: either a ready command or a skip with a reason."""

    adapter_id: str
    workload_id: str
    status: PlanStatus
    implementation_status: ImplementationStatus | None
    reason: str = ""
    command: CommandSpec | None = None
    expected_outputs: tuple[ArtifactSpec, ...] = ()
    metadata: Mapping[str, MetadataValue] = field(default_factory=dict)

    @property
    def ready(self) -> bool:
        """Whether a process should be launched."""
        return self.status is PlanStatus.READY


_RESERVED_WINDOWS_NAMES = re.compile(r"^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\..*)?$", re.IGNORECASE)
_INVALID_COMPONENT_CHARS = re.compile(r'[<>:"/\\|?*\x00-\x1f]')
_ENV_NAME = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


def _check_file_component(name: str) -> None:
    if (
        not name
        or name in (".", "..")
        or _INVALID_COMPONENT_CHARS.search(name)
        or _RESERVED_WINDOWS_NAMES.match(name)
        or name.endswith((" ", "."))
    ):
        raise AdapterContractError(f"config file name is not a portable file name: {name!r}")


def _inside(path: Path, container: Path) -> bool:
    try:
        Path(os.path.normpath(path)).relative_to(os.path.normpath(container))
    except ValueError:
        return False
    return True


def validate_command_spec(spec: CommandSpec) -> None:
    """Check a :class:`CommandSpec` against the adapter contract.

    Raises:
        AdapterContractError: On any violation.
    """
    if not isinstance(spec.argv, tuple) or not spec.argv:
        raise AdapterContractError("argv must be a non-empty tuple")
    for i, arg in enumerate(spec.argv):
        if not isinstance(arg, str):
            raise AdapterContractError(f"argv[{i}] is {type(arg).__name__}, not str")
        if "\x00" in arg:
            raise AdapterContractError(f"argv[{i}] contains a NUL character")
    if not Path(spec.argv[0]).is_absolute():
        raise AdapterContractError(f"argv[0] must be an absolute executable path: {spec.argv[0]!r}")
    if not Path(spec.cwd).is_absolute():
        raise AdapterContractError(f"cwd must be absolute: {spec.cwd}")
    for key, value in spec.env.items():
        if not isinstance(key, str) or not _ENV_NAME.match(key):
            raise AdapterContractError(f"invalid environment variable name: {key!r}")
        if not isinstance(value, str) or "\x00" in value:
            raise AdapterContractError(f"environment variable {key} must be a NUL-free string")
    names = [c.name for c in spec.config_files]
    for name in names:
        _check_file_component(name)
    if len({n.casefold() for n in names}) != len(names):
        raise AdapterContractError(f"duplicate config file names: {names}")
    for cfg in spec.config_files:
        if not isinstance(cfg.content, str):
            raise AdapterContractError(f"config file {cfg.name} content must be str")
    if not isinstance(spec.accepted_exceptions, tuple):
        raise AdapterContractError("accepted_exceptions must be a tuple of names")
    for name in spec.accepted_exceptions:
        if not isinstance(name, str) or not EXCEPTION_NAME_PATTERN.match(name):
            raise AdapterContractError(f"invalid accepted exception name: {name!r}")


def _skip(
    adapter: ToolAdapter,
    invocation: Invocation,
    status: PlanStatus,
    reason: str,
    implementation_status: ImplementationStatus | None,
) -> InvocationPlan:
    return InvocationPlan(
        adapter_id=adapter.adapter_id,
        workload_id=invocation.workload_id,
        status=status,
        implementation_status=implementation_status,
        reason=reason,
    )


def plan_invocation(
    adapter: ToolAdapter,
    invocation: Invocation,
    availability: Availability | None = None,
    platform_info: PlatformInfo | None = None,
) -> InvocationPlan:
    """Plan one attempt.

    Args:
        adapter: The tool adapter.
        invocation: The tool-neutral request.
        availability: A cached probe result. If ``None``, the adapter is probed
            with ``platform_info`` (detected when also ``None``).

    Returns:
        A ready plan, or a skip plan with a reason.

    Raises:
        AdapterContractError: The adapter returned an invalid command, output
            list or metadata, or raised an unexpected exception.
    """
    if availability is None:
        if platform_info is None:
            from lidarbench.core.platform import get_platform_info

            platform_info = get_platform_info()
        try:
            availability = adapter.probe(platform_info)
        except Exception as exc:
            availability = Availability.unavailable(
                UnavailableReason.PROBE_FAILED, f"probe raised {type(exc).__name__}: {exc}"
            )
    if not isinstance(availability, Availability):
        availability = Availability.unavailable(
            UnavailableReason.PROBE_FAILED,
            f"probe returned {type(availability).__name__}, not Availability",
        )
    if not availability.available:
        reason = f"{availability.reason}: {availability.detail}"
        return _skip(adapter, invocation, PlanStatus.SKIPPED_UNAVAILABLE, reason, None)

    try:
        support = adapter.support_for(invocation.workload_id, invocation.threads)
    except Exception as exc:
        raise AdapterContractError(
            f"adapter {adapter.adapter_id!r} failed to declare support: "
            f"{type(exc).__name__}: {exc}"
        ) from exc
    if support.status is ImplementationStatus.BLOCKED:
        return _skip(
            adapter, invocation, PlanStatus.SKIPPED_BLOCKED, support.reason, support.status
        )
    if not support.status.dispatchable:
        return _skip(
            adapter, invocation, PlanStatus.SKIPPED_UNSUPPORTED, support.reason, support.status
        )

    try:
        command = adapter.build_command(invocation)
        outputs = tuple(adapter.expected_outputs(invocation))
        metadata = dict(adapter.invocation_metadata(invocation))
    except UnsupportedCombinationError as exc:
        return _skip(
            adapter,
            invocation,
            PlanStatus.SKIPPED_UNSUPPORTED,
            exc.reason,
            ImplementationStatus.UNSUPPORTED,
        )
    except Exception as exc:
        raise AdapterContractError(
            f"adapter {adapter.adapter_id!r} failed to plan {invocation.workload_id!r}: "
            f"{type(exc).__name__}: {exc}"
        ) from exc

    if not isinstance(command, CommandSpec):
        raise AdapterContractError(f"build_command returned {type(command).__name__}")
    validate_command_spec(command)
    for artifact in outputs:
        if not isinstance(artifact, ArtifactSpec):
            raise AdapterContractError(f"expected_outputs item is {type(artifact).__name__}")
        if not Path(artifact.path).is_absolute() or not _inside(
            artifact.path, invocation.output_dir
        ):
            raise AdapterContractError(
                f"expected output {artifact.path} is outside {invocation.output_dir}"
            )
    for key, value in metadata.items():
        if not isinstance(key, str) or not isinstance(value, str | int | float | bool | None):
            raise AdapterContractError(f"invocation metadata {key!r} is not a JSON scalar")

    return InvocationPlan(
        adapter_id=adapter.adapter_id,
        workload_id=invocation.workload_id,
        status=PlanStatus.READY,
        implementation_status=support.status,
        command=command,
        expected_outputs=outputs,
        metadata=metadata,
    )


def write_config_files(command: CommandSpec, work_dir: Path) -> list[Path]:
    """Write a command's config files into ``work_dir`` (exclusive create, UTF-8).

    Returns:
        The written paths, in declaration order.

    Raises:
        FileExistsError: A config file already exists (records are write-once).
    """
    validate_command_spec(command)
    written = []
    for cfg in command.config_files:
        path = work_dir / cfg.name
        with open(path, "x", encoding="utf-8", newline="") as handle:
            handle.write(cfg.content)
        written.append(path)
    return written
