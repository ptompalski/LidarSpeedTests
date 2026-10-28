"""Capability reports: what each adapter can do on this machine."""

from __future__ import annotations

import os
from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Any

from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters.base import (
    ADAPTER_SCHEMA,
    Availability,
    ToolAdapter,
    ToolIdentity,
    UnavailableReason,
    WorkloadSupport,
)
from lidarbench.execution.adapters.registry import AdapterRegistry


@dataclass(frozen=True)
class AdapterReport:
    """Probe, identity and declared workload support of one adapter."""

    adapter_id: str
    display_name: str
    availability: Availability
    identity: ToolIdentity | None = None
    workloads: tuple[WorkloadSupport, ...] = ()
    errors: tuple[str, ...] = field(default_factory=tuple)
    """Adapter defects found while inspecting (raised exceptions, invalid declarations)."""

    def to_dict(self) -> dict[str, Any]:
        """JSON-compatible representation."""
        avail = self.availability
        return {
            "schema": ADAPTER_SCHEMA,
            "adapter_id": self.adapter_id,
            "display_name": self.display_name,
            "available": avail.available,
            "unavailable_reason": avail.reason.value if avail.reason else None,
            "detail": avail.detail,
            "executables": _paths(avail.executables),
            "identity": (
                None
                if self.identity is None
                else {
                    "tool": self.identity.tool,
                    "version": self.identity.version,
                    "executables": _paths(self.identity.executables),
                    "details": dict(self.identity.details),
                }
            ),
            "workloads": [
                {
                    "workload_id": w.workload_id,
                    "status": w.status.value,
                    "reason": w.reason,
                    "threading_model": w.threading.model.value,
                    "min_threads": w.threading.min_threads,
                    "max_threads": w.threading.max_threads,
                }
                for w in self.workloads
            ],
            "errors": list(self.errors),
        }


def _paths(mapping: Mapping[str, os.PathLike[str]]) -> dict[str, str]:
    return {k: os.fspath(v) for k, v in sorted(mapping.items())}


def _describe(exc: BaseException) -> str:
    return f"{type(exc).__name__}: {exc}"


def inspect_adapter(adapter: ToolAdapter, platform_info: PlatformInfo) -> AdapterReport:
    """Probe and describe one adapter without letting adapter defects escape."""
    errors: list[str] = []
    try:
        availability = adapter.probe(platform_info)
        if not isinstance(availability, Availability):
            raise TypeError(f"probe() returned {type(availability).__name__}")
    except Exception as exc:
        errors.append(f"probe: {_describe(exc)}")
        availability = Availability.unavailable(
            UnavailableReason.PROBE_FAILED, f"probe raised {_describe(exc)}"
        )

    identity: ToolIdentity | None = None
    if availability.available:
        try:
            identity = adapter.identify()
            if not isinstance(identity, ToolIdentity):
                raise TypeError(f"identify() returned {type(identity).__name__}")
            for key, value in identity.details.items():
                if not isinstance(value, str | int | float | bool | None):
                    raise TypeError(f"identity detail {key!r} is not a JSON scalar")
        except Exception as exc:
            identity = None
            errors.append(f"identify: {_describe(exc)}")
            availability = Availability.unavailable(
                UnavailableReason.PROBE_FAILED, f"identify raised {_describe(exc)}"
            )

    workloads: tuple[WorkloadSupport, ...] = ()
    try:
        declared = list(adapter.workload_support())
        ids = [w.workload_id for w in declared]
        duplicates = sorted({i for i in ids if ids.count(i) > 1})
        if duplicates:
            errors.append(f"workload_support: duplicate workload ids {duplicates}")
        workloads = tuple(sorted(declared, key=lambda w: w.workload_id))
    except Exception as exc:
        errors.append(f"workload_support: {_describe(exc)}")

    return AdapterReport(
        adapter_id=adapter.adapter_id,
        display_name=adapter.display_name,
        availability=availability,
        identity=identity,
        workloads=workloads,
        errors=tuple(errors),
    )


def inspect_registry(
    registry: AdapterRegistry,
    platform_info: PlatformInfo,
    adapter_ids: list[str] | None = None,
) -> list[AdapterReport]:
    """Inspect registered adapters (all, or only ``adapter_ids``), sorted by ID.

    Raises:
        KeyError: An ID in ``adapter_ids`` is not registered.
    """
    selected = registry.ids()
    if adapter_ids:
        for adapter_id in adapter_ids:
            registry.adapter_class(adapter_id)
        selected = [i for i in selected if i in adapter_ids]
    reports = []
    for adapter_id in selected:
        try:
            adapter = registry.create(adapter_id)
        except Exception as exc:
            cls = registry.adapter_class(adapter_id)
            reports.append(
                AdapterReport(
                    adapter_id=adapter_id,
                    display_name=cls.display_name,
                    availability=Availability.unavailable(
                        UnavailableReason.PROBE_FAILED,
                        f"constructor raised {_describe(exc)}",
                    ),
                    errors=(f"__init__: {_describe(exc)}",),
                )
            )
            continue
        reports.append(inspect_adapter(adapter, platform_info))
    return reports
