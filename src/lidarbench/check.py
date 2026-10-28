"""``lidarbench check``: operating system facts and detected adapter capabilities."""

from __future__ import annotations

import json
import sys
from typing import Any, TextIO

import lidarbench
from lidarbench.core.platform import PlatformInfo, get_platform_info
from lidarbench.execution.adapters.capabilities import AdapterReport, inspect_registry
from lidarbench.execution.adapters.registry import AdapterRegistry

CHECK_SCHEMA = "lidarbench.check/1"


def build_check_report(
    registry: AdapterRegistry,
    platform_info: PlatformInfo,
    adapter_ids: list[str] | None = None,
) -> tuple[dict[str, Any], list[AdapterReport]]:
    """Collect platform facts and adapter reports.

    Raises:
        KeyError: An ID in ``adapter_ids`` is not registered.
    """
    reports = inspect_registry(registry, platform_info, adapter_ids)
    data = {
        "schema": CHECK_SCHEMA,
        "lidarbench_version": lidarbench.__version__,
        "platform": {
            "system": platform_info.system,
            "release": platform_info.platform_release,
            "architecture": platform_info.architecture,
            "python_version": platform_info.python_version,
            "python_executable": sys.executable,
            "logical_cpus": platform_info.cpu_count,
            "total_memory_bytes": platform_info.total_memory_bytes,
        },
        "adapters": [r.to_dict() for r in reports],
        "load_failures": [
            {"name": f.name, "target": f.target, "error": f.error} for f in registry.failures
        ],
    }
    return data, reports


def _render_text(data: dict[str, Any], out: TextIO) -> None:
    plat = data["platform"]
    gib = plat["total_memory_bytes"] / 2**30
    lines = [
        f"lidarbench {data['lidarbench_version']} environment check",
        "",
        "Platform",
        f"  Operating system : {plat['system']} {plat['release']} ({plat['architecture']})",
        f"  Python           : {plat['python_version']} ({plat['python_executable']})",
        f"  Logical CPUs     : {plat['logical_cpus']}",
        f"  Memory           : {gib:.1f} GiB",
        "",
        f"Adapters ({len(data['adapters'])})",
    ]
    if not data["adapters"]:
        lines.append("  none registered")
    for adapter in data["adapters"]:
        lines.append(f"  {adapter['adapter_id']}: {adapter['display_name']}")
        if adapter["available"]:
            lines.append("    status     : available")
        else:
            lines.append(
                f"    status     : UNAVAILABLE ({adapter['unavailable_reason']}) "
                f"{adapter['detail']}"
            )
        identity = adapter["identity"]
        if identity is not None:
            lines.append(f"    version    : {identity['version'] or 'unknown'}")
            for role, path in identity["executables"].items():
                lines.append(f"    executable : {role} = {path}")
        for error in adapter["errors"]:
            lines.append(f"    ADAPTER ERROR: {error}")
        if adapter["workloads"]:
            lines.append("    workloads  :")
            width = max(len(w["workload_id"]) for w in adapter["workloads"])
            for w in adapter["workloads"]:
                if w["status"] in ("runnable", "diagnostic_only"):
                    hi = "+" if w["max_threads"] is None else f"-{w['max_threads']}"
                    threads = (
                        str(w["min_threads"])
                        if w["max_threads"] == w["min_threads"]
                        else f"{w['min_threads']}{hi}"
                    )
                    note = f"{w['threading_model']}, threads {threads}"
                    if w["reason"]:
                        note += f"; {w['reason']}"
                else:
                    note = w["reason"]
                lines.append(f"      {w['workload_id']:<{width}}  {w['status']:<15}  {note}")
    if data["load_failures"]:
        lines += ["", "Adapter plugins that failed to load"]
        for f in data["load_failures"]:
            lines.append(f"  {f['name']} ({f['target']}): {f['error']}")
    out.write("\n".join(lines) + "\n")


def run_check(
    *,
    as_json: bool = False,
    adapter_ids: list[str] | None = None,
    registry: AdapterRegistry | None = None,
    platform_info: PlatformInfo | None = None,
    out: TextIO | None = None,
    err: TextIO | None = None,
) -> int:
    """Run the check command. Unavailable tools are reported, not failures.

    Returns:
        0 on success, 2 if an unknown adapter ID was requested.
    """
    out = out or sys.stdout
    err = err or sys.stderr
    registry = registry if registry is not None else AdapterRegistry.discover()
    platform_info = platform_info or get_platform_info()
    try:
        data, _ = build_check_report(registry, platform_info, adapter_ids)
    except KeyError as exc:
        err.write(f"Error: {exc.args[0]}\n")
        return 2
    if as_json:
        out.write(json.dumps(data, indent=2, sort_keys=True) + "\n")
    else:
        _render_text(data, out)
    return 0
