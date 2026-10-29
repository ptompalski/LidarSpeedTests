"""Choice of the logical CPUs an attempt is pinned to (``cpu_affinity = "pinned"``).

A thread budget of ``n`` CPUs is enforced by restricting the tool's whole
process tree to ``n`` logical CPUs, one per physical core where possible, so
internal thread pools larger than the budget (for example PDAL's LAZ reader)
cannot use more cores. The set is deterministic for a machine and budget:

* Linux: the CPUs allowed for this process, grouped by (package, core) from
  ``/sys/devices/system/cpu/cpu*/topology``; the first logical CPU of each core,
  in (package, core) order. The first core is left to the orchestrator and the
  monitor when the budget leaves at least one other core free. SMT siblings are
  used only when the budget exceeds the number of physical cores.
* Windows: logical CPUs in order; when there are twice as many logical as
  physical CPUs, siblings are assumed adjacent (0/1, 2/3, ...) and even CPUs come
  first. Core 0 is reserved in the same way.
* macOS has no affinity API: pinning is reported as unsupported.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import psutil

SYSFS_CPU = Path("/sys/devices/system/cpu")


def _linux_cores(allowed: list[int]) -> list[list[int]]:
    """Logical CPUs grouped per physical core, ordered by (package, core id)."""
    cores: dict[tuple[int, int], list[int]] = {}
    for cpu in allowed:
        topo = SYSFS_CPU / f"cpu{cpu}" / "topology"
        try:
            package = int((topo / "physical_package_id").read_text().strip())
            core = int((topo / "core_id").read_text().strip())
        except (OSError, ValueError):
            package, core = 0, cpu
        cores.setdefault((package, core), []).append(cpu)
    return [sorted(cpus) for _, cpus in sorted(cores.items())]


def _windows_cores(allowed: list[int]) -> list[list[int]]:
    physical = psutil.cpu_count(logical=False) or len(allowed)
    if len(allowed) == 2 * physical:
        return [allowed[i : i + 2] for i in range(0, len(allowed), 2)]
    return [[cpu] for cpu in allowed]


def _order(cores: list[list[int]], n: int) -> list[int]:
    if len(cores) > n:
        cores = cores[1:]  # leave the first core to the orchestrator and the monitor
    first = [core[0] for core in cores]
    siblings = [cpu for core in cores for cpu in core[1:]]
    return (first + siblings)[:n]


def _allowed_cores() -> tuple[list[int], list[list[int]]] | str:
    """Allowed logical CPUs and their grouping per physical core, or why pinning is impossible."""
    if sys.platform == "linux":
        allowed = sorted(os.sched_getaffinity(0))
        return allowed, _linux_cores(allowed)
    if sys.platform == "win32":
        try:
            allowed = sorted(getattr(psutil.Process(), "cpu_affinity")() or [])  # noqa: B009
        except (psutil.Error, AttributeError) as exc:
            return f"cannot read the CPU affinity: {exc}"
        return allowed, _windows_cores(allowed)
    return f"CPU pinning is not supported on {sys.platform}"


def select_cpus(n: int) -> tuple[tuple[int, ...] | None, str]:
    """Logical CPUs for a budget of ``n``, or ``None`` with the reason it cannot be pinned."""
    if n < 1:
        raise ValueError("n must be >= 1")
    found = _allowed_cores()
    if isinstance(found, str):
        return None, found
    allowed, cores = found
    if n > len(allowed):
        return None, f"budget of {n} CPUs exceeds the {len(allowed)} CPUs available"
    chosen = _order(cores, n)
    detail = "one logical CPU per physical core" if n <= len(cores) else "SMT siblings included"
    if len(cores) > n:
        detail += "; first core left to the orchestrator and monitor"
    return tuple(sorted(chosen)), detail
