"""Guarded supplement campaigns for one report (design/REPORTING.md §3.5).

A supplement is a separate, complete campaign that re-measures units the base
campaign could not report (for example after fixing an execution exception or
a validation rule, which changes lidarbench's code and therefore the frozen
campaign identity). It is accepted only if everything that defines a
measurement is identical to the base campaign:

* environment (hardware, operating system, kernel, Python) and provenance host;
* every dataset it uses (file digests);
* every tool it uses: adapter, options, version, executables and identity
  details (only the adapter's source digest may differ: it is code);
* every workload it uses (parameters, timeout and contract key/digest);
* run settings: validation mode, timeout, monitoring, integrity, validation
  policies and scheduling (repetitions, warm-ups, cache policy, CPU affinity,
  cool-down; only the seed may differ); scratch retention, dataset scratch labels
  and scratch roots on the same filesystem (paths may differ);
* every planned measurement unit is a planned unit of the base campaign.

lidarbench code is the only permitted difference; the changed package files are
listed. The merge rule is mechanical: the base campaign always wins, and a
supplement fills only units in which the base has no included measurement.
"""

from __future__ import annotations

import os
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from typing import Any

from lidarbench.campaign.identity import diff_records
from lidarbench.campaign.store import CampaignStore
from lidarbench.core.store import read_json

SUPPLEMENT_SCHEMA = "lidarbench.report-supplement/1"
CONFIG_EQUAL = ("validation_mode", "timeout_s", "monitoring", "integrity", "validation")
"""Campaign config sections that must be identical."""
SCHEDULING_MAY_DIFFER = ("seed",)
TOOL_RECORD_MAY_DIFFER = ("adapter_module_sha256",)
"""Tool identity fields derived from lidarbench code (the permitted difference)."""


class SupplementError(RuntimeError):
    """A supplement campaign is not compatible with the base campaign."""


Unit = tuple[str, str, int, str]


def measurement_units(store: CampaignStore) -> set[Unit]:
    """Planned measurement units (workload, dataset, CPUs, tool) of a campaign."""
    return {
        (e.workload, e.dataset, e.cpus, e.tool) for e in store.schedule if e.phase == "measurement"
    }


@dataclass
class Supplement:
    """A supplement campaign that passed :func:`check_supplement`."""

    store: CampaignStore
    checks: list[str]
    code_changes: list[str]
    filled_units: list[Unit] = field(default_factory=list)
    unused_units: list[Unit] = field(default_factory=list)

    @property
    def campaign_id(self) -> str:
        """Campaign id of the supplement."""
        return self.store.campaign_id

    def to_record(self) -> dict[str, Any]:
        """JSON-ready description (report metadata)."""
        manifest = self.store.manifest or {}
        return {
            "schema": SUPPLEMENT_SCHEMA,
            "campaign_id": self.campaign_id,
            "identity_sha256": self.store.identity_sha256,
            "created_at": manifest.get("created_at"),
            "checks": list(self.checks),
            "code_changes": list(self.code_changes),
            "filled_units": [list(u) for u in self.filled_units],
            "unused_units": [list(u) for u in self.unused_units],
        }


def _by_id(records: Iterable[Mapping[str, Any]], key: str = "id") -> dict[str, Mapping[str, Any]]:
    return {str(r[key]): r for r in records}


def _without(record: Mapping[str, Any] | None, keys: Iterable[str]) -> dict[str, Any]:
    drop = set(keys)
    return {k: v for k, v in (record or {}).items() if k not in drop}


def _host(store: CampaignStore) -> Any:
    path = store.path / "provenance.json"
    return read_json(path).get("host") if path.is_file() else None


def _code_changes(base: Mapping[str, Any], supp: Mapping[str, Any]) -> list[str]:
    a = dict(base.get("package_files") or {})
    b = dict(supp.get("package_files") or {})
    out = [f"changed {p}" for p in sorted(set(a) & set(b)) if a[p] != b[p]]
    out += [f"added {p}" for p in sorted(set(b) - set(a))]
    out += [f"removed {p}" for p in sorted(set(a) - set(b))]
    for key in ("lidarbench_version", "dependencies"):
        if base.get(key) != supp.get(key):
            out.append(f"{key}: {base.get(key)!r} -> {supp.get(key)!r}")
    return out


def _device(path: str) -> int | None:
    try:
        return os.stat(path).st_dev
    except OSError:
        return None


def _check_scratch(
    bcfg: Mapping[str, Any], scfg: Mapping[str, Any], problems: list[str], checks: list[str]
) -> None:
    """Outputs are written (and timed) on scratch: same retention, labels and filesystems.

    Root paths may differ (each campaign has its own scratch directory), but every
    supplement root must be on the same filesystem (device) as the base root of the
    same label, and every dataset must use the same label.
    """
    bs, ss = bcfg.get("scratch") or {}, scfg.get("scratch") or {}
    found = len(problems)
    if bs.get("retain") != ss.get("retain"):
        problems.append(f"config.scratch.retain: {bs.get('retain')!r} -> {ss.get('retain')!r}")
    base_labels = {str(d["id"]): d.get("scratch") for d in bcfg.get("datasets") or []}
    for d in scfg.get("datasets") or []:
        if str(d["id"]) in base_labels and base_labels[str(d["id"])] != d.get("scratch"):
            problems.append(
                f"config.datasets[{d['id']}].scratch: {base_labels[str(d['id'])]!r} -> "
                f"{d.get('scratch')!r}"
            )
    base_roots = bs.get("roots") or {}
    for label, root in sorted((ss.get("roots") or {}).items()):
        if label not in base_roots:
            problems.append(f"config.scratch.roots: {label!r} is not a base scratch root")
            continue
        a, b = _device(str(base_roots[label])), _device(str(root))
        if a is None or b is None or a != b:
            problems.append(
                f"config.scratch.roots.{label}: {root} is not on the filesystem of "
                f"{base_roots[label]} (or either is missing)"
            )
    if len(problems) == found:
        checks.append("scratch on the same filesystem(s), same retention and dataset labels")


def check_supplement(base: CampaignStore, supp: CampaignStore) -> Supplement:
    """Verify that ``supp`` may fill gaps of ``base`` in one report.

    Raises:
        SupplementError: Every incompatibility found, in one message.
    """
    problems: list[str] = []
    checks: list[str] = []
    if supp.campaign_id == base.campaign_id:
        raise SupplementError(f"supplement {supp.campaign_id} is the base campaign itself")
    bc, sc = base.components, supp.components

    def compare(label: str, a: Any, b: Any, check: str) -> None:
        diffs = diff_records(a, b, label)
        if diffs:
            problems.extend(diffs)
        else:
            checks.append(check)

    compare(
        "environment",
        bc["environment"],
        sc["environment"],
        "identical environment (hardware, OS, kernel, Python)",
    )
    base_host, supp_host = _host(base), _host(supp)
    if base_host != supp_host:
        problems.append(f"provenance host {base_host!r} -> {supp_host!r}")
    else:
        checks.append(f"same host ({base_host})")

    bcfg, scfg = bc["config"], sc["config"]
    for key in CONFIG_EQUAL:
        compare(f"config.{key}", bcfg.get(key), scfg.get(key), f"identical {key}")
    compare(
        "config.scheduling",
        _without(bcfg.get("scheduling"), SCHEDULING_MAY_DIFFER),
        _without(scfg.get("scheduling"), SCHEDULING_MAY_DIFFER),
        "identical scheduling settings (except the seed)",
    )
    extra_cpus = sorted(set(scfg.get("cpu_counts") or []) - set(bcfg.get("cpu_counts") or []))
    if extra_cpus:
        problems.append(f"config.cpu_counts: {extra_cpus} not in the base campaign")
    _check_scratch(bcfg, scfg, problems, checks)

    base_tools = _by_id(bc["tools"]["tools"])
    for tool in sc["tools"]["tools"]:
        b = base_tools.get(str(tool["id"]))
        if b is None:
            problems.append(f"tools: {tool['id']!r} is not a base campaign tool")
            continue
        compare(
            f"tools[{tool['id']}]",
            _without(b, TOOL_RECORD_MAY_DIFFER),
            _without(tool, TOOL_RECORD_MAY_DIFFER),
            f"identical tool {tool['id']} {tool.get('version')} (options, executables, details)",
        )

    base_datasets = _by_id(bc["datasets"]["datasets"])
    for dataset in sc["datasets"]["datasets"]:
        b = base_datasets.get(str(dataset["id"]))
        if b is None:
            problems.append(f"datasets: {dataset['id']!r} is not a base campaign dataset")
            continue
        compare(
            f"datasets[{dataset['id']}]",
            b,
            dataset,
            f"identical dataset {dataset['id']} ({dataset.get('file_count')} files)",
        )

    base_workloads = _by_id(bc["workloads"]["workloads"], "name")
    for workload in sc["workloads"]["workloads"]:
        name = str(workload["name"])
        b = base_workloads.get(name)
        if b is None:
            problems.append(f"workloads: {name!r} is not a base campaign workload")
            continue
        extra = sorted(set(workload.get("datasets") or []) - set(b.get("datasets") or []))
        if extra:
            problems.append(f"workloads[{name}].datasets: {extra} not in the base campaign")
        contract = (workload.get("contract") or {}).get("digest")
        compare(
            f"workloads[{name}]",
            _without(b, ("datasets", "implementations")),
            _without(workload, ("datasets", "implementations")),
            f"identical workload {name} (contract {str(contract)[:12]})",
        )
        base_impl = b.get("implementations") or {}
        for tool_id, impl in sorted((workload.get("implementations") or {}).items()):
            diffs = diff_records(
                base_impl.get(tool_id), impl, f"workloads[{name}].implementations.{tool_id}"
            )
            problems.extend(diffs)

    outside = sorted(measurement_units(supp) - measurement_units(base))
    if outside:
        problems.append(
            f"{len(outside)} planned unit(s) are not planned in the base campaign, e.g. "
            f"{list(outside[0])}"
        )
    if problems:
        raise SupplementError(
            f"supplement {supp.campaign_id} is not compatible with base campaign "
            f"{base.campaign_id}: " + "; ".join(problems)
        )
    return Supplement(supp, checks, _code_changes(bc["code"], sc["code"]))


__all__ = [
    "SUPPLEMENT_SCHEMA",
    "Supplement",
    "SupplementError",
    "check_supplement",
    "measurement_units",
]
