"""Tests for workload contracts (lidarbench.workload/1)."""

from __future__ import annotations

import copy
import tomllib
from pathlib import Path

import pytest

from lidarbench.execution.adapters.base import ImplementationStatus
from lidarbench.workloads import (
    ContractError,
    default_suite_dir,
    load_contract,
    load_suite,
    parse_contract,
)

SUITE = default_suite_dir()


def _data(name: str = "pixel_metrics_1a") -> dict:
    with open(SUITE / f"{name}.toml", "rb") as handle:
        return tomllib.load(handle)


def test_suite_defines_the_two_initial_workloads():
    suite = load_suite(SUITE)
    assert suite.name == "workloads-v2"
    assert sorted(suite.contracts) == ["generate_DSM1", "pixel_metrics_1a"]
    mean = suite.get("pixel_metrics_1a")
    assert (mean.statistic, mean.raster.resolution, mean.raster.bands) == (
        "mean",
        20.0,
        ("z_mean",),
    )
    dsm = suite.get("generate_DSM1")
    assert (dsm.statistic, dsm.raster.resolution, dsm.raster.bands) == ("max", 1.0, ())
    for contract in suite.contracts.values():
        assert contract.buffer == 20.0
        assert contract.raster.cell_ownership == "right_down"
        assert contract.tolerance.absolute == 2e-6 and contract.tolerance.relative == 2e-7
        assert contract.implementation("lidR").cell_ownership == "lidr_buffered_clamp"
        assert contract.implementation("lasR").cell_ownership == "right_down"
        lastools = contract.implementation("lastools")
        assert lastools.equivalence == "non_comparable"
        assert not lastools.ownership_declared
        assert contract.implementation("pdal").cell_ownership == "pdal_binmode_floor"


def test_frozen_v1_suite_has_no_pdal():
    """workloads-v1 stays loadable and unchanged; PDAL exists only from workloads-v2."""
    v1 = load_suite(default_suite_dir("workloads-v1"))
    assert v1.name == "workloads-v1"
    for contract in v1.contracts.values():
        assert sorted(contract.implementations) == ["lasR", "lastools", "lidR"]
        assert contract.equivalence_group == f"workloads-v1/{contract.id}"


def test_matrix_matches_workloads_md():
    """Statuses mirror benchmark_workload_contract() for these two workloads."""
    suite = load_suite(SUITE)
    for workload in ("pixel_metrics_1a", "generate_DSM1"):
        contract = suite.get(workload)
        assert {t: i.status for t, i in contract.implementations.items()} == dict.fromkeys(
            ("lidR", "lasR", "lastools", "pdal"), ImplementationStatus.RUNNABLE
        )
        assert {
            t for t, i in contract.implementations.items() if i.equivalence == "matched_definition"
        } == {"lidR", "lasR", "pdal"}
    assert suite.get("pixel_metrics_1a").implementation("lastools").artifacts == ("{stem}_avg.tif",)


def test_expected_names_and_sidecars():
    contract = load_contract(SUITE / "pixel_metrics_1a.toml")
    assert contract.implementation("lastools").expected_names("t 1") == ("t 1_avg.tif",)
    assert contract.is_ignored_sidecar("west.tif.aux.xml")
    assert contract.is_ignored_sidecar("pixel_metrics.vrt")
    assert not contract.is_ignored_sidecar("west.tif")
    assert contract.implementation("PDAL") is None


def test_digest_is_stable_and_content_based():
    a = parse_contract(_data())
    b = parse_contract(_data())
    assert a.digest == b.digest
    assert parse_contract(a.record).digest == a.digest  # the record round-trips
    changed = _data()
    changed["tolerance"]["relative"] = 3e-7
    assert parse_contract(changed).digest != a.digest


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (lambda d: d.update(extra=1), "unknown key"),
        (lambda d: d["raster"].update(resolution=20), "raster.resolution must be a finite float"),
        (lambda d: d["raster"].update(resolution=-20.0), "raster.resolution must be > 0"),
        (lambda d: d["raster"].update(origin=[1.0, 0.0]), "raster.origin"),
        (lambda d: d["raster"].update(cell_ownership="nearest"), "raster.cell_ownership"),
        (lambda d: d["raster"].update(cell_ownership="undeclared"), "declared convention"),
        (lambda d: d["raster"].update(bands=["a", "a"]), "distinct"),
        (lambda d: d["raster"].update(bands=["a", "b"]), "exactly one band"),
        (lambda d: d["spatial"].update(buffer=5.0), "spatial.buffer must be >="),
        (lambda d: d["population"].update(points="first"), "population.points"),
        (lambda d: d["population"].update(require_finite_xyz=False), "require_finite_xyz"),
        (lambda d: d["reference"].update(statistic="median"), "reference.statistic"),
        (lambda d: d["tolerance"].update(absolute="2e-6"), "tolerance.absolute"),
        (lambda d: d.update(schema="lidarbench.workload/2"), "schema"),
        (lambda d: d.pop("tolerance"), "missing required key"),
        (lambda d: d["implementations"]["lidR"].update(status="blocked"), "reason is required"),
        (lambda d: d["implementations"]["lidR"].update(artifacts=["out.tif"]), "{stem}"),
        (lambda d: d["implementations"]["lidR"].update(artifacts=["{stem}*.tif"]), "exact name"),
        (lambda d: d["implementations"]["lidR"].update(artifacts=["sub/{stem}.tif"]), "file name"),
        (lambda d: d["implementations"]["lidR"].pop("artifacts"), "artifacts is required"),
        (lambda d: d["implementations"]["lidR"].update(equivalence="same"), "equivalence"),
        (
            lambda d: d["implementations"]["lidR"].update(status="diagnostic_only", reason="x"),
            "must be non_comparable",
        ),
        (lambda d: d["implementations"].update(LIDR=d["implementations"]["lidR"]), "case"),
        (lambda d: d.update(implementations={}), "non-empty"),
    ],
)
def test_invalid_contracts_name_the_key(mutate, message):
    data = copy.deepcopy(_data())
    mutate(data)
    with pytest.raises(ContractError, match=message.replace("{", r"\{").replace("}", r"\}")):
        parse_contract(data)


def test_blocked_implementation_needs_no_artifacts():
    data = _data()
    data["implementations"]["lasR"] = {
        "status": "blocked",
        "equivalence": "non_comparable",
        "reason": "known defect",
    }
    contract = parse_contract(data)
    assert contract.implementation("lasR").artifacts == ()
    assert not contract.implementation("lasR").status.dispatchable


def test_file_name_and_suite_must_match(tmp_path: Path):
    text = (SUITE / "pixel_metrics_1a.toml").read_text(encoding="utf-8")
    wrong = tmp_path / "workloads-v1" / "other.toml"
    wrong.parent.mkdir()
    wrong.write_text(text, encoding="utf-8")
    with pytest.raises(ContractError, match="file name must equal"):
        load_contract(wrong)
    suite = tmp_path / "workloads-v9"
    suite.mkdir()
    (suite / "pixel_metrics_1a.toml").write_text(text, encoding="utf-8")
    with pytest.raises(ContractError, match="suite"):
        load_suite(suite)


def test_suite_errors(tmp_path: Path):
    with pytest.raises(ContractError, match="not found"):
        load_suite(tmp_path / "missing")
    with pytest.raises(ContractError, match="no contracts"):
        load_suite(tmp_path)
    bad = tmp_path / "bad.toml"
    bad.write_text("schema = [", encoding="utf-8")
    with pytest.raises(ContractError, match="invalid TOML"):
        load_contract(bad)
    with pytest.raises(ContractError, match="defines no contract"):
        load_suite(SUITE).get("normalization")
