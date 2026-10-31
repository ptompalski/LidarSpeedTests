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


def test_suite_defines_the_cell_statistic_workloads_the_dem_and_normalization():
    suite = load_suite(SUITE)
    assert suite.name == "workloads-v7"
    assert sorted(suite.contracts) == [
        "generate_DEM",
        "generate_DSM1",
        "normalization",
        "pixel_metrics_1a",
    ]
    mean = suite.get("pixel_metrics_1a")
    assert (mean.statistic, mean.raster.resolution, mean.raster.bands) == (
        "mean",
        20.0,
        ("z_mean",),
    )
    dsm = suite.get("generate_DSM1")
    assert (dsm.statistic, dsm.raster.resolution, dsm.raster.bands) == ("max", 1.0, ())
    for contract in (c for c in suite.contracts.values() if c.has_reference):
        assert contract.buffer == 20.0
        assert contract.raster.cell_ownership == "right_down"
        assert contract.tolerance.absolute == 2e-6 and contract.tolerance.relative == 2e-7
        assert contract.implementation("lidR").cell_ownership == "lidr_buffered_clamp"
        assert contract.implementation("lasR").cell_ownership == "right_down"
        lastools = contract.implementation("lastools")
        assert lastools.equivalence == "matched_definition"
        assert lastools.cell_ownership == "pdal_binmode_floor"
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
        # From workloads-v4 the LAStools convention is declared (non_comparable up to v3).
        assert {
            t for t, i in contract.implementations.items() if i.equivalence == "matched_definition"
        } == {"lidR", "lasR", "pdal", "lastools"}
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
        load_suite(SUITE).get("tree_detection")


def test_v3_differs_from_v2_only_by_tfw_sidecars():
    """workloads-v3 = workloads-v2 + LAStools ``.tfw`` world files as ignored sidecars."""
    v2 = load_suite(default_suite_dir("workloads-v2"))
    v3 = load_suite(default_suite_dir("workloads-v3"))
    assert sorted(v2.contracts) == sorted(v3.contracts)
    for workload_id, old in v2.contracts.items():
        new = v3.get(workload_id)
        assert new.ignored_sidecars == (*old.ignored_sidecars, "*.tfw")
        assert new.equivalence_group == f"workloads-v3/{workload_id}"
        assert new.implementations == old.implementations
        assert new.raster == old.raster and new.tolerance == old.tolerance
        assert new.digest != old.digest
        assert new.is_ignored_sidecar("298000_5093000_avg.tfw")


def test_v4_declares_lastools_convention_and_precision_only():
    """workloads-v4 = workloads-v3 + the LAStools cell rule and centimetre precision (mean)."""
    v3 = load_suite(default_suite_dir("workloads-v3"))
    v4 = load_suite(default_suite_dir("workloads-v4"))
    for workload_id, old in v3.contracts.items():
        new = v4.get(workload_id)
        assert new.equivalence_group == f"workloads-v4/{workload_id}"
        assert new.raster == old.raster and new.tolerance == old.tolerance
        assert new.ignored_sidecars == old.ignored_sidecars
        for tool in ("lidR", "lasR", "pdal"):
            assert new.implementation(tool) == old.implementation(tool)
        assert old.declared_comparability("lastools") == "non_comparable"
        assert new.declared_comparability("lastools") == "functionally_comparable"
    assert v4.get("pixel_metrics_1a").implementation("lastools").value_precision == 0.01
    assert v4.get("generate_DSM1").implementation("lastools").value_precision is None


def test_v5_declares_lidr_extent_rule_only():
    """workloads-v5 = workloads-v4 + the lidR extent rule (terra::crop snap = "near")."""
    v4 = load_suite(default_suite_dir("workloads-v4"))
    v5 = load_suite(default_suite_dir("workloads-v5"))
    for workload_id, old in v4.contracts.items():
        new = v5.get(workload_id)
        assert new.equivalence_group == f"workloads-v5/{workload_id}"
        assert new.raster == old.raster and new.tolerance == old.tolerance
        assert new.ignored_sidecars == old.ignored_sidecars
        for tool in ("lasR", "lastools", "pdal"):
            assert new.implementation(tool) == old.implementation(tool)
        assert old.implementation("lidR").extent is None
        assert new.implementation("lidR").extent == "header_bounds_snapped_nearest"
        assert new.digest != old.digest
        assert "extent" not in old.record["implementations"]["lidR"]


def test_v6_keeps_the_v5_definitions_and_adds_a_structural_only_dem():
    v5 = load_suite(default_suite_dir("workloads-v5"))
    v6 = load_suite(default_suite_dir("workloads-v6"))
    for workload_id, old in v5.contracts.items():
        new, old_record = dict(v6.get(workload_id).record), dict(old.record)
        for record in (new, old_record):
            record.pop("suite")
            record.pop("equivalence")
        assert new == old_record
    dem = v6.get("generate_DEM")
    assert not dem.has_reference and dem.statistic == ""
    assert dem.population.points == "classes" and dem.population.classes == (2, 9)
    assert dem.raster.cell_ownership == "cell_centre_sample" and dem.raster.resolution == 1.0
    assert "tolerance" not in dem.record and dem.record["reference"] == {"method": "none"}
    assert dem.is_ignored_sidecar("298000_5093000.kml")
    assert {t: dem.declared_comparability(t) for t in dem.implementations} == {
        "lasR": "non_comparable",
        "lastools": "non_comparable",
        "lidR": "non_comparable",
        "pdal": "non_comparable",
    }
    assert dem.implementation("lidR").extent == "header_bounds_snapped_nearest"


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (lambda d: d["population"].pop("classes"), "population.classes is required"),
        (lambda d: d["population"].update(classes=[9, 2]), "sorted, distinct"),
        (lambda d: d["population"].update(classes=[]), "sorted, distinct"),
        (lambda d: d["population"].update(classes=[2, 2]), "sorted, distinct"),
        (lambda d: d["population"].update(classes=[2, 256]), r"classes\[1\]"),
        (lambda d: d["population"].update(classes=[True]), r"classes\[0\]"),
        (lambda d: d["population"].update(points="all"), "only allowed when"),
        (lambda d: d["reference"].update(statistic="max"), "not allowed when method"),
        (lambda d: d.update(tolerance={"absolute": 0.0, "relative": 0.0}), "tolerance is not"),
        (lambda d: d["reference"].update(method="cell_statistic"), "statistic"),
    ],
)
def test_invalid_structural_only_contracts_name_the_key(mutate, message):
    data = copy.deepcopy(_data("generate_DEM"))
    mutate(data)
    with pytest.raises(ContractError, match=message):
        parse_contract(data)


def test_cell_statistic_needs_all_points_and_a_tolerance():
    data = copy.deepcopy(_data())
    data["population"].update(points="classes", classes=[2])
    with pytest.raises(ContractError, match=r'require population\.points = "all"'):
        parse_contract(data)
    data = copy.deepcopy(_data())
    data["reference"].pop("statistic")
    with pytest.raises(ContractError, match="statistic"):
        parse_contract(data)


def test_implementation_extent_rule_is_validated():
    data = _data()
    data["implementations"]["lasR"]["extent"] = "anything"
    with pytest.raises(ContractError, match="extent"):
        parse_contract(data)
    data["implementations"]["lasR"]["extent"] = "header_bounds_snapped_nearest"
    assert parse_contract(data).implementation("lasR").extent == "header_bounds_snapped_nearest"
    data["implementations"]["lastools"]["cell_ownership"] = "undeclared"
    data["implementations"]["lastools"]["equivalence"] = "non_comparable"
    data["implementations"]["lastools"]["extent"] = "header_bounds_snapped_nearest"
    with pytest.raises(ContractError, match="requires a declared cell_ownership"):
        parse_contract(data)


def test_value_precision_must_be_a_positive_float():
    data = _data()
    data["implementations"]["lastools"]["value_precision"] = 0
    with pytest.raises(ContractError, match="value_precision"):
        parse_contract(data)
    data["implementations"]["lastools"]["value_precision"] = 0.01
    assert parse_contract(data).implementation("lastools").value_precision == 0.01
    assert "value_precision" in parse_contract(data).record["implementations"]["lastools"]


def test_v7_keeps_the_v6_definitions_and_adds_point_cloud_normalization():
    v6 = load_suite(default_suite_dir("workloads-v6"))
    v7 = load_suite(default_suite_dir("workloads-v7"))
    for workload_id, old in v6.contracts.items():
        new, old_record = dict(v7.get(workload_id).record), dict(old.record)
        for record in (new, old_record):
            record.pop("suite")
            record.pop("equivalence")
        assert new == old_record
    norm = v7.get("normalization")
    assert norm.product == "point_cloud" and norm.raster is None
    assert "raster" not in norm.record and norm.record["reference"] == {"method": "none"}
    assert not norm.has_reference and norm.buffer == 20.0
    pc = norm.point_cloud
    assert pc is not None
    assert (pc.format, pc.z, pc.ground_classes, pc.surface, pc.ownership) == (
        "laz",
        "height_above_ground",
        (2, 9),
        "linear_tin",
        "closed_header_bounds",
    )
    assert {"x", "y", "gps_time", "classification"} <= set(pc.preserved_attributes)
    assert {t: (i.status, norm.declared_comparability(t)) for t, i in norm.implementations.items()}[
        "lasR"
    ] == (ImplementationStatus.DIAGNOSTIC_ONLY, "non_comparable")
    assert {norm.declared_comparability(t) for t in norm.implementations} == {"non_comparable"}
    assert norm.implementation("lidR").expected_names("t") == ("t.laz",)
    assert norm.implementation("lidR").outside_surface == "extrapolate"
    assert norm.implementation("lasR").outside_surface == "discard"
    assert "cell_ownership" not in norm.record["implementations"]["lidR"]
    with pytest.raises(ContractError, match="no raster product"):
        norm.require_raster()


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (lambda d: d.update(raster={}), "exactly one product table"),
        (lambda d: d.pop("point_cloud"), "exactly one product table"),
        (lambda d: d["point_cloud"].update(format="las"), "point_cloud.format"),
        (lambda d: d["point_cloud"].update(z="original"), "point_cloud.z"),
        (lambda d: d["point_cloud"].update(ground_classes=[9, 2]), "sorted, distinct"),
        (lambda d: d["point_cloud"].update(ground_classes=[300]), r"ground_classes\[0\]"),
        (lambda d: d["point_cloud"].update(preserved_attributes=["x"]), "include x and y"),
        (lambda d: d["point_cloud"].update(preserved_attributes=["x", "y", "rgb"]), "rgb"),
        (lambda d: d["point_cloud"].update(max_missing_fraction=1.0), "< 1.0"),
        (lambda d: d["point_cloud"].update(max_missing_fraction=0), "finite float"),
        (lambda d: d["point_cloud"].pop("surface"), "surface"),
        (lambda d: d["point_cloud"].update(extra=1), "unknown key"),
        (
            lambda d: d["population"].update(points="classes", classes=[2]),
            'population.points = "all"',
        ),
        (lambda d: d["spatial"].update(buffer=0.0), "buffer must be > 0"),
        (lambda d: d["reference"].update(method="cell_statistic", statistic="max"), "none"),
        (
            lambda d: d["implementations"]["lidR"].update(cell_ownership="right_down"),
            "unknown key",
        ),
        (lambda d: d["implementations"]["lidR"].pop("outside_surface"), "outside_surface"),
        (
            lambda d: d["implementations"]["lidR"].update(outside_surface="keep"),
            "outside_surface",
        ),
        (
            lambda d: d["implementations"]["lidR"].update(equivalence="matched_definition"),
            "must be non_comparable",
        ),
    ],
)
def test_invalid_point_cloud_contracts_name_the_key(mutate, message):
    data = copy.deepcopy(_data("normalization"))
    mutate(data)
    with pytest.raises(ContractError, match=message):
        parse_contract(data)


def test_raster_contracts_reject_point_cloud_implementation_keys():
    data = _data()
    data["implementations"]["lidR"]["outside_surface"] = "discard"
    with pytest.raises(ContractError, match="unknown key"):
        parse_contract(data)
