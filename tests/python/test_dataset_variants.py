"""Format variants of a dataset: a tool reads its declared variant of a base dataset.

The base dataset defines the schedule units, the report groups and the comparisons;
the variant must hold the same tiles (identity checks point counts and header
bounds). Here the variant is a byte copy of the base tiles in another directory, and
two instances of the synthetic adapter read the base and the variant respectively.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
from campaign_helpers import make_runner, no_git, registry
from validation_fixtures import write_las

from lidarbench.campaign.identity import IdentityError, tile_key, variant_check
from lidarbench.campaign.preflight import run_preflight
from lidarbench.campaign.results import collect_rows
from lidarbench.campaign.schedule import campaign_schedule
from lidarbench.config.campaign import ConfigError, load_config
from lidarbench.datasets.fixtures import generate_synthetic_tiles
from lidarbench.reporting.dataset import ReportPolicy, build_campaign_dataset


def _layout(tmp_path: Path) -> Path:
    base = tmp_path / "bench root"
    for name in ("data", "scratch", "state"):
        (base / name).mkdir(parents=True)
    generate_synthetic_tiles(base / "data", {"points_per_tile": 20})
    (base / "data copc").mkdir()
    for tile in sorted((base / "data").glob("*.las")):
        shutil.copy2(tile, base / "data copc" / tile.name)
    return base


def _config(base: Path, *, variant: str = "", tools: str | None = None) -> Path:
    variant = variant or (
        '[[datasets]]\nid = "tiles-copc"\npath = "data copc"\ninclude = ["*.las"]\n'
        'variant_of = "tiles"\n'
    )
    tools = tools or (
        '[[tools]]\nid = "plain"\nadapter = "synthetic"\n'
        '[[tools]]\nid = "copc-reader"\nadapter = "synthetic"\n'
        'dataset_variants = { tiles = "tiles-copc" }\n'
    )
    text = "\n".join(
        [
            'schema = "lidarbench.campaign-config/1"',
            'campaign_id = "variants"',
            'workspace = "workspace"',
            "cpu_counts = [1]",
            'validation_mode = "performance-only"',
            "[scheduling]",
            "repetitions = 2\nwarmups = 0\ncooldown_s = 0",
            "[monitoring]",
            "enabled = false",
            "[scratch]",
            'roots = { local = "scratch" }',
            "[[datasets]]",
            'id = "tiles"',
            'path = "data"',
            'include = ["*.las"]',
            variant,
            '[[workloads]]\nid = "synthetic.checksum"\n',
            tools,
        ]
    )
    path = base / "campaign.toml"
    path.write_text(text, encoding="utf-8")
    return path


# -- configuration ------------------------------------------------------------------------


def test_variants_are_never_units_and_tools_read_their_variant(tmp_path):
    base = _layout(tmp_path)
    config = load_config(_config(base))
    assert config.dataset("tiles-copc").variant_of == "tiles"
    assert config.workload_datasets(config.workloads[0]) == ("tiles",)
    assert config.input_dataset("plain", "tiles") == "tiles"
    assert config.input_dataset("copc-reader", "tiles") == "tiles-copc"
    units = {(e.dataset, e.cpus) for e in campaign_schedule(config)}
    assert units == {("tiles", 1)}
    record = config.to_record()
    assert record["datasets"][1]["variant_of"] == "tiles"
    assert "variant_of" not in record["datasets"][0]  # base records are unchanged
    tools = {t["id"]: t for t in record["tools"]}
    assert tools["copc-reader"]["dataset_variants"] == {"tiles": "tiles-copc"}
    assert "dataset_variants" not in tools["plain"]  # unchanged for existing configs


@pytest.mark.parametrize(
    ("variant", "tools", "message"),
    [
        (
            '[[datasets]]\nid = "v"\npath = "data copc"\nvariant_of = "nope"\n',
            None,
            "variant_of must name a base dataset",
        ),
        (
            '[[datasets]]\nid = "v"\npath = "data copc"\nvariant_of = "tiles"\n'
            '[[datasets]]\nid = "w"\npath = "data copc"\nvariant_of = "v"\n',
            None,
            "variant_of must name a base dataset",
        ),
        (
            "",
            '[[tools]]\nid = "x"\nadapter = "synthetic"\n'
            'dataset_variants = { tiles = "tiles" }\n',
            "dataset_variants.tiles must name a dataset declared with variant_of",
        ),
        (
            "",
            '[[tools]]\nid = "x"\nadapter = "synthetic"\n'
            "dataset_variants = { tiles = [1] }\n",
            "dataset_variants.tiles",
        ),
    ],
)
def test_invalid_variant_declarations_name_the_key(tmp_path, variant, tools, message):
    base = _layout(tmp_path)
    with pytest.raises(ConfigError, match=message):
        load_config(_config(base, variant=variant, tools=tools))


def test_workloads_cannot_select_a_variant(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    path.write_text(
        path.read_text(encoding="utf-8").replace(
            'id = "synthetic.checksum"\n', 'id = "synthetic.checksum"\ndatasets = ["tiles-copc"]\n'
        ),
        encoding="utf-8",
    )
    with pytest.raises(ConfigError, match=r"datasets must list dataset ids from \['tiles'\]"):
        load_config(path)


# -- identity check -------------------------------------------------------------------------


def test_tile_keys_ignore_the_format_suffix():
    assert tile_key(Path("/d/298000_5093000.copc.laz")) == "298000_5093000"
    assert tile_key(Path("/d/298000_5093000.laz")) == "298000_5093000"
    assert tile_key(Path("/d/a.b.LAS")) == "a.b"


def test_variant_check_compares_tiles_counts_and_bounds(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    a.mkdir(), b.mkdir()
    write_las(a / "t1.las", [500000.5, 500010.0], [5000000.5, 5000010.0], [1.0, 2.0])
    write_las(b / "t1.copc.laz", [500010.0, 500000.5], [5000010.0, 5000000.5], [2.0, 1.0])
    check = variant_check([a / "t1.las"], [b / "t1.copc.laz"])
    assert check["tiles"] == 1 and "point_count" in check["compared"]
    write_las(b / "t2.las", [500000.5], [5000000.5], [1.0])
    with pytest.raises(IdentityError, match="only in variant"):
        variant_check([a / "t1.las"], [b / "t1.copc.laz", b / "t2.las"])
    write_las(b / "t1.las", [500000.5], [5000000.5], [1.0])
    with pytest.raises(IdentityError, match="2 != 1 points"):
        variant_check([a / "t1.las"], [b / "t1.las"])
    write_las(b / "t1.las", [500000.5, 500011.0], [5000000.5, 5000010.0], [1.0, 2.0])
    with pytest.raises(IdentityError, match=r"header bounds differ \(X\)"):
        variant_check([a / "t1.las"], [b / "t1.las"])


def test_preflight_checks_the_variant(tmp_path):
    base = _layout(tmp_path)
    report = run_preflight(
        _config(base),
        registry=registry(),
        state_dir=base / "state",
        git_info=no_git,
        cpu_sample_s=0,
        which=lambda name: f"/opt/bin/{name}",
    )
    item = next(i for i in report.items if i.id == "dataset.tiles-copc.variant")
    assert item.status == "ok" and "copc-reader" in item.message, item.message
    plans = next(i for i in report.items if i.id == "plan.copc-reader.synthetic.checksum")
    assert {p["input_dataset"] for p in plans.details["plans"]} == {"tiles-copc"}
    (base / "data copc" / sorted(p.name for p in (base / "data copc").iterdir())[0]).unlink()
    report = run_preflight(
        _config(base),
        registry=registry(),
        state_dir=base / "state",
        git_info=no_git,
        cpu_sample_s=0,
        which=lambda name: f"/opt/bin/{name}",
    )
    item = next(i for i in report.items if i.id == "dataset.tiles-copc.variant")
    assert item.status == "error" and "only in base" in item.message


# -- campaign and report ---------------------------------------------------------------------


def test_campaign_runs_each_tool_on_its_input_dataset_and_reports_them_together(tmp_path):
    base = _layout(tmp_path)
    summary = make_runner(_config(base), base).run()
    assert summary.statuses == {"succeeded": 4}, summary.to_record()
    identity = summary.campaign_path / "identity" / "datasets.json"
    assert '"variant_of": "tiles"' in identity.read_text(encoding="utf-8")
    rows = collect_rows(summary.campaign_path)
    assert {(r["tool"], r["dataset"], r["input_dataset"]) for r in rows} == {
        ("plain", "tiles", "tiles"),
        ("copc-reader", "tiles", "tiles-copc"),
    }
    for attempt in (summary.campaign_path / "attempts").iterdir():
        invocation = (attempt / "invocation.json").read_text(encoding="utf-8")
        reads_copc = "data copc" in invocation
        assert reads_copc == ('"tool": "copc-reader"' in (attempt / "started.json").read_text())

    dataset = build_campaign_dataset(summary.campaign_path, ReportPolicy("performance-only", 0.0))
    summary_rows = dataset.table("summary").rows
    assert {(r["tool"], r["dataset"], r["input_dataset"]) for r in summary_rows} == {
        ("plain", "tiles", "tiles"),
        ("copc-reader", "tiles", "tiles-copc"),
    }
    comparisons = dataset.table("comparisons").rows
    assert len(comparisons) == 1  # both tools in the same unit and report group
    for row in comparisons:
        assert "input datasets differ (declared format variants of 'tiles')" in (
            row["accepted_differences"]
        )
    variants = {r["dataset"]: r["variant_of"] for r in dataset.table("datasets").rows}
    assert variants == {"tiles": None, "tiles-copc": "tiles"}
