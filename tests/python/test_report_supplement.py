"""Guarded supplement campaigns in one report (reporting/supplement.py)."""

from __future__ import annotations

import io
import json
from pathlib import Path

import campaign_helpers
import pytest
from campaign_helpers import make_runner, write_config, write_layout

from lidarbench.reporting.commands import report_command
from lidarbench.reporting.dataset import ReportPolicy, build_campaign_dataset
from lidarbench.reporting.supplement import SupplementError

WORKLOADS = '[[workloads]]\nid = "synthetic.checksum"\n[[workloads]]\nid = "scripted.ok"\n'
TOOLS = '[[tools]]\nid = "synthetic"\n[[tools]]\nid = "scripted"\n'
SCHEDULING = "repetitions = 2\nwarmups = 0\ncooldown_s = 0"
FILLED = [("scripted.ok", "tiny", 1, "scripted"), ("scripted.ok", "tiny", 2, "scripted")]


def _run(base: Path, campaign_id: str, **kwargs) -> Path:
    kwargs = {
        "workloads": WORKLOADS,
        "tools": TOOLS,
        "cpu_counts": "[1, 2]",
        "scheduling": SCHEDULING,
        **kwargs,
    }
    config = write_config(base, campaign_id=campaign_id, name=f"{campaign_id}.toml", **kwargs)
    summary = make_runner(config, base).run()
    assert summary.exit_code in (0, 1), summary
    return base / "workspace" / "campaigns" / campaign_id


@pytest.fixture(scope="module")
def campaigns(tmp_path_factory: pytest.TempPathFactory) -> tuple[Path, Path]:
    """A base campaign whose scripted.ok attempts failed, and a supplement that ran them."""
    base = write_layout(tmp_path_factory.mktemp("supplement"))
    with pytest.MonkeyPatch.context() as mp:
        mp.setitem(campaign_helpers._SCRIPTS, "scripted.ok", "import sys; sys.exit(3)")
        base_campaign = _run(base, "base")
    return base_campaign, _run(base, "supp")


def test_supplement_fills_only_units_without_base_measurements(campaigns):
    base, supp = campaigns
    policy = ReportPolicy("performance-only", 0.0)
    plain = build_campaign_dataset(base, policy)
    gaps = {
        (r["workload"], r["dataset"], r["cpus"], r["tool"])
        for r in plain.tables["summary"].rows
        if r["n_included"] == 0 and r["workload"] == "scripted.ok"
    }
    assert gaps >= set(FILLED)

    dataset = build_campaign_dataset(base, policy, supplements=[supp])
    summary = {
        (r["workload"], r["dataset"], r["cpus"], r["tool"]): r
        for r in dataset.tables["summary"].rows
    }
    for unit in FILLED:
        assert summary[unit]["source_campaign"] == "supp"
        assert summary[unit]["n_included"] == 2
        assert summary[unit]["n_failed"] == 0
    reported = summary[("synthetic.checksum", "tiny", 1, "synthetic")]
    assert reported["source_campaign"] == "base" and reported["n_included"] == 2
    assert {r["campaign_id"] for r in dataset.tables["repetitions"].rows} == {"base", "supp"}

    supplemented = [r for r in dataset.tables["issues"].rows if r["kind"] == "supplemented"]
    assert [(r["workload"], r["dataset"], r["cpus"], r["tool"]) for r in supplemented] == FILLED
    # The base failures stay on record; consistency rows of the supplement add no issues.
    failed = [r for r in dataset.tables["issues"].rows if r["kind"] == "execution_failed"]
    assert failed and {r["campaign_id"] for r in failed} == {"base"}

    unused = [
        r
        for r in dataset.tables["attempts"].rows
        if r["campaign_id"] == "supp"
        and r["workload"] == "synthetic.checksum"
        and r["tool"] == "synthetic"
        and r["execution_status"] == "succeeded"
    ]
    assert unused and not any(r["report_included"] for r in unused)
    assert all(r["exclusion"].startswith("supplement not used") for r in unused)
    overlap = dataset.tables["supplement_overlap"].rows
    assert {(r["tool"], r["cpus"]) for r in overlap} == {("synthetic", 1)}  # single-threaded
    assert all(r["base_n"] == 2 and r["supplement_n"] == 2 for r in overlap)
    assert all(r["supplement_vs_base_median"] > 0 for r in overlap)

    record = dataset.campaign["supplements"][0]
    assert record["campaign_id"] == "supp" and record["code_changes"] == []
    assert [tuple(u) for u in record["filled_units"]] == FILLED
    assert any("identical tool scripted" in c for c in record["checks"])
    assert any(n.startswith("Supplemented report: 2 measurement unit(s)") for n in dataset.notes)
    assert not any("never combined into ratios" in n for n in dataset.notes)


def test_report_command_writes_the_supplement(campaigns, tmp_path):
    base, supp = campaigns
    out, err = io.StringIO(), io.StringIO()
    target = tmp_path / "report"
    code = report_command(
        str(base),
        output=str(target),
        mode="performance-only",
        min_seconds=0.0,
        supplements=[str(supp)],
        render="never",
        out=out,
        err=err,
    )
    assert code == 0, err.getvalue()
    summary = (target / "summary.md").read_text(encoding="utf-8")
    assert "## Supplement campaign `supp`" in summary
    assert "Supplement consistency" in summary
    receipt = json.loads((target / "report.json").read_text(encoding="utf-8"))
    assert receipt["supplement_records"][0]["path"] == str(supp.resolve())
    assert receipt["campaign"]["supplements"][0]["campaign_id"] == "supp"
    assert (target / "tables" / "supplement_overlap.csv").is_file()
    rows = (target / "tables" / "results-table.jsonl").read_text(encoding="utf-8").splitlines()
    assert {json.loads(r)["campaign_id"] for r in rows} == {"base", "supp"}


@pytest.mark.parametrize(
    ("kwargs", "fragment"),
    [
        ({"scheduling": "repetitions = 1\nwarmups = 0\ncooldown_s = 0"}, "config.scheduling"),
        ({"cpu_counts": "[3]"}, "config.cpu_counts"),
        (
            {"workloads": '[[workloads]]\nid = "scripted.nooutput"\n'},
            "not a base campaign workload",
        ),
        ({"tools": '[[tools]]\nid = "other"\nadapter = "synthetic"\n'}, "not a base campaign tool"),
        ({"monitoring": "enabled = true\nrequired = false"}, "config.monitoring"),
    ],
)
def test_incompatible_supplements_are_refused(campaigns, tmp_path_factory, kwargs, fragment):
    base, _ = campaigns
    root = base.parent.parent.parent
    name = "bad-" + str(abs(hash(fragment)))[:8]
    bad = _run(root, name, **kwargs)
    with pytest.raises(SupplementError, match=fragment.replace("[", r"\[")):
        build_campaign_dataset(base, ReportPolicy("performance-only", 0.0), supplements=[bad])
    err = io.StringIO()
    code = report_command(
        str(base),
        output=str(tmp_path_factory.mktemp("bad") / "r"),
        mode="performance-only",
        min_seconds=0.0,
        supplements=[str(bad)],
        render="never",
        out=io.StringIO(),
        err=err,
    )
    assert code == 1 and "not compatible" in err.getvalue()


def test_a_campaign_cannot_supplement_itself(campaigns):
    base, _ = campaigns
    with pytest.raises(SupplementError, match="base campaign itself"):
        build_campaign_dataset(base, ReportPolicy("performance-only", 0.0), supplements=[base])


def test_supplement_needs_a_base_campaign(tmp_path):
    err = io.StringIO()
    code = report_command(
        None,
        output=str(tmp_path / "r"),
        supplements=[str(tmp_path)],
        historical=[str(tmp_path)],
        out=io.StringIO(),
        err=err,
    )
    assert code == 2 and "--supplement needs a base campaign" in err.getvalue()


def test_scratch_must_match_retention_labels_and_filesystem(tmp_path):
    from lidarbench.reporting.supplement import _check_scratch

    (tmp_path / "a").mkdir()
    (tmp_path / "b").mkdir()

    def cfg(root: str, retain: str = "on-failure", label: str = "ssd") -> dict:
        return {
            "scratch": {"retain": retain, "roots": {label: str(tmp_path / root)}},
            "datasets": [{"id": "d", "scratch": label}],
        }

    problems: list[str] = []
    checks: list[str] = []
    _check_scratch(cfg("a"), cfg("b"), problems, checks)
    assert problems == [] and checks  # other directory, same filesystem
    for supp, fragment in (
        (cfg("b", retain="always"), "scratch.retain"),
        (cfg("b", label="other"), "datasets[d].scratch"),
        (cfg("missing"), "not on the filesystem"),
    ):
        problems = []
        _check_scratch(cfg("a"), supp, problems, [])
        assert any(fragment in p for p in problems), (fragment, problems)
