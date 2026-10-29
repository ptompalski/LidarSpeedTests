"""Tests for ``lidarbench preflight`` (read-only campaign readiness check)."""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path
from typing import Any

import pytest
from campaign_helpers import make_runner, no_git, registry
from dataset_helpers import copy_repo_recipes

from lidarbench import cli
from lidarbench.campaign.commands import preflight_command
from lidarbench.campaign.preflight import (
    PREFLIGHT_SCHEMA,
    STATUSES,
    CheckItem,
    PreflightReport,
    render_report,
    run_preflight,
)
from lidarbench.core.locks import MACHINE_LOCK_NAME, WORKSPACE_LOCK_NAME, ExclusiveLock
from lidarbench.core.platform import PlatformInfo
from lidarbench.datasets.fixtures import generate_synthetic_tiles
from lidarbench.execution.adapters.base import (
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    WorkloadSupport,
)
from lidarbench.validation.io import BackendStatus


class FakeLasRAdapter(ToolAdapter):
    """Stands in for the lasR adapter (``.lax`` rule, contract tool name)."""

    adapter_id = "lasr"
    display_name = "Fake lasR"
    contract_tool = "lasR"

    def probe(self, platform_info: PlatformInfo) -> Availability:
        return Availability.ok({"python": Path(sys.executable)})

    def identify(self) -> ToolIdentity:
        return ToolIdentity("lasR", "0.0-test", {"python": Path(sys.executable)})

    def workload_support(self) -> list[WorkloadSupport]:
        threads = ThreadingSupport(ThreadingModel.NATIVE_THREADS, 1, None)
        return [
            WorkloadSupport("pixel_metrics_1a", ImplementationStatus.RUNNABLE, threading=threads)
        ]

    def build_command(self, invocation: Invocation) -> CommandSpec:
        return CommandSpec(argv=(sys.executable, "-c", "pass"), cwd=invocation.work_dir)

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        return [ArtifactSpec(invocation.output_dir / "metrics.tif", ArtifactKind.RASTER)]


def _registry() -> Any:
    reg = registry()
    reg.register(FakeLasRAdapter)
    return reg


SYNTHETIC_TOOL = '[[tools]]\nid = "synthetic"\n'
SYNTHETIC_WORKLOAD = '[[workloads]]\nid = "synthetic.checksum"\n'


def _layout(tmp_path: Path) -> Path:
    base = tmp_path / "bench root"
    for name in ("data", "scratch", "state"):
        (base / name).mkdir(parents=True)
    generate_synthetic_tiles(base / "data", {"points_per_tile": 20})
    return base


def _config(
    base: Path,
    *,
    tools: str = SYNTHETIC_TOOL,
    workloads: str = SYNTHETIC_WORKLOAD,
    cpu_counts: str = "[1]",
    scratch: str = '"scratch"',
    validation_mode: str = "performance-only",
    dataset: str = 'path = "data"\ninclude = ["*.las"]',
    extra: str = "",
    campaign_id: str = "preflight-test",
) -> Path:
    text = "\n".join(
        [
            'schema = "lidarbench.campaign-config/1"',
            f'campaign_id = "{campaign_id}"',
            'workspace = "workspace"',
            f"cpu_counts = {cpu_counts}",
            f'validation_mode = "{validation_mode}"',
            "[scheduling]",
            "repetitions = 1\nwarmups = 0\ncooldown_s = 0",
            "[monitoring]",
            "enabled = false",
            "[scratch]",
            f"roots = {{ local = {scratch} }}",
            extra,
            "[[datasets]]",
            'id = "tiles"',
            dataset,
            "",
        ]
    )
    path = base / "campaign.toml"
    path.write_text(text + workloads + "\n" + tools, encoding="utf-8")
    return path


def _preflight(path: Path, base: Path, **kwargs: Any) -> PreflightReport:
    kwargs.setdefault("registry", _registry())
    kwargs.setdefault("state_dir", base / "state")
    kwargs.setdefault("git_info", no_git)
    kwargs.setdefault("cpu_sample_s", 0)
    kwargs.setdefault("which", lambda name: f"/opt/bin/{name}")
    return run_preflight(path, **kwargs)


def _item(report: PreflightReport, check_id: str) -> CheckItem:
    matches = [i for i in report.items if i.id == check_id]
    assert matches, f"{check_id} not in {[i.id for i in report.items]}"
    return matches[0]


def _tree(path: Path) -> list[str]:
    return sorted(os.fspath(p.relative_to(path)) for p in path.rglob("*"))


def test_ready_synthetic_campaign_writes_nothing(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    before = _tree(base)
    report = _preflight(path, base)
    assert report.ready and report.exit_code == 0, render_report(report)
    assert _tree(base) == before
    assert not (base / "workspace").exists()
    ids = [i.id for i in report.items]
    for expected in (
        "config",
        "code.git",
        "workspace",
        "lock.workspace",
        "lock.machine",
        "dataset.tiles",
        "dataset.tiles.headers",
        "dataset.tiles.protection",
        "scratch.local",
        "tool.synthetic",
        "plan.synthetic.synthetic.checksum",
        "campaign",
        "cpu.counts",
        "cpu.affinity",
        "cpu.load",
        "memory",
        "monitoring",
        "validation.suite",
        "reporting.quarto",
        "reporting.rscript",
    ):
        assert expected in ids
    assert "dataset.tiles.lax" not in ids  # only checked when lasR is configured
    headers = _item(report, "dataset.tiles.headers")
    assert headers.status == "ok" and headers.details["points"] == 40
    assert _item(report, "plan.synthetic.synthetic.checksum").status == "ok"
    assert _item(report, "campaign").message.startswith("new campaign")
    assert _item(report, "workspace").message.endswith("will be created by `run`")
    assert _item(report, "monitoring").status == "skipped"


def test_config_error_exit_code_and_message(tmp_path):
    base = _layout(tmp_path)
    path = _config(base, extra="unknown_key = 1")
    report = _preflight(path, base)
    assert report.exit_code == 1
    assert [(i.id, i.status) for i in report.items] == [("config", "error")]
    assert "unknown key" in report.items[0].message

    out, err = _Capture(), _Capture()
    code = preflight_command(os.fspath(base / "missing.toml"), out=out, err=err)
    assert code == 1
    assert "cannot read campaign config" in err.text


class _Capture:
    def __init__(self) -> None:
        self.text = ""

    def write(self, text: str) -> int:
        self.text += text
        return len(text)


def test_json_record_schema(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    out = _Capture()
    code = preflight_command(
        os.fspath(path),
        as_json=True,
        out=out,
        err=_Capture(),
        registry=_registry(),
        state_dir=base / "state",
        git_info=no_git,
        cpu_sample_s=0,
    )
    record = json.loads(out.text)
    assert code == record["exit_code"] == 0
    assert record["schema"] == PREFLIGHT_SCHEMA
    assert record["ready"] is True
    assert record["campaign_id"] == "preflight-test"
    assert record["full_hash"] is False
    assert set(record["counts"]) == set(STATUSES)
    assert sum(record["counts"].values()) == len(record["checks"])
    for check in record["checks"]:
        assert set(check) == {"id", "group", "status", "message", "details"}
        assert check["status"] in STATUSES
        assert check["group"] == check["id"].split(".")[0]


def test_check_item_rejects_unknown_status():
    with pytest.raises(ValueError):
        CheckItem("x", "fine", "message")


def test_active_machine_lock_is_error_and_stale_workspace_lock_warns(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    (base / "workspace").mkdir()
    (base / "workspace" / WORKSPACE_LOCK_NAME).write_text(
        json.dumps(
            {
                "host": __import__("socket").gethostname(),
                "pid": os.getpid(),
                "process_create_time": 1.0,
                "purpose": "workspace",
            }
        ),
        encoding="utf-8",
    )
    lock = ExclusiveLock(base / "state" / MACHINE_LOCK_NAME, "benchmark-execution")
    lock.acquire()
    try:
        report = _preflight(path, base)
    finally:
        lock.release()
    machine = _item(report, "lock.machine")
    assert machine.status == "error" and "active" in machine.message
    workspace = _item(report, "lock.workspace")
    assert workspace.status == "warning"
    assert "lock release --confirm" in workspace.message
    assert (base / "workspace" / WORKSPACE_LOCK_NAME).exists()
    assert report.exit_code == 1


def test_scratch_root_overlapping_dataset_is_error(tmp_path):
    base = _layout(tmp_path)
    path = _config(base, scratch='"data"')
    report = _preflight(path, base)
    item = _item(report, "scratch.local")
    assert item.status == "error" and "overlaps" in item.message
    assert report.exit_code == 1


def test_missing_scratch_root_is_error_with_hint(tmp_path):
    base = _layout(tmp_path)
    path = _config(base, scratch='"not-there"')
    report = _preflight(path, base)
    item = _item(report, "scratch.local")
    assert item.status == "error"
    assert "mkdir" in item.message and item.details["creatable"] is True
    assert not (base / "not-there").exists()


def test_scratch_low_disk_space_warns(tmp_path, monkeypatch):
    import shutil as shutil_module

    base = _layout(tmp_path)
    path = _config(base)
    usage = shutil_module.disk_usage(base)
    monkeypatch.setattr(
        "lidarbench.campaign.preflight.shutil.disk_usage",
        lambda p: type(usage)(usage.total, usage.used, 1024),
    )
    item = _item(_preflight(path, base), "scratch.local")
    assert item.status == "warning" and "free" in item.message


def test_workspace_overlapping_dataset_is_error(tmp_path):
    base = _layout(tmp_path)
    path = _config(base, dataset='path = "."\ninclude = ["*.toml"]')
    report = _preflight(path, base)
    assert _item(report, "workspace").status == "error"
    assert _item(report, "dataset.tiles.headers").status == "skipped"


def test_lasr_requires_lax_indexes(tmp_path):
    base = _layout(tmp_path)
    tools = SYNTHETIC_TOOL + '[[tools]]\nid = "lasr"\n'
    path = _config(base, tools=tools)
    report = _preflight(path, base)
    lax = _item(report, "dataset.tiles.lax")
    assert lax.status == "error" and len(lax.details["missing"]) == 2
    assert report.exit_code == 1
    for tile in (base / "data").glob("*.las"):
        tile.with_suffix(".lax").write_bytes(b"index")
    report = _preflight(path, base)
    assert _item(report, "dataset.tiles.lax").status == "ok"
    assert report.exit_code == 0, render_report(report)


def test_cpu_count_above_available_is_error(tmp_path):
    base = _layout(tmp_path)
    path = _config(base, cpu_counts="[1, 4096]")
    report = _preflight(path, base)
    item = _item(report, "cpu.counts")
    assert item.status == "error" and "4096" in item.message
    assert report.exit_code == 1


def test_affinity_none_is_skipped(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    text = path.read_text(encoding="utf-8").replace(
        "cooldown_s = 0", 'cooldown_s = 0\ncpu_affinity = "none"'
    )
    path.write_text(text, encoding="utf-8")
    assert _item(_preflight(path, base), "cpu.affinity").status == "skipped"


@pytest.mark.skipif(sys.platform != "darwin", reason="macOS-only affinity warning")
def test_affinity_pinned_warns_on_macos(tmp_path):
    base = _layout(tmp_path)
    item = _item(_preflight(_config(base), base), "cpu.affinity")
    assert item.status == "warning" and "macOS" in item.message


@pytest.mark.skipif(sys.platform not in ("linux", "win32"), reason="pinning supported here")
def test_affinity_pinned_supported(tmp_path):
    base = _layout(tmp_path)
    assert _item(_preflight(_config(base), base), "cpu.affinity").status == "ok"


def test_busy_machine_warns(tmp_path, monkeypatch):
    base = _layout(tmp_path)
    monkeypatch.setattr("lidarbench.campaign.preflight.psutil.cpu_percent", lambda interval: 95.0)
    item = _item(_preflight(_config(base), base, cpu_sample_s=0.01), "cpu.load")
    assert item.status == "warning" and "busy" in item.message


def test_unavailable_tool_is_warning_not_error(tmp_path):
    base = _layout(tmp_path)
    tools = '[[tools]]\nid = "synthetic"\noptions = { python = "no-such-python-lidarbench" }\n'
    report = _preflight(_config(base, tools=tools), base)
    assert _item(report, "tool.synthetic").status == "warning"
    plan = _item(report, "plan.synthetic.synthetic.checksum")
    assert plan.status == "warning" and "skipped_unavailable" in plan.message
    assert report.exit_code == 0


def test_unknown_adapter_is_error(tmp_path):
    base = _layout(tmp_path)
    report = _preflight(_config(base, tools='[[tools]]\nid = "nonexistent"\n'), base)
    assert _item(report, "tool.nonexistent").status == "error"
    assert _item(report, "plan.nonexistent.synthetic.checksum").status == "skipped"


def test_validation_contracts_and_missing_contract(tmp_path):
    base = _layout(tmp_path)
    workloads = SYNTHETIC_WORKLOAD + '[[workloads]]\nid = "pixel_metrics_1a"\n'
    tools = SYNTHETIC_TOOL + '[[tools]]\nid = "lasr"\n'
    for tile in (base / "data").glob("*.las"):
        tile.with_suffix(".lax").write_bytes(b"index")
    report = _preflight(_config(base, workloads=workloads, tools=tools), base)
    synthetic = _item(report, "validation.synthetic.checksum")
    assert synthetic.status == "skipped" and "no contract" in synthetic.message
    contract = _item(report, "validation.pixel_metrics_1a")
    assert contract.status == "ok"
    assert contract.details["tools"]["lasr"]["contract_tool"] == "lasR"
    assert contract.details["tools"]["lasr"]["comparability"] == "strictly_comparable"
    assert contract.details["tools"]["synthetic"]["comparability"] == "unsupported"
    assert _item(report, "validation.backends").status == "skipped"
    plan = _item(report, "plan.synthetic.pixel_metrics_1a")
    assert plan.status == "skipped" and "skipped_unsupported" in plan.message


def test_numerical_mode_without_backends_is_error(tmp_path, monkeypatch):
    base = _layout(tmp_path)
    missing = BackendStatus("rasterio", False, "optional dependency missing", {})
    monkeypatch.setattr("lidarbench.validation.io.probe_raster_backend", lambda: missing)
    workloads = '[[workloads]]\nid = "pixel_metrics_1a"\n'
    tools = '[[tools]]\nid = "lasr"\n'
    for tile in (base / "data").glob("*.las"):
        tile.with_suffix(".lax").write_bytes(b"index")
    path = _config(base, workloads=workloads, tools=tools, validation_mode="numerical")
    report = _preflight(path, base)
    item = _item(report, "validation.backends")
    assert item.status == "error" and "unavailable" in item.message
    assert report.exit_code == 1


def test_numerical_mode_synthetic_only_skips_backends(tmp_path):
    base = _layout(tmp_path)
    report = _preflight(_config(base, validation_mode="numerical"), base)
    assert _item(report, "validation.backends").status == "skipped"
    assert report.exit_code == 0


def test_corrupt_header_is_error(tmp_path):
    base = _layout(tmp_path)
    (base / "data" / "broken.las").write_bytes(b"not a las file")
    report = _preflight(_config(base), base)
    item = _item(report, "dataset.tiles.headers")
    assert item.status == "error" and "broken.las" in item.message


@pytest.mark.skipif(sys.platform == "win32", reason="POSIX permission bits")
def test_read_only_inputs_are_ok(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    data = base / "data"
    tiles = list(data.iterdir())
    try:
        for tile in tiles:
            tile.chmod(0o444)
        data.chmod(0o555)
        if os.access(data, os.W_OK):
            pytest.skip("running with privileges that ignore permission bits")
        item = _item(_preflight(path, base), "dataset.tiles.protection")
        assert item.status == "ok"
    finally:
        data.chmod(0o755)
        for tile in tiles:
            tile.chmod(0o644)


def test_writable_inputs_warn(tmp_path):
    base = _layout(tmp_path)
    item = _item(_preflight(_config(base), base), "dataset.tiles.protection")
    assert item.status == "warning" and "read-only copy" in item.message


def test_dirty_checkout_warns_for_new_campaign(tmp_path):
    base = _layout(tmp_path)

    def dirty() -> dict[str, Any]:
        return {"available": True, "toplevel": os.fspath(tmp_path / "elsewhere"), "dirty": True}

    item = _item(_preflight(_config(base), base, git_info=dirty), "code.git")
    assert item.status == "warning" and "--allow-dirty" in item.message


def test_missing_reporting_tools_warn(tmp_path, monkeypatch):
    monkeypatch.delenv("QUARTO", raising=False)
    monkeypatch.delenv("LIDARBENCH_RSCRIPT", raising=False)
    base = _layout(tmp_path)
    report = _preflight(_config(base), base, which=lambda name: None)
    assert _item(report, "reporting.quarto").status == "warning"
    assert _item(report, "reporting.rscript").status == "warning"
    assert report.exit_code == 0


def test_existing_campaign_identity_and_hash(tmp_path):
    base = _layout(tmp_path)
    path = _config(base)
    summary = make_runner(path, base, registry=_registry()).run()
    assert summary.exit_code == 0
    campaign = summary.campaign_path
    before = _tree(campaign)

    report = _preflight(path, base)
    assert report.exit_code == 0, render_report(report)
    item = _item(report, "campaign")
    assert item.status == "ok" and "1/1 measurement entries complete" in item.message
    identity = _item(report, "campaign.identity")
    assert identity.status == "ok" and "datasets" in identity.details["not_compared"]

    hashed = _preflight(path, base, full_hash=True)
    assert _item(hashed, "dataset.tiles.hash").status == "ok"
    identity = _item(hashed, "campaign.identity")
    assert identity.status == "ok" and "datasets" in identity.details["compared"]
    assert _tree(campaign) == before

    text = path.read_text(encoding="utf-8").replace("repetitions = 1", "repetitions = 2")
    path.write_text(text, encoding="utf-8")
    identity = _item(_preflight(path, base), "campaign.identity")
    assert identity.status == "error"
    assert "config" in identity.details["changed"]


def test_manifest_dataset_quick_and_hash(tmp_path):
    registry_dir = copy_repo_recipes(tmp_path).root
    fixtures = tmp_path / "fixtures"
    fixtures.mkdir()
    args = ["--registry", str(registry_dir), "--root", f"fixtures={fixtures}"]
    assert cli.main(["data", "prepare", "synthetic-tiles-v1", *args]) == 0
    base = _layout(tmp_path)
    extra = "\n".join(
        [
            "[data]",
            f"registry = {json.dumps(str(registry_dir))}",
            f"roots = {{ fixtures = {json.dumps(str(fixtures))} }}",
        ]
    )
    path = _config(base, dataset='manifest = "synthetic-tiles-v1/pair"', extra=extra)
    report = _preflight(path, base)
    assert _item(report, "dataset.tiles.manifest").status == "ok"
    assert report.exit_code == 0, render_report(report)

    tile = fixtures / "synthetic-tiles-v1" / "pair" / "500100_5000000.las"
    tile.chmod(0o644)
    data = bytearray(tile.read_bytes())
    data[-1] ^= 1
    tile.write_bytes(bytes(data))
    assert _item(_preflight(path, base), "dataset.tiles.manifest").status == "ok"
    hashed = _preflight(path, base, full_hash=True)
    item = _item(hashed, "dataset.tiles.hash")
    assert item.status == "error" and "does not match manifest" in item.message

    with open(tile, "ab") as handle:
        handle.write(b"x")
    item = _item(_preflight(path, base), "dataset.tiles.manifest")
    assert item.status == "error" and "size" in item.message


def test_cli_preflight_json(tmp_path, monkeypatch, capsys):
    monkeypatch.setenv("LIDARBENCH_STATE_DIR", str(tmp_path / "state-cli"))
    base = _layout(tmp_path)
    path = _config(base)
    code = cli.main(["preflight", str(path), "--json"])
    record = json.loads(capsys.readouterr().out)
    assert record["schema"] == PREFLIGHT_SCHEMA
    assert code == record["exit_code"]
    assert not (tmp_path / "state-cli").exists()

    assert cli.main(["preflight", str(path)]) == code
    assert "Result:" in capsys.readouterr().out


def test_cli_preflight_usage_error():
    with pytest.raises(SystemExit) as info:
        cli.main(["preflight"])
    assert info.value.code == 2
