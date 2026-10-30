"""lidR and lasR adapters with a mocked R (no R installation needed).

Covers planning (argv, environment, invocation document, expected outputs),
probing (missing Rscript, packages, versions, renv), interpretation of the
bridge record (warnings, errors and inconsistencies never become successes),
execution through the generic ``execute_plan`` API and a campaign run whose
results use the common campaign schema.
"""

from __future__ import annotations

import json
import re
from pathlib import Path

import pytest
from campaign_helpers import write_config, write_layout
from r_bridge_fakes import (
    FAKE_VERSION,
    fake_probe_record,
    make_fake_project,
    make_fake_rscript,
    posix_only,
)

from lidarbench.campaign.results import collect_rows
from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.config.campaign import load_config
from lidarbench.core.clock import Instant
from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import (
    ArtifactKind,
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    UnavailableReason,
)
from lidarbench.execution.adapters.lasr import LasRAdapter
from lidarbench.execution.adapters.lidr import LidRAdapter
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.adapters.rscript import (
    INVOCATION_FILE,
    INVOCATION_SCHEMA,
    LEGACY_THREAD_VARIABLES,
    RESULT_FILE,
    RESULT_SCHEMA,
    RSCRIPT_ENV,
    ProbeError,
    RBridgeAdapter,
    default_project_root,
    exit_code_meaning,
)
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import PlanStatus, plan_invocation
from lidarbench.execution.process import ProcessOutcome, ProcessResult
from lidarbench.workloads import default_suite_dir, load_suite

ADAPTERS = (LidRAdapter, LasRAdapter)
REPO = Path(__file__).resolve().parents[2]
PLATFORM = get_platform_info()


@pytest.fixture
def project(tmp_path: Path) -> Path:
    return make_fake_project(tmp_path / "r project")


@pytest.fixture
def layout(tmp_path: Path) -> dict[str, Path]:
    base = tmp_path / "attempt root"
    paths = {
        "inputs": base / "input tiles",
        "out": base / "out dir",
        "work": base / "work dir",
        "attempt": base / "attempt",
    }
    for path in paths.values():
        path.mkdir(parents=True)
    for name in ("west tile.las", "east;$tile.laz"):
        (paths["inputs"] / name).write_bytes(b"LASF")
    return paths


def _ready(adapter: RBridgeAdapter, packages: tuple[str, ...] | None = None) -> RBridgeAdapter:
    """Mark an adapter as probed without running anything."""
    adapter._probe_record = fake_probe_record(packages or adapter.required_packages)
    adapter._rscript_path = Path("/opt/R bin/Rscript").absolute()
    return adapter


def _invocation(layout, workload="pixel_metrics_1a", threads=2, **parameters) -> Invocation:
    return Invocation(
        workload,
        tuple(sorted(layout["inputs"].iterdir())),
        layout["out"],
        layout["work"],
        threads=threads,
        parameters=parameters,
    )


def _process(tmp_path: Path, exit_code: int = 0, stderr: str = "") -> ProcessResult:
    stderr_path = tmp_path / "stderr.log"
    stderr_path.write_text(stderr, encoding="utf-8")
    return ProcessResult(
        outcome=ProcessOutcome.EXITED,
        argv=("/usr/bin/Rscript",),
        cwd=tmp_path,
        stdout_path=tmp_path / "stdout.log",
        stderr_path=stderr_path,
        environment={},
        pid=4242,
        exit_code=exit_code,
        start=Instant(1_000, 1_000_000_000),
        end=Instant(9_000_001_000, 10_000_000_000),
    )


def _record(adapter: RBridgeAdapter, workload: str = "pixel_metrics_1a", **overrides) -> dict:
    record = {
        "schema": RESULT_SCHEMA,
        "status": "succeeded",
        "phase": "complete",
        "exit_code": 0,
        "tool": adapter.r_package,
        "workload_id": workload,
        "diagnostic": None,
        "warnings": [],
        "r": {"version": "4.6.1"},
        "packages": dict.fromkeys(adapter.required_packages, FAKE_VERSION),
        "timing_workload": {
            "start_wall_ns": 3_000_000_000,
            "end_wall_ns": 8_000_000_000,
            "elapsed_s": 5.0,
        },
        "timing": {
            "clock": "R Sys.time()",
            "bridge_start": 1_500_000_000,
            "packages_loaded": 2_000_000_000,
            "namespaces_preloaded": 2_500_000_000,
            "flag": True,
        },
        "preloaded_namespaces": {"sf": "1.0-21", "terra": "1.8-60"},
        "workload_loaded_namespaces": [],
    }
    record.update(overrides)
    return record


# -- registration and declarations ------------------------------------------------------


def test_adapters_are_registered_entry_points():
    registry = AdapterRegistry.discover()
    assert registry.adapter_class("lidr") is LidRAdapter
    assert registry.adapter_class("lasr") is LasRAdapter


@pytest.mark.parametrize(
    ("cls", "model"),
    [
        (LidRAdapter, ThreadingModel.WORKER_PROCESSES),
        (LasRAdapter, ThreadingModel.CONCURRENT_FILES),
    ],
)
def test_workload_declarations(cls, model):
    adapter = cls()
    support = {s.workload_id: s for s in adapter.workload_support()}
    for workload in ("pixel_metrics_1a", "generate_DSM1"):
        assert support[workload].status is ImplementationStatus.RUNNABLE
        assert support[workload].threading.model is model
        assert adapter.support_for(workload, 64).status is ImplementationStatus.RUNNABLE
    others = [s for w, s in support.items() if w not in ("pixel_metrics_1a", "generate_DSM1")]
    assert others and all(s.reason and not s.status.dispatchable for s in others)


def test_lasr_declares_legacy_blocked_workloads():
    support = {s.workload_id: s for s in LasRAdapter().workload_support()}
    for workload in ("normalization", "complex_task1", "complex_task2", "generate_DSM2"):
        assert support[workload].status is ImplementationStatus.BLOCKED


@pytest.mark.parametrize("cls", ADAPTERS)
def test_pinned_definitions_match_contracts_and_legacy_calls(cls):
    """The adapter pins the workloads-v1 contract and the exact legacy R call."""
    suite = load_suite(default_suite_dir())
    setup = (REPO / "R" / "0_setup.R").read_text(encoding="utf-8")
    for workload in cls.workloads.values():
        contract = suite.get(workload.workload_id)
        impl = contract.implementation(cls.contract_tool)
        assert impl is not None and impl.status is ImplementationStatus.RUNNABLE
        assert impl.equivalence == "matched_definition"
        assert impl.artifacts == (workload.artifact_pattern,)
        assert workload.buffer == contract.buffer
        assert f'"{workload.call}"' in setup


def test_default_project_is_this_checkout():
    assert default_project_root() == REPO
    adapter = LidRAdapter()
    assert all(path.is_file() for path in adapter.bridge_files.values())


@pytest.mark.parametrize(
    "options",
    [
        {"rscript": 1},
        {"renv": "yes"},
        {"probe_timeout_s": True},
        {"probe_timeout_s": 0},
        {"future_backend": "callr"},
        {"future_backend": 1},
        {"unknown": 1},
    ],
)
def test_invalid_options_raise_type_error(options):
    with pytest.raises(TypeError):
        LidRAdapter(**options)


def test_lidr_future_backend_option(layout, project):
    default = LidRAdapter(project=str(project))
    assert default.future_backend == "multisession"
    assert default.required_packages == LidRAdapter.required_packages
    mirai = _ready(LidRAdapter(project=str(project), future_backend="mirai"))
    assert mirai.required_packages == (*LidRAdapter.required_packages, "future.mirai", "mirai")
    assert LidRAdapter.required_packages == ("lidR", "future", "data.table", "jsonlite")
    for adapter, backend in ((_ready(default), "multisession"), (mirai, "mirai")):
        inv = _invocation(layout, threads=3, epsg=2959)
        plan = plan_invocation(adapter, inv, availability=_available(adapter))
        document = json.loads(plan.command.config_files[0].content)
        assert document["lidr_future_backend"] == backend
        assert document["required_packages"] == list(adapter.required_packages)
        assert plan.metadata["future_backend"] == backend
    lasr = _ready(LasRAdapter(project=str(project)))
    plan = plan_invocation(lasr, _invocation(layout), availability=_available(lasr))
    assert "lidr_future_backend" not in json.loads(plan.command.config_files[0].content)


# -- planning --------------------------------------------------------------------------


@pytest.mark.parametrize("cls", ADAPTERS)
def test_plan_builds_rscript_command(cls, layout, project):
    adapter = _ready(cls(project=str(project)))
    inv = _invocation(layout, threads=3, epsg=2959)
    plan = plan_invocation(adapter, inv, availability=_available(adapter))
    assert plan.status is PlanStatus.READY, plan.reason
    argv = plan.command.argv
    assert argv[0] == str(adapter._rscript_path)
    assert argv[1:3] == ("--vanilla", str(project / "bridges" / "r" / "runner.R"))
    options = dict(zip(argv[3::2], argv[4::2], strict=True))
    assert options == {
        "--project": str(project),
        "--renv": "on",
        "--invocation": str(layout["work"] / INVOCATION_FILE),
        "--result": str(layout["work"] / RESULT_FILE),
    }
    assert plan.command.cwd == layout["work"]
    [config] = plan.command.config_files
    assert config.name == INVOCATION_FILE
    document = json.loads(config.content)
    assert document["schema"] == INVOCATION_SCHEMA
    assert document["tool"] == cls.r_package
    assert document["expected_call"] == cls.workloads["pixel_metrics_1a"].call
    assert document["buffer"] == 20.0 and document["threads"] == 3 and document["epsg"] == 2959
    assert document["inputs"] == [str(p) for p in inv.inputs]  # spaces/metacharacters intact
    assert document["required_packages"] == list(cls.required_packages)
    names = sorted(a.path.name for a in plan.expected_outputs)
    assert names == ["east;$tile.tif", "west tile.tif"]
    assert all(a.kind is ArtifactKind.RASTER and a.required for a in plan.expected_outputs)
    assert plan.metadata["legacy_call_sha256"] == cls.workloads["pixel_metrics_1a"].call_sha256
    assert plan.metadata["epsg"] == 2959


def _available(adapter):
    from lidarbench.execution.adapters import Availability

    return Availability.ok({"rscript": adapter._rscript_path})


def test_thread_environment_follows_legacy_harness(project):
    lidr = LidRAdapter(project=str(project)).thread_env(4)
    assert lidr == dict.fromkeys(LEGACY_THREAD_VARIABLES, "1")
    lasr = LasRAdapter(project=str(project)).thread_env(4)
    assert lasr["OMP_NUM_THREADS"] == lasr["OMP_THREAD_LIMIT"] == "4"
    assert all(lasr[v] == "1" for v in LEGACY_THREAD_VARIABLES if not v.startswith("OMP_"))
    assert lasr["OMP_MAX_ACTIVE_LEVELS"] == "1"


def test_renv_off_is_passed_to_bridge(layout, project):
    adapter = _ready(LidRAdapter(project=str(project), renv=False))
    plan = plan_invocation(adapter, _invocation(layout), availability=_available(adapter))
    assert plan.command.argv[plan.command.argv.index("--renv") + 1] == "off"


@pytest.mark.parametrize(
    ("parameters", "reason"),
    [
        ({"height_cutoff": 1.0}, "does not accept parameter"),
        ({"epsg": "2959"}, "positive integer"),
        ({"epsg": True}, "positive integer"),
        ({"epsg": -1}, "positive integer"),
    ],
)
def test_invalid_parameters_are_unsupported(layout, project, parameters, reason):
    adapter = _ready(LidRAdapter(project=str(project)))
    plan = plan_invocation(adapter, _invocation(layout, **parameters), _available(adapter))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and reason in plan.reason


def test_non_las_and_colliding_inputs_are_unsupported(layout, project):
    adapter = _ready(LasRAdapter(project=str(project)))
    (layout["inputs"] / "notes.txt").write_text("x")
    plan = plan_invocation(adapter, _invocation(layout), _available(adapter))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "LAS/LAZ" in plan.reason
    (layout["inputs"] / "notes.txt").unlink()
    (layout["inputs"] / "WEST TILE.laz").write_bytes(b"LASF")
    plan = plan_invocation(adapter, _invocation(layout), _available(adapter))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "same output" in plan.reason


def test_blocked_workload_is_skipped(layout, project):
    adapter = _ready(LasRAdapter(project=str(project)))
    plan = plan_invocation(adapter, _invocation(layout, "generate_DSM2"), _available(adapter))
    assert plan.status is PlanStatus.SKIPPED_BLOCKED and "TIN" in plan.reason


# -- probing ---------------------------------------------------------------------------


def test_probe_reports_missing_bridge_files(tmp_path):
    availability = LidRAdapter(project=str(tmp_path)).probe(PLATFORM)
    assert availability.reason is UnavailableReason.MISCONFIGURED
    assert "runner.R" in availability.detail


def test_probe_reports_missing_rscript(project, monkeypatch):
    monkeypatch.delenv(RSCRIPT_ENV, raising=False)
    adapter = LasRAdapter(project=str(project), rscript="Rscript-missing-lidarbench")
    availability = adapter.probe(PLATFORM)
    assert availability.reason is UnavailableReason.NOT_INSTALLED
    assert "Rscript not found" in availability.detail
    plan = plan_invocation(adapter, Invocation("generate_DSM1", (project,), project, project))
    assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE


def test_rscript_from_environment(project, monkeypatch, tmp_path):
    fake = make_fake_rscript(tmp_path / "bin dir")
    monkeypatch.setenv(RSCRIPT_ENV, str(fake))
    assert LidRAdapter(project=str(project)).resolve_rscript() == fake


@pytest.mark.parametrize(
    ("mutate", "reason", "text"),
    [
        (
            lambda r: r["packages"]["lidR"].update(installed=None),
            UnavailableReason.NOT_INSTALLED,
            "lidR (not installed)",
        ),
        (
            lambda r: r["packages"]["future"].update(loadable=False, error="libgdal missing"),
            UnavailableReason.NOT_INSTALLED,
            "future (cannot be loaded: libgdal missing)",
        ),
        (
            lambda r: r["packages"]["lidR"].update(installed="4.0.0"),
            UnavailableReason.VERSION_UNSUPPORTED,
            "lidR 4.0.0 (renv.lock: 1.0)",
        ),
        (
            lambda r: r.update(r_version="4.5.0"),
            UnavailableReason.VERSION_UNSUPPORTED,
            "R 4.5.0",
        ),
        (
            lambda r: r["renv"].update(activated=False, error="no renv"),
            UnavailableReason.MISCONFIGURED,
            "no renv",
        ),
    ],
)
def test_probe_assessment(project, mutate, reason, text):
    adapter = LidRAdapter(project=str(project))
    record = fake_probe_record(adapter.required_packages)
    mutate(record)
    availability = adapter.assess_probe(record)
    assert availability is not None and availability.reason is reason
    assert text in availability.detail


def test_version_check_can_be_disabled(project):
    adapter = LidRAdapter(project=str(project), require_locked_versions=False)
    record = fake_probe_record(adapter.required_packages)
    record["packages"]["lidR"]["installed"] = "4.0.0"
    assert adapter.assess_probe(record) is None


def test_probe_failure_is_reported(project, monkeypatch, tmp_path):
    adapter = LidRAdapter(project=str(project), rscript=str(make_fake_rscript(tmp_path / "b")))

    def boom(rscript):
        raise ProbeError("R probe timed out after 1 s")

    monkeypatch.setattr(adapter, "run_probe", boom)
    availability = adapter.probe(PLATFORM)
    assert availability.reason is UnavailableReason.PROBE_FAILED
    assert "timed out" in availability.detail


@pytest.mark.platform
@pytest.mark.skipif(posix_only, reason="fake Rscript is a POSIX shebang script")
@pytest.mark.parametrize(
    ("mode", "reason", "text"),
    [
        ("missing", UnavailableReason.NOT_INSTALLED, "not installed"),
        ("unloadable", UnavailableReason.NOT_INSTALLED, "no GDAL"),
        ("mismatch", UnavailableReason.VERSION_UNSUPPORTED, "0.9"),
        ("renv", UnavailableReason.MISCONFIGURED, "boom"),
        ("error", UnavailableReason.PROBE_FAILED, "probe exploded"),
        ("garbage", UnavailableReason.PROBE_FAILED, "invalid JSON"),
        ("silent", UnavailableReason.PROBE_FAILED, "nothing to see"),
    ],
)
def test_probe_through_fake_rscript(project, tmp_path, monkeypatch, mode, reason, text):
    monkeypatch.setenv("FAKE_R_PROBE", mode)
    adapter = LasRAdapter(project=str(project), rscript=str(make_fake_rscript(tmp_path / "b")))
    availability = adapter.probe(PLATFORM)
    assert availability.reason is reason and text in availability.detail


@pytest.mark.platform
@pytest.mark.skipif(posix_only, reason="fake Rscript is a POSIX shebang script")
def test_identify_records_versions_and_bridge_files(project, tmp_path):
    rscript = make_fake_rscript(tmp_path / "b")
    adapter = LidRAdapter(project=str(project), rscript=str(rscript))
    assert adapter.probe(PLATFORM).available
    identity = adapter.identify()
    assert identity.tool == "lidR" and identity.version == FAKE_VERSION
    assert identity.executables["rscript"] == rscript
    assert identity.executables["legacy_functions"] == project / "R" / "1_functions.R"
    assert identity.executables["bridge_runner"] == project / "bridges" / "r" / "runner.R"
    details = identity.details
    assert details["r_version"] == "4.6.1" and details["package.future"] == FAKE_VERSION
    assert details["locked.lidR"] == FAKE_VERSION and details["renv"] is True
    assert re.fullmatch(r"[0-9a-f]{64}", details["renv_lockfile_sha256"])
    assert all(isinstance(v, str | int | float | bool | type(None)) for v in details.values())
    assert details["future_backend"] == "multisession"
    mirai = LidRAdapter(project=str(project), rscript=str(rscript), future_backend="mirai")
    assert mirai.probe(PLATFORM).available
    mirai_details = mirai.identify().details
    assert mirai_details["future_backend"] == "mirai"
    assert mirai_details["package.mirai"] == FAKE_VERSION
    assert mirai_details["package.future.mirai"] == FAKE_VERSION


# -- interpretation --------------------------------------------------------------------


def _write_record(layout, record) -> None:
    (layout["work"] / RESULT_FILE).write_text(json.dumps(record), encoding="utf-8")


@pytest.mark.parametrize("cls", ADAPTERS)
def test_interpret_success(cls, layout, project, tmp_path):
    adapter = _ready(cls(project=str(project)))
    _write_record(layout, _record(adapter))
    result = adapter.interpret(_invocation(layout), _process(tmp_path))
    assert result.failure_reason is None and result.warnings == ()
    assert result.tool_timing.start_wall_ns == 3_000_000_000
    assert result.tool_timing.marks == {
        "bridge_start": 1_500_000_000,
        "packages_loaded": 2_000_000_000,
        "namespaces_preloaded": 2_500_000_000,
    }
    assert "preloading of namespaces" in result.tool_timing.boundary
    assert "sf, terra" in result.tool_timing.boundary
    assert result.details["bridge_record"]["tool"] == cls.r_package


@pytest.mark.parametrize(
    ("overrides", "exit_code", "text"),
    [
        (
            {
                "status": "failed",
                "phase": "workload",
                "exit_code": 3,
                "diagnostic": "1 R warning(s) during the workload: NAs",
                "warnings": [{"phase": "workload", "message": "NAs"}],
            },
            3,
            "R warning during the workload",
        ),
        (
            {"status": "failed", "phase": "preparation", "exit_code": 1, "diagnostic": "no LAX"},
            1,
            "phase preparation",
        ),
        ({"status": "failed", "phase": "workload", "exit_code": 1}, 0, "phase workload"),
        ({}, 1, "says succeeded but the process exited with 1"),
        ({"phase": "workload"}, 0, "says succeeded"),
        ({"tool": "lasR", "workload_id": "x"}, 0, "describes lasR/x"),
        ({"timing_workload": {}}, 0, "no workload interval"),
        ({"packages": {"lidR": "9.9"}}, 0, "lidR 1.0 -> 9.9"),
        ({"r": {"version": "4.7.0"}}, 0, "R 4.6.1 -> 4.7.0"),
    ],
)
def test_interpret_rejects_failures_and_inconsistencies(
    layout, project, tmp_path, overrides, exit_code, text
):
    adapter = _ready(LidRAdapter(project=str(project)))
    _write_record(layout, _record(adapter, **overrides))
    result = adapter.interpret(_invocation(layout), _process(tmp_path, exit_code))
    assert result.failure_reason is not None and text in result.failure_reason
    if overrides.get("warnings"):
        assert result.warnings == ("R warning (workload): NAs",)


def test_interpret_without_record_uses_stderr(layout, project, tmp_path):
    adapter = _ready(LidRAdapter(project=str(project)))
    result = adapter.interpret(_invocation(layout), _process(tmp_path, 4, "renv failed\n"))
    assert "no result record (exit 4: renv activation or package failure)" in (
        result.failure_reason or ""
    )
    assert "renv failed" in result.failure_reason


@pytest.mark.parametrize("content", ["{broken", json.dumps({"schema": "other"}), "[]"])
def test_interpret_rejects_unreadable_records(layout, project, tmp_path, content):
    adapter = _ready(LidRAdapter(project=str(project)))
    (layout["work"] / RESULT_FILE).write_text(content, encoding="utf-8")
    result = adapter.interpret(_invocation(layout), _process(tmp_path))
    assert result.failure_reason is not None


def test_exit_code_meanings():
    assert exit_code_meaning(0) == "succeeded"
    assert "warning" in exit_code_meaning(3)
    assert "signal 9" in exit_code_meaning(-9)
    assert "unexpected" in exit_code_meaning(139)
    assert exit_code_meaning(None) == "no exit status"


# -- execution through the generic API (fake Rscript) ----------------------------------


def _execute(cls, layout, project, tmp_path, monkeypatch, mode, threads=2):
    monkeypatch.setenv("FAKE_R_MODE", mode)
    adapter = cls(project=str(project), rscript=str(make_fake_rscript(tmp_path / "bin")))
    availability = adapter.probe(PLATFORM)
    assert availability.available, availability.detail
    inv = _invocation(layout, threads=threads)
    plan = plan_invocation(adapter, inv, availability=availability)
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, layout["attempt"], timeout_s=60, adapter=adapter)
    record = json.loads((layout["attempt"] / "execution.json").read_text(encoding="utf-8"))
    return execution, record


@pytest.mark.platform
@pytest.mark.skipif(posix_only, reason="fake Rscript is a POSIX shebang script")
@pytest.mark.parametrize("cls", ADAPTERS)
def test_execute_plan_success_records_interpretation(cls, layout, project, tmp_path, monkeypatch):
    execution, record = _execute(cls, layout, project, tmp_path, monkeypatch, "ok", threads=3)
    assert str(execution.execution_status) == "succeeded"
    assert not execution.missing_required_outputs
    interpretation = record["interpretation"]
    assert interpretation["provided"] and interpretation["failure_reason"] is None
    seen = interpretation["details"]["bridge_record"]
    expected_omp = "3" if cls is LasRAdapter else "1"
    assert seen["threads"]["OMP_NUM_THREADS"] == expected_omp
    assert seen["threads"]["OPENBLAS_NUM_THREADS"] == "1"
    timing = interpretation["tool_timing"]
    assert timing["within_outer_interval"] is True
    assert timing["startup_overhead_ns"] > 0 and timing["shutdown_overhead_ns"] >= 0
    parts = sum(timing[k] for k in ("startup_overhead_ns", "workload_elapsed_ns"))
    parts += timing["shutdown_overhead_ns"]
    # Parts use the wall clock, the outer interval the monotonic clock.
    assert abs(parts - timing["outer_elapsed_ns"]) < 50_000_000
    assert timing["outer_timing_boundary"] == "process"
    marks = timing["marks"]
    assert marks["packages_loaded"] <= marks["namespaces_preloaded"] <= timing["start_wall_ns"]
    assert {"sf", "terra"} <= set(seen["preloaded_namespaces"])
    assert [p["name"] for p in record["phases"]][-2:] == ["interpretation", "output_check"]


@pytest.mark.platform
@pytest.mark.skipif(posix_only, reason="fake Rscript is a POSIX shebang script")
@pytest.mark.parametrize(
    ("mode", "phase", "text"),
    [
        ("warning", "process", "R warning during the workload"),
        ("error", "process", "lidR: chunk failed"),
        ("lie", "interpretation", "hidden error"),
        ("norecord", "interpretation", "no result record"),
        ("crash", "process", "caught segfault"),
        ("drift", "interpretation", "differ from the probed tool identity"),
    ],
)
def test_execute_plan_never_turns_r_problems_into_success(
    layout, project, tmp_path, monkeypatch, mode, phase, text
):
    execution, record = _execute(LidRAdapter, layout, project, tmp_path, monkeypatch, mode)
    assert str(execution.execution_status) == "failed"
    assert record["execution_status"] == "failed"
    assert record["failure_phase"] == phase
    assert text in record["failure_reason"]
    if mode == "warning":
        assert "R warning (workload): NAs introduced" in record["warnings"]


def test_execute_plan_rejects_foreign_adapter(layout, project):
    adapter = _ready(LidRAdapter(project=str(project)))
    inv = _invocation(layout)
    plan = plan_invocation(adapter, inv, availability=_available(adapter))
    with pytest.raises(ValueError, match="different adapter"):
        execute_plan(
            plan, inv, layout["attempt"], adapter=_ready(LasRAdapter(project=str(project)))
        )


# -- campaign schema -------------------------------------------------------------------


@pytest.mark.platform
@pytest.mark.skipif(posix_only, reason="fake Rscript is a POSIX shebang script")
def test_campaign_run_uses_common_schema(tmp_path, project, monkeypatch):
    monkeypatch.setenv("FAKE_R_MODE", "ok")
    rscript = make_fake_rscript(tmp_path / "bin")
    base = write_layout(tmp_path)
    options = f'options = {{ rscript = "{rscript}", project = "{project}" }}'
    config_path = write_config(
        base,
        workloads=(
            '[[workloads]]\nid = "pixel_metrics_1a"\nparameters = { epsg = 2959 }\n'
            '[[workloads]]\nid = "generate_DSM1"\n'
        ),
        tools=f'[[tools]]\nid = "lidr"\n{options}\n[[tools]]\nid = "lasr"\n{options}\n',
        cpu_counts="[1, 2]",
        scheduling="repetitions = 1\nwarmups = 0\ncooldown_s = 0",
    )
    registry = AdapterRegistry()
    registry.register(LidRAdapter)
    registry.register(LasRAdapter)
    runner = CampaignRunner(
        load_config(config_path),
        options=RunOptions(),
        registry=registry,
        state_dir=base / "state",
        git_info=lambda: {"available": False, "reason": "test"},
        report=lambda line: None,
        handle_signals=False,
    )
    summary = runner.run()
    assert summary.statuses == {"succeeded": 8}, summary.to_record()
    rows = collect_rows(summary.campaign_path)
    assert len(rows) == 8
    for row in rows:
        assert row["tool_adapter"] in ("lidr", "lasr")
        assert row["tool_version"] == FAKE_VERSION
        assert row["timing_boundary"] == "process"
        assert row["tool_workload_elapsed_ns"] > 0
        assert 0 < row["startup_overhead_ns"] < row["elapsed_ns"]
        assert row["threading_model"] in ("worker_processes", "concurrent_files")
        assert row["output_files"] == 2 and row["missing_required_outputs"] == 0
    result_files = sorted((summary.campaign_path / "attempts").glob("*/result.json"))
    result = json.loads(result_files[0].read_text(encoding="utf-8"))
    assert result["tool_timing"]["clock"] == "R Sys.time()"
    tools = json.loads((summary.campaign_path / "identity" / "tools.json").read_text("utf-8"))
    text = json.dumps(tools)
    assert "legacy_functions" in text and "renv_lockfile_sha256" in text
