"""LAStools adapter with a fake LAStools installation (no LAStools needed).

Covers executable discovery (option, environment, PATH; Windows ``.exe`` and
native names; missing and partial installations), version detection and
normalization (licensing text is never recorded), command construction (argv
only, legacy arguments, paths with spaces and shell metacharacters, the height
cutoff port), conservative interpretation (exit statuses, ``ERROR``/``WARNING``
lines, unlicensed notices, incomplete outputs), the opt-in cutoff-warning
exception, and a campaign run through the generic execution engine.
"""

from __future__ import annotations

import json
import os
import re
from pathlib import Path

import pytest
from campaign_helpers import write_config
from lastools_fakes import FAKE_BUILD, SECRET, fake_env, make_fake_lastools, posix_only
from validation_fixtures import write_las

from lidarbench.campaign.results import collect_rows
from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.config.campaign import load_config
from lidarbench.core.clock import Instant
from lidarbench.core.platform import PlatformInfo, get_platform_info
from lidarbench.execution.adapters import (
    ArtifactKind,
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    UnavailableReason,
    UnsupportedCombinationError,
)
from lidarbench.execution.adapters.lastools import (
    BIN_ENV,
    CUTOFF_EXCEPTION,
    CUTOFF_POLICY,
    CUTOFF_SUGGESTION,
    LICENCE_FLAG,
    WORKLOADS,
    LastoolsAdapter,
    LastoolsWorkload,
    VersionError,
    command_filename,
    find_command,
    format_r17,
    height_cutoff,
    inputs_height_cutoff,
    known_cutoff_warnings,
    locate_bin_dir,
    parse_version,
    query_version,
)
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.adapters.rscript import LEGACY_THREAD_VARIABLES
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import AdapterContractError, PlanStatus, plan_invocation
from lidarbench.execution.process import ProcessOutcome, ProcessResult
from lidarbench.workloads import default_suite_dir, load_suite

REPO = Path(__file__).resolve().parents[2]
PLATFORM = get_platform_info()
WINDOWS = PlatformInfo("Windows", "11", "AMD64", "3.11", 8, 2**34)
LINUX = PlatformInfo("Linux", "6", "x86_64", "3.11", 8, 2**34)
needs_posix = pytest.mark.skipif(posix_only, reason="fake LAStools are POSIX shebang scripts")


@pytest.fixture(autouse=True)
def _isolated_environment(monkeypatch, tmp_path):
    """No real LAStools from the developer's environment leaks into unit tests."""
    monkeypatch.delenv(BIN_ENV, raising=False)
    monkeypatch.setenv("PATH", os.fspath(tmp_path / "empty PATH"))
    fake_env(monkeypatch)


@pytest.fixture
def bin_dir(tmp_path: Path) -> Path:
    return make_fake_lastools(tmp_path / "Program Files" / "LAStools" / "bin")


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
    write_las(paths["inputs"] / "west tile.las", [500000.5, 500010.0], [5000000.5, 5000010.0],
              [-12.25, 40.0])  # fmt: skip
    write_las(paths["inputs"] / "east;$tile.las", [500100.5], [5000000.5], [3.0])
    return paths


def _adapter(bin_dir: Path, platform: PlatformInfo | None = None, **options) -> LastoolsAdapter:
    adapter = LastoolsAdapter(bin_dir=os.fspath(bin_dir), **options)
    availability = adapter.probe(platform or PLATFORM)
    assert availability.available, availability.detail
    return adapter


def _invocation(layout, workload="pixel_metrics_1a", threads=2, **parameters) -> Invocation:
    return Invocation(
        workload,
        tuple(sorted(layout["inputs"].glob("*.las"))),
        layout["out"],
        layout["work"],
        threads=threads,
        parameters=parameters,
    )


# -- registration, declarations and legacy parity -------------------------------------


def test_adapter_is_a_registered_entry_point():
    registry = AdapterRegistry.discover()
    assert "lastools" in registry.ids()
    assert registry.adapter_class("lastools") is LastoolsAdapter


def test_workload_declarations():
    supports = {s.workload_id: s for s in LastoolsAdapter().workload_support()}
    assert set(supports) == {
        "normalization",
        "pixel_metrics_1a",
        "pixel_metrics_2",
        "generate_DEM",
        "generate_DSM1",
        "generate_DSM2",
        "tree_detection",
        "complex_task1",
        "complex_task2",
    }
    for workload in ("pixel_metrics_1a", "generate_DSM1"):
        support = supports[workload]
        assert support.status is ImplementationStatus.RUNNABLE
        assert support.threading.model is ThreadingModel.NATIVE_THREADS
        assert support.threading.min_threads == 1 and support.threading.max_threads is None
    for workload, support in supports.items():
        if workload not in WORKLOADS:
            assert support.status is ImplementationStatus.UNSUPPORTED and support.reason


def _legacy_lastools_call(task_id: str) -> tuple[str, tuple[str, ...]]:
    """The ``task_call_lastools`` of ``task_id`` in R/0_setup.R."""
    text = (REPO / "R" / "0_setup.R").read_text(encoding="utf-8")
    start = text.index(f'"{task_id}",')
    match = re.compile(r'list\(executable = "([^"]+)", args = c\((.*?)\)\)', re.S).search(
        text, start
    )
    assert match is not None
    return match.group(1), tuple(re.findall(r'"([^"]*)"', match.group(2)))


@pytest.mark.parametrize("workload_id", sorted(WORKLOADS))
def test_pinned_templates_match_legacy_calls_and_contracts(workload_id):
    workload = WORKLOADS[workload_id]
    assert (workload.command, workload.args) == _legacy_lastools_call(workload_id)
    impl = load_suite(default_suite_dir()).get(workload_id).implementation("lastools")
    assert impl is not None and impl.status is ImplementationStatus.RUNNABLE
    # workloads-v4 declares the upward-Y floor rule (and 0.01 m precision for the mean).
    assert impl.equivalence == "matched_definition"
    assert impl.cell_ownership == "pdal_binmode_floor"
    assert impl.value_precision == (0.01 if workload_id == "pixel_metrics_1a" else None)
    assert list(impl.artifacts) == [workload.artifact_pattern]
    assert workload.buffer == 20.0 and "-buffered" in workload.args
    assert workload.args[workload.args.index("-buffered") + 1] == "20"


def test_unknown_placeholder_is_rejected():
    with pytest.raises(ValueError, match="placeholder"):
        LastoolsWorkload("x", "lasgrid64", ("-i", "{inputs}", "{threads}"), "{stem}.tif")


@pytest.mark.parametrize(
    "options",
    [
        {"bin_dir": 3},
        {"accept_known_cutoff_warning": "yes"},
        {"expected_version": 260914},
        {"expected_version": "latest build"},
        {"version_timeout_s": True},
        {"version_timeout_s": 0},
    ],
)
def test_invalid_options_raise_type_error(options):
    with pytest.raises(TypeError):
        LastoolsAdapter(**options)


# -- discovery ------------------------------------------------------------------------


def test_command_filenames_per_platform():
    assert command_filename("lascanopy64", windows=True) == "lascanopy64.exe"
    assert command_filename("lascanopy64", windows=False) == "lascanopy64"


def test_bin_dir_precedence(tmp_path, monkeypatch):
    on_path = make_fake_lastools(tmp_path / "on path", ("lasgrid64",))
    unrelated = tmp_path / "unrelated"
    unrelated.mkdir()
    environ = {"PATH": os.pathsep.join([os.fspath(unrelated), "", os.fspath(on_path)])}
    commands = ("lascanopy64", "lasgrid64")
    assert locate_bin_dir("", commands, False, environ) == (on_path, "PATH")
    environ[BIN_ENV] = "relative/bin"
    assert locate_bin_dir("", commands, False, environ) == (
        Path("relative/bin").absolute(),
        "environment",
    )
    assert locate_bin_dir("/opt/x", commands, False, environ) == (Path("/opt/x"), "option")
    assert locate_bin_dir("", commands, True, {"PATH": os.fspath(on_path)}) == (None, "PATH")


@pytest.mark.platform
@needs_posix
def test_non_executable_files_and_directories_are_not_commands(tmp_path):
    (tmp_path / "lascanopy64").write_text("not executable")
    (tmp_path / "lasgrid64").mkdir()
    assert find_command("lascanopy64", tmp_path, windows=False) is None
    assert find_command("lasgrid64", tmp_path, windows=False) is None
    assert find_command("lasinfo64", tmp_path, windows=False) is None


@needs_posix
def test_probe_identifies_build_without_licensing_text(bin_dir):
    adapter = LastoolsAdapter(bin_dir=os.fspath(bin_dir))
    availability = adapter.probe(LINUX)
    assert availability.available, availability.detail
    assert availability.executables == {
        "lascanopy64": bin_dir / "lascanopy64",
        "lasgrid64": bin_dir / "lasgrid64",
    }
    identity = adapter.identify()
    assert identity.tool == "LAStools" and identity.version == FAKE_BUILD
    assert identity.executables == availability.executables
    assert identity.details["version.lascanopy64"] == FAKE_BUILD
    assert identity.details["discovery"] == "option"
    assert identity.details["platform_binaries"] == "native"
    assert identity.details["accept_known_cutoff_warning"] is False
    published = json.dumps([str(availability), str(identity), dict(identity.details)])
    assert SECRET not in published and "licensed to" not in published


@needs_posix
def test_probe_from_environment_and_path(bin_dir, monkeypatch):
    monkeypatch.setenv(BIN_ENV, os.fspath(bin_dir))
    adapter = LastoolsAdapter()
    assert adapter.probe(LINUX).available
    assert adapter.identify().details["discovery"] == "environment"
    monkeypatch.delenv(BIN_ENV)
    monkeypatch.setenv("PATH", os.fspath(bin_dir))
    adapter = LastoolsAdapter()
    assert adapter.probe(LINUX).available
    assert adapter.identify().details["discovery"] == "PATH"


@pytest.mark.platform
@needs_posix
def test_windows_exe_binaries(tmp_path, layout):
    win_bin = make_fake_lastools(tmp_path / "C" / "Program Files" / "LAStools" / "bin", exe=True)
    adapter = _adapter(win_bin, WINDOWS)
    assert adapter.identify().details["platform_binaries"] == "windows-exe"
    plan = plan_invocation(adapter, _invocation(layout), availability=adapter.probe(WINDOWS))
    assert plan.ready, plan.reason
    assert plan.command.argv[0] == os.fspath(win_bin / "lascanopy64.exe")
    # Native (suffix-less) binaries are not used on Windows, and vice versa.
    native = make_fake_lastools(tmp_path / "native bin")
    assert LastoolsAdapter(bin_dir=os.fspath(native)).probe(WINDOWS).reason is (
        UnavailableReason.NOT_INSTALLED
    )
    assert LastoolsAdapter(bin_dir=os.fspath(win_bin)).probe(LINUX).reason is (
        UnavailableReason.NOT_INSTALLED
    )


def test_windows_style_bin_dir_option_is_kept_verbatim():
    adapter = LastoolsAdapter(bin_dir=r"C:\LAStools\bin")
    adapter.windows = True
    bin_dir, source, statuses = adapter.discover()
    assert source == "option" and r"LAStools\bin" in os.fspath(bin_dir)
    assert all("lascanopy64.exe" in s.problem for c, s in statuses.items() if c == "lascanopy64")


def test_missing_installation_is_unavailable_not_failed(tmp_path):
    availability = LastoolsAdapter().probe(LINUX)
    assert availability.reason is UnavailableReason.NOT_INSTALLED
    assert "not downloaded or installed automatically" in availability.detail
    missing = LastoolsAdapter(bin_dir=os.fspath(tmp_path / "no such dir")).probe(LINUX)
    assert missing.reason is UnavailableReason.MISCONFIGURED
    (tmp_path / "empty").mkdir()
    empty = LastoolsAdapter(bin_dir=os.fspath(tmp_path / "empty")).probe(LINUX)
    assert empty.reason is UnavailableReason.NOT_INSTALLED
    assert "lascanopy64, lasgrid64" in empty.detail


def test_missing_installation_is_a_skip_plan(layout):
    adapter = LastoolsAdapter()
    plan = plan_invocation(adapter, _invocation(layout), platform_info=LINUX)
    assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
    assert "not_installed" in plan.reason


@needs_posix
def test_partial_installation_supports_only_installed_commands(tmp_path, layout):
    only_grid = make_fake_lastools(tmp_path / "bin", ("lasgrid64",))
    adapter = _adapter(only_grid)
    supports = {s.workload_id: s for s in adapter.workload_support()}
    assert supports["generate_DSM1"].status is ImplementationStatus.RUNNABLE
    assert supports["pixel_metrics_1a"].status is ImplementationStatus.UNSUPPORTED
    assert "lascanopy64 not found" in supports["pixel_metrics_1a"].reason
    availability = adapter.probe(PLATFORM)
    plan = plan_invocation(adapter, _invocation(layout), availability=availability)
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "lascanopy64" in plan.reason
    assert plan_invocation(
        adapter, _invocation(layout, "generate_DSM1"), availability=availability
    ).ready
    assert adapter.identify().details["problem.lascanopy64"]


@needs_posix
@pytest.mark.parametrize(
    ("version", "options", "reason", "text"),
    [
        ("none", {}, UnavailableReason.PROBE_FAILED, "no 'LAStools ... version"),
        ("two", {}, UnavailableReason.PROBE_FAILED, "2 version lines"),
        ("fail", {}, UnavailableReason.PROBE_FAILED, "exited with status 3"),
        ("ok", {"expected_version": "261023"}, UnavailableReason.VERSION_UNSUPPORTED, "261023"),
    ],
)
def test_unidentifiable_or_unexpected_versions(
    bin_dir, monkeypatch, version, options, reason, text
):
    monkeypatch.setenv("FAKE_LASTOOLS_VERSION", version)
    availability = LastoolsAdapter(bin_dir=os.fspath(bin_dir), **options).probe(LINUX)
    assert availability.reason is reason
    assert text in availability.detail and SECRET not in availability.detail


@needs_posix
def test_expected_version_accepts_the_matching_build(bin_dir):
    adapter = _adapter(bin_dir, expected_version=FAKE_BUILD)
    assert adapter.identify().details["expected_version"] == FAKE_BUILD


@needs_posix
def test_mixed_builds_are_refused(tmp_path):
    mixed = make_fake_lastools(tmp_path / "bin", ("lascanopy64",), build="260914")
    make_fake_lastools(mixed, ("lasgrid64",), build="261023")
    availability = LastoolsAdapter(bin_dir=os.fspath(mixed)).probe(LINUX)
    assert availability.reason is UnavailableReason.MISCONFIGURED
    assert "mixed LAStools builds" in availability.detail


@pytest.mark.parametrize(
    ("text", "build"),
    [
        ("LAStools lascanopy (by info@rapidlasso.de) version 260914 (non-profit)", "260914"),
        ("LAStools lasgrid (by info@rapidlasso.de) version 261023 (unlicensed)\n", "261023"),
        ("note\r\nLAStools (by martin@rapidlasso.com) version 140301 (academic)\r\n", "140301"),
        (f"LAStools lasgrid (by x) version 260914 (licensed to {SECRET})", "260914"),
    ],
)
def test_parse_version_keeps_only_the_build(text, build):
    assert parse_version(text) == build


@pytest.mark.parametrize(
    "text", ["", "usage: lasgrid", "LAStools a version 1\nLAStools b version 1"]
)
def test_parse_version_rejects_unidentifiable_output(text):
    with pytest.raises(VersionError):
        parse_version(text)


def test_query_version_of_missing_executable(tmp_path):
    with pytest.raises(VersionError, match="cannot start"):
        query_version(tmp_path / "lasgrid64", 5)


# -- height cutoff --------------------------------------------------------------------


@pytest.mark.parametrize(
    ("minimum", "expected"),
    [
        # Golden values from R: format(min - max(1, |min| * eps * 4), digits = 17, ...).
        (95.37, "94.370000000000005"),
        (0.0, "-1"),
        (-12.25, "-13.25"),
        (1e15, "999999999999999"),
        (-3.7e14, "-370000000000001"),
        (253.123456789, "252.12345678899999"),
        (0.001, "-0.999"),
        (100.0, "99"),
        (1234567.891, "1234566.8910000001"),
        (2.5e-7, "-0.99999974999999997"),
        (-0.5, "-1.5"),
        (9007199254740993.0, "9007199254740984"),
        (312.84999999999997, "311.84999999999997"),
    ],
)
def test_height_cutoff_matches_r_golden_values(minimum, expected):
    assert height_cutoff([minimum, minimum + 10]) == expected
    assert float(expected) < minimum


@pytest.mark.parametrize("values", [[], [float("nan")], [1.0, float("inf")], ["1"]])
def test_height_cutoff_needs_finite_values(values):
    with pytest.raises(ValueError):
        height_cutoff(values)


def test_format_r17_rejects_non_finite():
    with pytest.raises(ValueError):
        format_r17(float("inf"))


def test_inputs_height_cutoff_reads_every_header(layout, tmp_path):
    assert inputs_height_cutoff(sorted(layout["inputs"].glob("*.las"))) == "-13.25"
    corrupt = tmp_path / "corrupt.las"
    corrupt.write_bytes(b"not a las file")
    with pytest.raises(ValueError, match="cannot read the LAS header"):
        inputs_height_cutoff([corrupt])


@needs_posix
def test_unreadable_header_fails_planning_instead_of_skipping(bin_dir, layout):
    (layout["inputs"] / "broken.las").write_bytes(b"LASF truncated")
    adapter = _adapter(bin_dir)
    with pytest.raises(AdapterContractError, match="LAS header"):
        plan_invocation(adapter, _invocation(layout), availability=adapter.probe(PLATFORM))


# -- command construction -------------------------------------------------------------


@needs_posix
def test_pixel_metrics_command_reproduces_the_legacy_call(bin_dir, layout):
    adapter = _adapter(bin_dir)
    inv = _invocation(layout, threads=3, epsg=2959)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    west, east = (os.fspath(layout["inputs"] / n) for n in ("east;$tile.las", "west tile.las"))
    assert plan.command.argv == (
        os.fspath(bin_dir / "lascanopy64"),
        "-i", west, east, "-step", "20", "-avg", "-odir", os.fspath(layout["out"]),
        "-otif", "-cores", "3", "-buffered", "20", "-use_orig_bb",
        "-height_cutoff", "-13.25", "-grid_ll", "0", "0", "-no_data", "-9999", "-no_kml",
        "-epsg", "2959", "-fail",
    )  # fmt: skip
    assert plan.command.cwd == layout["work"]
    assert plan.command.accepted_exceptions == ()
    assert plan.command.config_files == ()
    assert plan.metadata["height_cutoff"] == "-13.25"
    assert plan.metadata["accepted_exception_policy"] is None
    assert plan.metadata["legacy_template_sha256"] == WORKLOADS["pixel_metrics_1a"].template_sha256
    assert [a.path.name for a in plan.expected_outputs] == [
        "east;$tile_avg.tif",
        "west tile_avg.tif",
    ]
    assert all(a.kind is ArtifactKind.RASTER and a.required for a in plan.expected_outputs)


@needs_posix
def test_dsm_command_reproduces_the_legacy_call(bin_dir, layout):
    adapter = _adapter(bin_dir)
    plan = plan_invocation(
        adapter,
        _invocation(layout, "generate_DSM1", threads=1),
        availability=adapter.probe(PLATFORM),
    )
    argv = plan.command.argv
    assert argv[0] == os.fspath(bin_dir / "lasgrid64")
    assert argv[1:4] == ("-i", *(os.fspath(p) for p in sorted(layout["inputs"].glob("*.las"))))
    assert argv[4:] == (
        "-highest", "-otif", "-odir", os.fspath(layout["out"]), "-step", "1", "-cores", "1",
        "-buffered", "20", "-use_orig_bb", "-no_data", "-9999", "-no_kml", LICENCE_FLAG,
    )  # fmt: skip
    assert "-epsg" not in argv and plan.metadata["height_cutoff"] is None
    assert [a.path.name for a in plan.expected_outputs] == ["east;$tile.tif", "west tile.tif"]


@needs_posix
def test_thread_environment_follows_legacy_harness(bin_dir, layout):
    adapter = _adapter(bin_dir)
    env = adapter.build_command(_invocation(layout, threads=4)).env
    assert set(env) == set(LEGACY_THREAD_VARIABLES)
    assert env["OMP_NUM_THREADS"] == env["OMP_THREAD_LIMIT"] == "4"
    assert all(v == "1" for k, v in env.items() if k not in ("OMP_NUM_THREADS", "OMP_THREAD_LIMIT"))


@needs_posix
def test_cutoff_exception_is_declared_only_when_opted_in_and_applicable(bin_dir, layout):
    default = _adapter(bin_dir)
    opted = _adapter(bin_dir, accept_known_cutoff_warning=True)
    assert default.build_command(_invocation(layout)).accepted_exceptions == ()
    assert opted.build_command(_invocation(layout)).accepted_exceptions == (CUTOFF_EXCEPTION,)
    assert opted.build_command(_invocation(layout, "generate_DSM1")).accepted_exceptions == ()
    assert opted.invocation_metadata(_invocation(layout))["accepted_exception_policy"] == (
        CUTOFF_POLICY
    )
    assert opted.identify().details["accept_known_cutoff_warning"] is True


@needs_posix
@pytest.mark.parametrize(
    ("parameters", "text"),
    [
        ({"height_cutoff": 2.0}, "derived from the input headers"),
        ({"buffer": 10}, "does not accept parameter"),
        ({"epsg": "2959"}, "positive integer"),
        ({"epsg": True}, "positive integer"),
        ({"epsg": 0}, "positive integer"),
    ],
)
def test_invalid_parameters_are_unsupported(bin_dir, layout, parameters, text):
    adapter = _adapter(bin_dir)
    plan = plan_invocation(
        adapter, _invocation(layout, **parameters), availability=adapter.probe(PLATFORM)
    )
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and text in plan.reason


@needs_posix
def test_non_las_and_colliding_inputs_are_unsupported(bin_dir, layout):
    adapter = _adapter(bin_dir)
    availability = adapter.probe(PLATFORM)
    for names, text in (
        (("tile.txt",), "LAS/LAZ point clouds only"),
        (("a.las", "A.laz"), "would write the same output"),
    ):
        inv = Invocation(
            "generate_DSM1",
            tuple(layout["inputs"] / n for n in names),
            layout["out"],
            layout["work"],
        )
        plan = plan_invocation(adapter, inv, availability=availability)
        assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and text in plan.reason


@needs_posix
def test_windows_command_line_limit(bin_dir, layout):
    adapter = _adapter(bin_dir)
    long_dir = layout["inputs"] / ("very long directory name " * 8)
    inputs = tuple(long_dir / f"tile {i:05d}.laz" for i in range(200))
    inv = Invocation("generate_DSM1", inputs, layout["out"], layout["work"])
    assert len(adapter.build_command(inv).argv) == 217  # no limit on POSIX
    adapter.windows = True  # as probe(WINDOWS) would set it
    with pytest.raises(UnsupportedCombinationError, match="exceeds the Windows limit"):
        adapter.build_command(inv)


@needs_posix
def test_unknown_workload_is_unsupported(bin_dir, layout):
    adapter = _adapter(bin_dir)
    plan = plan_invocation(
        adapter, _invocation(layout, "generate_DEM"), availability=adapter.probe(PLATFORM)
    )
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "blast2dem64" in plan.reason


# -- interpretation (direct) ----------------------------------------------------------


def _process(tmp_path: Path, argv, exit_code=0, stdout="", stderr="") -> ProcessResult:
    (tmp_path / "stdout.log").write_text(stdout, encoding="utf-8")
    (tmp_path / "stderr.log").write_text(stderr, encoding="utf-8")
    return ProcessResult(
        outcome=ProcessOutcome.EXITED,
        argv=tuple(argv),
        cwd=tmp_path,
        stdout_path=tmp_path / "stdout.log",
        stderr_path=tmp_path / "stderr.log",
        environment={},
        pid=4242,
        exit_code=exit_code,
        start=Instant(1_000, 1_000_000_000),
        end=Instant(9_000_001_000, 10_000_000_000),
    )


def _warning(path: str, cutoff: str, low: float = 1.5) -> str:
    return (
        f"WARNING: z coordinates range from {low} to 99.5 for '{path}'\n"
        f"         all values are above the height_cutoff {cutoff}.\n"
        f"         {CUTOFF_SUGGESTION}\n"
    )


def _outputs(layout, workload="pixel_metrics_1a"):
    adapter = LastoolsAdapter()
    for artifact in adapter.expected_outputs(_invocation(layout, workload)):
        artifact.path.write_bytes(b"II*\x00")


@needs_posix
def test_interpret_accepts_cutoff_block_only_for_lascanopy_metrics(bin_dir, layout, tmp_path):
    adapter = _adapter(bin_dir, accept_known_cutoff_warning=True)
    inv = _invocation(layout)
    argv = adapter.build_command(inv).argv
    stderr = "".join(_warning(os.fspath(p), "-13.25") for p in inv.inputs)
    _outputs(layout)
    result = adapter.interpret(inv, _process(tmp_path, argv, 1, stderr=stderr))
    assert result.failure_reason is None
    assert result.accepted_exception.name == CUTOFF_EXCEPTION
    assert result.accepted_exception.exit_code == 1
    assert result.accepted_exception.policy == CUTOFF_POLICY
    assert len(result.accepted_exception.evidence) == 6
    assert result.details["accepted_warning_blocks"] == 2

    dsm = _invocation(layout, "generate_DSM1")
    _outputs(layout, "generate_DSM1")
    dsm_argv = adapter.build_command(dsm).argv
    refused = adapter.interpret(dsm, _process(tmp_path, dsm_argv, 1, stderr=stderr))
    assert refused.accepted_exception is None and "status 1" in refused.failure_reason


@pytest.mark.parametrize(
    ("argv_cutoff", "warning_cutoff", "low", "path", "known"),
    [
        ("-13.25", "-13.25", 1.5, "IN", True),
        ("-13.25", "-13.25", -20.0, "IN", False),  # range not above the cutoff
        ("-13.25", "-14.25", 1.5, "IN", False),  # a different cutoff was reported
        ("-14.25", "-14.25", 1.5, "IN", False),  # argv cutoff is not the derived one
        ("-13.25", "-13.25", 1.5, "/other.las", False),  # not a supplied input
    ],
)
def test_known_cutoff_warning_rules(argv_cutoff, warning_cutoff, low, path, known):
    supplied = "/data/west tile.las"
    argv = ["/opt/bin/lascanopy64.exe", "-i", supplied, "-height_cutoff", argv_cutoff]
    lines = _warning(supplied if path == "IN" else path, warning_cutoff, low).splitlines()
    assert bool(known_cutoff_warnings(lines, argv, [supplied], "-13.25")) is known


def test_known_cutoff_warning_requires_lascanopy_and_one_cutoff():
    supplied = "/d/t.las"
    lines = _warning(supplied, "-1").splitlines()
    good = ["/x/lascanopy", "-height_cutoff", "-1"]
    assert known_cutoff_warnings(lines, good, [supplied], "-1") == [0]
    for argv in (
        ["/x/lasgrid64", "-height_cutoff", "-1"],
        ["/x/lascanopy64", "-height_cutoff", "-1", "-height_cutoff", "-1"],
        ["/x/lascanopy64", "-height_cutoff"],
        [],
    ):
        assert known_cutoff_warnings(lines, argv, [supplied], "-1") == []
    assert known_cutoff_warnings(lines[:2], good, [supplied], "-1") == []


@needs_posix
def test_failure_reasons_withhold_licensing_messages(bin_dir, layout, tmp_path):
    adapter = _adapter(bin_dir)
    inv = _invocation(layout, "generate_DSM1")
    argv = adapter.build_command(inv).argv
    stderr = f"ERROR: license for {SECRET} expired\n"
    result = adapter.interpret(inv, _process(tmp_path, argv, 1, stderr=stderr))
    assert "status 1" in result.failure_reason and "withheld" in result.failure_reason
    assert SECRET not in result.failure_reason and SECRET not in json.dumps(result.details)


# -- execution through the generic engine (fake LAStools) ------------------------------


def _execute(bin_dir, layout, monkeypatch, mode, workload="pixel_metrics_1a", threads=2, **opts):
    fake_env(monkeypatch, mode)
    adapter = _adapter(bin_dir, **opts)
    inv = _invocation(layout, workload, threads=threads, epsg=2959)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, layout["attempt"], timeout_s=60, adapter=adapter)
    record = json.loads((layout["attempt"] / "execution.json").read_text(encoding="utf-8"))
    return execution, record


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize("workload", sorted(WORKLOADS))
def test_execute_plan_success(bin_dir, layout, monkeypatch, workload):
    execution, record = _execute(bin_dir, layout, monkeypatch, "ok", workload)
    assert str(execution.execution_status) == "succeeded", record["failure_reason"]
    assert record["process"]["exit_code"] == 0 and record["accepted_exception"] is None
    assert not execution.missing_required_outputs
    details = record["interpretation"]["details"]
    assert details["raw_exit_code"] == 0 and details["output_problems"] == []
    assert record["interpretation"]["tool_timing"] is None
    assert record["process"]["argv"][-1] == LICENCE_FLAG


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize(
    ("mode", "opt_in", "exit_code", "accepted"),
    [("cutoff1", True, 1, 1), ("cutoff0", True, 0, 0)],
)
def test_opted_in_cutoff_warning_is_a_named_exception(
    bin_dir, layout, monkeypatch, mode, opt_in, exit_code, accepted
):
    execution, record = _execute(
        bin_dir, layout, monkeypatch, mode, accept_known_cutoff_warning=opt_in
    )
    assert str(execution.execution_status) == "succeeded", record["failure_reason"]
    assert record["process"]["exit_code"] == exit_code
    exception = record["accepted_exception"]
    assert exception["name"] == CUTOFF_EXCEPTION and exception["policy"] == CUTOFF_POLICY
    assert exception["exit_code"] == accepted and exception["raw_exit_code"] == exit_code
    assert len(exception["evidence"]) == 6
    assert any(CUTOFF_EXCEPTION in w for w in record["warnings"])


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize(
    ("mode", "opt_in", "workload", "phase", "text"),
    [
        ("cutoff1", False, "pixel_metrics_1a", "process", "exited with status 1"),
        ("cutoff0", False, "pixel_metrics_1a", "interpretation", "warnings outside"),
        ("cutoffbad", True, "pixel_metrics_1a", "process", "exited with status 1"),
        ("cutofftrunc", True, "pixel_metrics_1a", "process", "exited with status 1"),
        ("cutoffother", True, "pixel_metrics_1a", "process", "exited with status 1"),
        ("other", True, "pixel_metrics_1a", "interpretation", "number of cores 4"),
        ("error0", False, "generate_DSM1", "interpretation", "error despite exit status 0"),
        ("fatal3", False, "generate_DSM1", "process", "status 3: FATAL: cannot open input"),
        ("child", False, "generate_DSM1", "interpretation", "missing west tile.tif"),
        ("empty", True, "pixel_metrics_1a", "interpretation", "empty west tile_avg.tif"),
        ("unlicensed", False, "generate_DSM1", "interpretation", "unlicensed run"),
        ("crash", False, "generate_DSM1", "process", "exited with status 139"),
    ],
)
def test_execute_plan_never_hides_lastools_problems(
    bin_dir, layout, monkeypatch, mode, opt_in, workload, phase, text
):
    execution, record = _execute(
        bin_dir, layout, monkeypatch, mode, workload, accept_known_cutoff_warning=opt_in
    )
    assert str(execution.execution_status) == "failed"
    assert record["execution_status"] == "failed" and record["accepted_exception"] is None
    assert record["failure_phase"] == phase
    assert text in record["failure_reason"]


# -- campaign -------------------------------------------------------------------------


@pytest.mark.platform
@needs_posix
def test_campaign_runs_lastools_through_the_common_schema(tmp_path, bin_dir, monkeypatch):
    fake_env(monkeypatch, "cutoff1")
    base = tmp_path / "bench root"
    for name in ("data", "scratch", "state"):
        (base / name).mkdir(parents=True)
    write_las(base / "data" / "tile 1.las", [500000.5], [5000000.5], [10.0])
    write_las(base / "data" / "tile2.las", [500100.5], [5000000.5], [12.0])
    options = f'options = {{ bin_dir = "{bin_dir}", accept_known_cutoff_warning = true }}'
    config_path = write_config(
        base,
        workloads=(
            '[[workloads]]\nid = "pixel_metrics_1a"\nparameters = { epsg = 2959 }\n'
            '[[workloads]]\nid = "generate_DSM1"\n'
        ),
        tools=f'[[tools]]\nid = "lastools"\n{options}\n',
        cpu_counts="[1, 2]",
        scheduling="repetitions = 1\nwarmups = 0\ncooldown_s = 0",
    )
    config_path.write_text(
        config_path.read_text(encoding="utf-8").replace('"*.laz"', '"*.las"'), encoding="utf-8"
    )
    registry = AdapterRegistry()
    registry.register(LastoolsAdapter)
    summary = CampaignRunner(
        load_config(config_path),
        options=RunOptions(),
        registry=registry,
        state_dir=base / "state",
        git_info=lambda: {"available": False, "reason": "test"},
        report=lambda line: None,
        handle_signals=False,
    ).run()
    assert summary.statuses == {"succeeded": 4}, summary.to_record()
    rows = collect_rows(summary.campaign_path)
    for row in rows:
        assert row["tool_adapter"] == "lastools" and row["tool_version"] == FAKE_BUILD
        assert row["timing_boundary"] == "process" and row["elapsed_ns"] > 0
        assert row["threading_model"] == "native_threads"
        # The fake lascanopy/lasgrid write placeholder bytes, not GeoTIFFs: the runner's
        # validation fails them at the artifacts level, so later levels never run.
        assert (row["structural_status"], row["numerical_status"]) == ("not_run", "not_run")
        assert row["missing_required_outputs"] == 0
        if row["workload_id"] == "pixel_metrics_1a":
            assert row["exit_code"] == 1 and row["accepted_exception"] == CUTOFF_EXCEPTION
        else:
            assert row["exit_code"] == 0 and row["accepted_exception"] is None
    invocations = [
        json.loads(p.read_text(encoding="utf-8"))
        for p in (summary.campaign_path / "attempts").glob("*/invocation.json")
    ]
    declared = {i["workload_id"]: i["command"]["accepted_exceptions"] for i in invocations}
    assert declared == {"pixel_metrics_1a": [CUTOFF_EXCEPTION], "generate_DSM1": []}
    tools = (summary.campaign_path / "identity" / "tools.json").read_text(encoding="utf-8")
    assert "lascanopy64" in tools and "sha256" in tools and FAKE_BUILD in tools
    assert SECRET not in tools and "licensed to" not in tools
