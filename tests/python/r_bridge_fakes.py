"""Fake R installation for R bridge adapter tests (no R needed).

``make_fake_rscript`` writes an executable Python script that speaks the bridge
protocol of ``bridges/r/probe.R`` and ``bridges/r/runner.R``. Its behaviour is
selected with the ``FAKE_R_MODE`` environment variable (inherited by the
launched process):

========== =========================================================================
ok         writes ``<stem>.tif`` per input and a success record, exits 0
warning    writes outputs, records a workload warning, exits 3 (bridge policy)
error      records an R error in the workload, exits 1, no outputs
lie        records a failure but exits 0 (inconsistent; must not succeed)
norecord   exits 0 without writing a record
crash      prints to stderr and exits 139 without a record
drift      success record whose package versions differ from the probe
========== =========================================================================

The probe reports every requested package as installed, loadable and locked at
``1.0`` unless ``FAKE_R_PROBE`` is ``missing``, ``unloadable``, ``mismatch``,
``renv`` (activation failed), ``error``, ``garbage`` or ``silent``.
"""

from __future__ import annotations

import os
import stat
import sys
from pathlib import Path

FAKE_VERSION = "1.0"

_SCRIPT = r"""
import json, os, sys, time
from pathlib import Path

args = sys.argv[1:]
assert args[0] == "--vanilla", args
script = Path(args[1]).name
opts = dict(zip(args[2::2], args[3::2]))

if script == "probe.R":
    mode = os.environ.get("FAKE_R_PROBE", "ok")
    pkgs = opts["--packages"].split(",")
    record = {
        "schema": "lidarbench.r-bridge-probe/1",
        "r_version": "4.6.1",
        "platform": "fake-platform",
        "blas": "libblas.so",
        "lapack": "liblapack.so",
        "renv": {"enabled": True, "activated": mode != "renv", "version": "1.3.1",
                 "error": "boom" if mode == "renv" else None},
        "library_paths": ["/fake/lib one", "/fake/lib two"],
        "lockfile": {"r_version": "4.6.1", "packages": {p: "1.0" for p in pkgs}},
        "packages": {p: {"installed": "1.0", "loadable": True, "error": None} for p in pkgs},
        "error": "probe exploded" if mode == "error" else None,
    }
    if mode == "missing":
        record["packages"][pkgs[0]] = {"installed": None, "loadable": False, "error": None}
    if mode == "unloadable":
        record["packages"][pkgs[0]] = {"installed": "1.0", "loadable": False, "error": "no GDAL"}
    if mode == "mismatch":
        record["packages"][pkgs[0]]["installed"] = "0.9"
    print("startup noise")
    if mode == "garbage":
        print("LIDARBENCH_R_PROBE {not json")
    elif mode == "silent":
        print("nothing to see", file=sys.stderr)
        sys.exit(1)
    else:
        print("LIDARBENCH_R_PROBE " + json.dumps(record))
    sys.exit(0)

mode = os.environ.get("FAKE_R_MODE", "ok")
invocation = json.loads(Path(opts["--invocation"]).read_text(encoding="utf-8"))
result = Path(opts["--result"])
if mode == "crash":
    print("*** caught segfault ***", file=sys.stderr)
    sys.exit(139)
if mode == "norecord":
    sys.exit(0)
bridge_start = time.time_ns()
record = {
    "schema": "lidarbench.r-bridge-result/1",
    "tool": invocation["tool"],
    "workload_id": invocation["workload_id"],
    "warnings": [],
    "packages": {p: "1.0" for p in invocation["required_packages"]},
    "r": {"version": "4.6.1"},
    "threads": {k: os.environ.get(k) for k in ("OMP_NUM_THREADS", "OMP_THREAD_LIMIT",
                                                 "OPENBLAS_NUM_THREADS")},
    "invocation_seen": invocation,
}
packages_loaded = time.time_ns()
preload = ["sf", "terra"] + (["glue"] if invocation["tool"] == "lidR" else [])
record["preloaded_namespaces"] = {name: "1.0" for name in preload}
namespaces_preloaded = time.time_ns()
time.sleep(0.02)
start = time.time_ns()
if mode != "error":
    for path in invocation["inputs"]:
        Path(invocation["output_dir"], Path(path).stem + ".tif").write_bytes(b"II*\x00fake")
time.sleep(0.02)
end = time.time_ns()
record["timing_workload"] = {"start_wall_ns": start, "end_wall_ns": end,
                             "elapsed_s": (end - start) / 1e9}
record["workload_loaded_namespaces"] = []
record["timing"] = {"clock": "R Sys.time()", "bridge_start": bridge_start,
                    "packages_loaded": packages_loaded,
                    "namespaces_preloaded": namespaces_preloaded,
                    "bridge_end": time.time_ns()}
status, code, phase, diagnostic, exit_code = "succeeded", 0, "complete", None, 0
if mode == "warning":
    record["warnings"] = [{"phase": "workload", "message": "NAs introduced"}]
    status, code, phase = "failed", 3, "workload"
    diagnostic = "1 R warning(s) during the workload: NAs introduced"
    exit_code = 3
elif mode == "error":
    status, code, phase, diagnostic, exit_code = "failed", 1, "workload", "lidR: chunk failed", 1
elif mode == "lie":
    status, code, phase, diagnostic, exit_code = "failed", 1, "workload", "hidden error", 0
elif mode == "drift":
    record["packages"][invocation["tool"]] = "9.9"
record.update(status=status, exit_code=code, phase=phase, diagnostic=diagnostic)
result.write_text(json.dumps(record), encoding="utf-8")
sys.exit(exit_code)
"""


def make_fake_project(root: Path) -> Path:
    """Create the files an R bridge project must contain (contents irrelevant)."""
    for relative in (
        "bridges/r/runner.R",
        "bridges/r/probe.R",
        "bridges/r/bridge.R",
        "R/1_functions.R",
        "R/0_setup.R",
        "renv/activate.R",
        "renv.lock",
    ):
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(f"# fake {relative}\n", encoding="utf-8")
    return root


def make_fake_rscript(directory: Path) -> Path:
    """Write an executable fake ``Rscript`` (POSIX shebang script)."""
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / "Rscript"
    path.write_text(f"#!{sys.executable}\n{_SCRIPT}", encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


def fake_probe_record(packages: tuple[str, ...], **overrides: object) -> dict:
    """A probe record like ``probe.R`` prints, for tests that bypass the subprocess."""
    record: dict = {
        "schema": "lidarbench.r-bridge-probe/1",
        "r_version": "4.6.1",
        "platform": "fake-platform",
        "blas": "libblas.so",
        "lapack": "liblapack.so",
        "renv": {"enabled": True, "activated": True, "version": "1.3.1", "error": None},
        "library_paths": ["/fake/lib"],
        "lockfile": {"r_version": "4.6.1", "packages": dict.fromkeys(packages, FAKE_VERSION)},
        "packages": {
            p: {"installed": FAKE_VERSION, "loadable": True, "error": None} for p in packages
        },
        "error": None,
    }
    record.update(overrides)
    return record


posix_only = os.name != "posix"
