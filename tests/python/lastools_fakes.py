"""Fake LAStools installation for adapter tests (no LAStools needed).

``make_fake_lastools`` writes executable Python scripts named like the LAStools
commands. Their behaviour is selected by environment variables inherited by the
launched process.

``FAKE_LASTOOLS_VERSION`` (``-version`` queries):

========= ======================================================================
ok        one banner line with a licensee note that must never be recorded
none      prints text without a version line, exits 0
two       two version lines
fail      exits 3
========= ======================================================================

``FAKE_LASTOOLS_MODE`` (processing):

============= ==================================================================
ok            writes ``<stem><suffix>.tif`` and a ``.tfw`` per input, exits 0
cutoff1       outputs plus the documented lascanopy cutoff warning, exits 1
              (``cutoff*`` modes act like ``ok`` for commands without a cutoff)
cutoff0       the same warning with exit 0 (multi-core parent)
cutoffbad     the warning names a file that was not supplied, exits 1
cutofftrunc   the warning block is incomplete (no suggestion line), exits 1
other         outputs plus an unrelated ``WARNING:`` line, exits 0
cutoffother   cutoff warning plus an unrelated warning, exits 1
error0        outputs plus an ``ERROR:`` line, exits 0 (hidden child failure)
fatal3        ``FATAL`` line, no outputs, exits 3
child         the last tile's output is missing, exits 0 (hidden child failure)
empty         the last tile's output has zero bytes, exits 0
unlicensed    outputs plus an unlicensed notice, exits 0
crash         exits 139 without outputs
============= ==================================================================

Every invocation appends its argv (JSON) to ``FAKE_LASTOOLS_LOG`` when set.
"""

from __future__ import annotations

import stat
import sys
from pathlib import Path

FAKE_BUILD = "260914"
SECRET = "Secret Licensee GmbH key 0xDEADBEEF"

_SCRIPT = r"""
import json, os, sys
from pathlib import Path

name = Path(sys.argv[0]).name
command = name[:-4] if name.lower().endswith(".exe") else name
args = sys.argv[1:]
log = os.environ.get("FAKE_LASTOOLS_LOG")
if log:
    with open(log, "a", encoding="utf-8") as handle:
        handle.write(json.dumps({"argv0": sys.argv[0], "args": args}) + "\n")

if args == ["-version"]:
    mode = os.environ.get("FAKE_LASTOOLS_VERSION", "ok")
    tool = command[:-2] if command.endswith("64") else command
    banner = f"LAStools {tool} (by info@rapidlasso.de) version __BUILD__ (licensed to __SECRET__)"
    if mode == "fail":
        sys.exit(3)
    if mode == "none":
        print("usage: lots of help text")
        sys.exit(0)
    print(banner, file=sys.stderr)
    if mode == "two":
        print(banner, file=sys.stderr)
    sys.exit(0)

mode = os.environ.get("FAKE_LASTOOLS_MODE", "ok")
start = end = args.index("-i") + 1
while end < len(args) and not args[end].startswith("-"):
    end += 1
inputs = args[start:end]
odir = Path(args[args.index("-odir") + 1])
suffix = "_avg" if command.startswith("lascanopy") else ""
cutoff = args[args.index("-height_cutoff") + 1] if "-height_cutoff" in args else None
if mode.startswith("cutoff") and cutoff is None:
    mode = "ok"  # only lascanopy has a height cutoff

def write(path, data=b"II*\x00fake tiff"):
    path.write_bytes(data)
    path.with_suffix(".tfw").write_text("1\n0\n0\n-1\n0\n0\n")

if mode in ("fatal3", "crash"):
    print("FATAL: cannot open input" if mode == "fatal3" else "Segmentation fault", file=sys.stderr)
    sys.exit(3 if mode == "fatal3" else 139)

for i, tile in enumerate(inputs):
    last = i == len(inputs) - 1
    target = odir / f"{Path(tile).stem}{suffix}.tif"
    if mode == "child" and last:
        print(f"child process for {Path(tile).name} exited", file=sys.stderr)
        continue
    write(target, b"" if mode == "empty" and last else b"II*\x00fake tiff")
    if mode.startswith("cutoff"):
        named = "/elsewhere/other.las" if mode == "cutoffbad" else tile
        print(f"WARNING: z coordinates range from {float(cutoff) + 2.5} to 99.5 for '{named}'",
              file=sys.stderr)
        print(f"         all values are above the height_cutoff {cutoff}.", file=sys.stderr)
        if mode != "cutofftrunc":
            print("         maybe run 'lasheight -i raw.las -replace_z -o normalized.laz'.",
                  file=sys.stderr)
        else:
            print("done.", file=sys.stderr)

if mode in ("other", "cutoffother"):
    print("WARNING: number of cores 4 larger than number of files 2", file=sys.stderr)
if mode == "error0":
    print("ERROR: child failed to write raster", file=sys.stderr)
if mode == "unlicensed":
    print("LAStools is unlicensed. adding noise to output", file=sys.stderr)
sys.exit(1 if mode in ("cutoff1", "cutoffbad", "cutofftrunc", "cutoffother") else 0)
"""

posix_only = sys.platform == "win32"


def make_fake_lastools(
    bin_dir: Path,
    commands: tuple[str, ...] = ("lascanopy64", "lasgrid64", "blast2dem64"),
    exe: bool = False,
    build: str = FAKE_BUILD,
) -> Path:
    """Write fake LAStools commands into ``bin_dir`` (``.exe`` names if ``exe``)."""
    bin_dir.mkdir(parents=True, exist_ok=True)
    body = _SCRIPT.replace("__BUILD__", build).replace("__SECRET__", SECRET)
    for command in commands:
        path = bin_dir / (f"{command}.exe" if exe else command)
        path.write_text(f"#!{sys.executable}\n{body}", encoding="utf-8")
        path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return bin_dir


def fake_env(monkeypatch, mode: str = "ok", version: str = "ok") -> None:
    """Select the fake behaviour for processes launched by the test."""
    monkeypatch.setenv("FAKE_LASTOOLS_MODE", mode)
    monkeypatch.setenv("FAKE_LASTOOLS_VERSION", version)
    monkeypatch.delenv("FAKE_LASTOOLS_LOG", raising=False)
