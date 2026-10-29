"""Fake PDAL installation for adapter tests (no PDAL needed).

``make_fake_pdal`` writes an executable Python script named ``pdal`` that answers
the probe queries and runs ``pdal pipeline <file> --metadata <file> [...]``. For
pipelines it parses the JSON the adapter generated and reproduces the
documented ``writers.gdal`` binmode behaviour with laspy and rasterio:
``floor((v - origin) / r)`` cells counted upwards from ``origin_y``, running
mean/max in float64, NoData from ``nodata``, band description ``mean``/``max``.
It is a stand-in for plumbing tests, not evidence about PDAL itself; the real
behaviour is covered by ``test_pdal_integration.py``.

Environment variables read by the launched script:

=================== =================================================================
FAKE_PDAL_VERSION    version printed by ``--version`` (default ``2.6.3``)
FAKE_PDAL_OPTIONS    ``ok`` | ``nobinmode`` | ``broken`` (``--options`` output)
FAKE_PDAL_DRIVERS    ``ok`` | ``nomerge``
FAKE_PDAL_READER_THREADS ``absent``: ``readers.las`` lists no ``threads`` option
FAKE_PDAL_MODE       ``ok`` | ``fail`` (exit 1, ``PDAL:`` error) | ``warning`` (exit 0
                     with a warning line) | ``error0`` (exit 0 with an error line) |
                     ``nooutput`` (exit 0, no raster) | ``nometadata`` | ``failsecond``
                     (only the second pipeline fails)
FAKE_PDAL_LOG        file that receives one JSON line per invocation (argv)
=================== =================================================================
"""

from __future__ import annotations

import stat
import sys
from pathlib import Path

_SCRIPT = r"""
import json, math, os, sys
from pathlib import Path

args = sys.argv[1:]
log = os.environ.get("FAKE_PDAL_LOG")
if log:
    with open(log, "a", encoding="utf-8") as handle:
        handle.write(json.dumps({"args": args, "cwd": os.getcwd()}) + "\n")

if args == ["--version"]:
    line = "-" * 80
    print(f"{line}\npdal {os.environ.get('FAKE_PDAL_VERSION', '2.6.3')} (git-version: abc123)\n{line}\n")
    sys.exit(0)
if args == ["--drivers", "--showjson"]:
    names = ["readers.las", "filters.merge", "writers.gdal", "filters.crop"]
    if os.environ.get("FAKE_PDAL_DRIVERS") == "nomerge":
        names.remove("filters.merge")
    print(json.dumps([{"name": n, "description": "", "link": "", "extensions": []} for n in names]))
    sys.exit(0)
if args == ["--options", "writers.gdal", "--showjson"]:
    mode = os.environ.get("FAKE_PDAL_OPTIONS", "ok")
    if mode == "broken":
        print("not json")
        sys.exit(0)
    names = ["resolution", "radius", "power", "gdaldriver", "gdalopts", "output_type",
             "data_type", "window_size", "nodata", "dimension", "bounds", "origin_x",
             "origin_y", "width", "height", "override_srs", "default_srs", "metadata",
             "pdal_metadata", "binmode", "allow_empty"]
    if mode == "nobinmode":
        names.remove("binmode")
    print(json.dumps(["writers.gdal", [{"name": n, "description": ""} for n in names]]))
    sys.exit(0)
if args == ["--options", "readers.las", "--showjson"]:
    names = ["filename", "spatialreference", "extra_dims", "compression", "ignore_vlr",
             "start", "nosrs", "threads", "use_eb_vlr"]
    if os.environ.get("FAKE_PDAL_READER_THREADS") == "absent":
        names.remove("threads")
    print(json.dumps(["readers.las", [{"name": n, "description": ""} for n in names]]))
    sys.exit(0)
if not args or args[0] != "pipeline":
    print("PDAL: unknown command", file=sys.stderr)
    sys.exit(1)

pipeline_file = Path(args[1])
metadata_file = Path(args[args.index("--metadata") + 1])
mode = os.environ.get("FAKE_PDAL_MODE", "ok")
if mode == "failsecond":
    mode = "fail" if pipeline_file.name == "pipeline-0002.json" else "ok"
if mode == "fail":
    print("PDAL: readers.las: Couldn't read LAS header. File size insufficient.", file=sys.stderr)
    sys.exit(1)

stages = json.loads(pipeline_file.read_text(encoding="utf-8"))["pipeline"]
writer = stages[-1]
assert writer["type"] == "writers.gdal" and writer["binmode"] is True
readers = [s for s in stages if s["type"] == "readers.las"]

import laspy, numpy as np, rasterio
from rasterio.transform import from_origin

xs, ys, zs, counts = [], [], [], []
for reader in readers:
    las = laspy.read(reader["filename"])
    xs.append(np.asarray(las.x, dtype=np.float64))
    ys.append(np.asarray(las.y, dtype=np.float64))
    zs.append(np.asarray(las.z, dtype=np.float64))
    counts.append(len(las.points))
x, y, z = np.concatenate(xs), np.concatenate(ys), np.concatenate(zs)
r, ox, oy = writer["resolution"], writer["origin_x"], writer["origin_y"]
w, h = writer["width"], writer["height"]
count = np.zeros((h, w))
value = np.full((h, w), np.nan)
for px, py, pz in zip(x, y, z):
    i, j = math.floor((px - ox) / r), math.floor((py - oy) / r)
    if not (0 <= i < w and 0 <= j < h):
        continue
    row = h - j - 1
    count[row, i] += 1
    if writer["output_type"] == "max":
        value[row, i] = pz if np.isnan(value[row, i]) else max(value[row, i], pz)
    else:
        mean = 0.0 if np.isnan(value[row, i]) else value[row, i]
        value[row, i] = mean + (pz - mean) / count[row, i]
data = np.where(np.isnan(value), writer["nodata"], value)
if mode != "nooutput":
    crs = writer.get("override_srs")
    with rasterio.open(
        writer["filename"], "w", driver="GTiff", width=w, height=h, count=1,
        dtype="float64", nodata=writer["nodata"], crs=crs,
        transform=from_origin(ox, oy + r * h, r, r),
    ) as dst:
        dst.write(data, 1)
        dst.set_band_description(1, writer["output_type"])
if mode != "nometadata":
    meta = {"stages": {
        **{f"readers.las{'' if k == 0 else k}": {"count": c} for k, c in enumerate(counts)},
        "writers.gdal": {"filename": [writer["filename"]]},
    }}
    metadata_file.write_text(json.dumps(meta), encoding="utf-8")
if mode == "warning":
    print("(pdal pipeline writers.gdal Warning) Unable to set SRS", file=sys.stderr)
if mode == "error0":
    print("(pdal pipeline Error) GDAL failure (1) something odd", file=sys.stderr)
sys.exit(0)
"""

posix_only = sys.platform == "win32"


def make_fake_pdal(bin_dir: Path, name: str = "pdal") -> Path:
    """Write the fake ``pdal`` into ``bin_dir`` and return its path."""
    bin_dir.mkdir(parents=True, exist_ok=True)
    path = bin_dir / name
    path.write_text(f"#!{sys.executable}\n{_SCRIPT}", encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


def fake_env(monkeypatch, mode: str = "ok", **extra: str) -> None:
    """Select the fake behaviour for processes launched by the test."""
    monkeypatch.setenv("FAKE_PDAL_MODE", mode)
    for key in ("VERSION", "OPTIONS", "DRIVERS", "LOG"):
        monkeypatch.delenv(f"FAKE_PDAL_{key}", raising=False)
    for key, value in extra.items():
        monkeypatch.setenv(f"FAKE_PDAL_{key.upper()}", value)
