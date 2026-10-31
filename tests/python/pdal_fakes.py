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
FAKE_PDAL_DRIVERS    ``ok`` | ``nomerge`` | ``notin`` (no TIN stages) | ``nocopc`` |
                     ``nohag`` (no ``filters.hag_delaunay``)
FAKE_PDAL_HAG_OPTIONS ``ok`` | ``noclass`` (no ``class`` option, PDAL < 2.6) |
                     ``nocount`` | ``broken`` (the options query fails)
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
    if os.environ.get("FAKE_PDAL_DRIVERS") != "notin":
        names += ["filters.range", "filters.delaunay", "filters.faceraster", "writers.raster"]
    if os.environ.get("FAKE_PDAL_DRIVERS") != "nocopc":
        names.append("readers.copc")
    names += ["filters.ferry", "filters.assign", "writers.las"]
    if os.environ.get("FAKE_PDAL_DRIVERS") != "nohag":
        names.append("filters.hag_delaunay")
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
if args == ["--options", "filters.hag_delaunay", "--showjson"]:
    mode = os.environ.get("FAKE_PDAL_HAG_OPTIONS", "ok")
    if mode == "broken":
        print("PDAL: options query failed", file=sys.stderr)
        sys.exit(1)
    names = ["where", "count", "allow_extrapolation", "class"]
    if mode in ("noclass", "nocount"):
        names.remove(mode[2:])
    print(json.dumps(["filters.hag_delaunay", [{"name": n, "description": ""} for n in names]]))
    sys.exit(0)
if args == ["--options", "filters.assign", "--showjson"]:
    names = ["where", "assignment", "condition", "value"]
    print(json.dumps(["filters.assign", [{"name": n, "description": ""} for n in names]]))
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
readers = [s for s in stages if s["type"] in ("readers.las", "readers.copc")]

import laspy, numpy as np, rasterio
from rasterio.transform import from_origin

xs, ys, zs, cs, counts = [], [], [], [], []
for reader in readers:
    las = laspy.read(reader["filename"])
    keep = np.ones(len(las.points), dtype=bool)
    if "bounds" in reader:  # readers.copc spatial query
        assert reader["type"] == "readers.copc", reader
        (bx0, bx1), (by0, by1) = json.loads(reader["bounds"].replace("(", "[").replace(")", "]"))
        keep = (las.x >= bx0) & (las.x <= bx1) & (las.y >= by0) & (las.y <= by1)
    xs.append(np.asarray(las.x, dtype=np.float64)[keep])
    ys.append(np.asarray(las.y, dtype=np.float64)[keep])
    zs.append(np.asarray(las.z, dtype=np.float64)[keep])
    cs.append(np.asarray(las.classification, dtype=np.int64)[keep])
    counts.append(int(keep.sum()))
x, y, z = np.concatenate(xs), np.concatenate(ys), np.concatenate(zs)
c = np.concatenate(cs)


def _box(text):
    return json.loads(text.replace("(", "[").replace(")", "]"))


if writer["type"] == "writers.las":
    # Normalization stand-in: neighbour branches keep their filters.range classes, the
    # merged points are cropped, Z becomes Z minus the Z of the nearest (2D) ground
    # point (no triangulation; plumbing only, not PDAL behaviour), then the final crop.
    names = [s["type"] for s in stages]
    assert names[-3:] == ["filters.ferry", "filters.crop", "writers.las"], names
    keep = np.ones(len(x), dtype=bool)
    start = 0
    for reader, n in zip(readers, counts):
        ranges = [s for s in stages if s["type"] == "filters.range"
                  and s.get("inputs") == [reader["tag"]]]
        for rng in ranges:
            allowed = [int(p.split("[")[1].split(":")[0]) for p in rng["limits"].split(",")]
            keep[start:start + n] &= np.isin(c[start:start + n], allowed)
        start += n
    crops = [s for s in stages if s["type"] == "filters.crop"]
    (x0, x1), (y0, y1) = _box(crops[0]["bounds"])
    keep &= (x >= x0) & (x <= x1) & (y >= y0) & (y <= y1)
    hag = next(s for s in stages if s["type"] == "filters.hag_delaunay")
    ground_classes = [hag.get("class", 2)]
    for assign in (s for s in stages if s["type"] == "filters.assign"):
        ground_classes += [int(p.split("==")[1]) for p in assign["value"].split("WHERE")[1].split("||")]
    ground = keep & np.isin(c, ground_classes)
    gx, gy, gz = x[ground], y[ground], z[ground]
    height = np.zeros(len(x))
    for i in np.flatnonzero(keep & ~ground):
        height[i] = z[i] - gz[np.argmin((gx - x[i]) ** 2 + (gy - y[i]) ** 2)]
    (x0, x1), (y0, y1) = _box(crops[-1]["bounds"])
    keep &= (x >= x0) & (x <= x1) & (y >= y0) & (y <= y1)
    arrays = []
    for reader in readers:
        las = laspy.read(reader["filename"])
        inside = np.ones(len(las.points), dtype=bool)
        if "bounds" in reader:
            (bx0, bx1), (by0, by1) = _box(reader["bounds"])
            inside = (las.x >= bx0) & (las.x <= bx1) & (las.y >= by0) & (las.y <= by1)
        arrays.append(las.points.array[inside])
        point_format = las.header.point_format
    points = laspy.PackedPointRecord(np.concatenate(arrays)[keep], point_format)
    header = laspy.LasHeader(point_format=writer["dataformat_id"],
                             version=f"{writer['major_version']}.{writer['minor_version']}")
    header.scales = [writer["scale_x"], writer["scale_y"], writer["scale_z"]]
    header.offsets = [writer["offset_x"], writer["offset_y"], writer["offset_z"]]
    out = laspy.LasData(header)
    for name in out.point_format.dimension_names:
        if name not in ("X", "Y", "Z"):
            out[name] = points[name]
    out.x, out.y, out.z = x[keep], y[keep], height[keep]
    if mode != "nooutput":
        out.write(writer["filename"], do_compress=writer["compression"] == "true")
    if mode != "nometadata":
        meta = {"stages": {
            "readers.las": [{"count": n} for n in counts],
            "writers.las": {"filename": [writer["filename"]]},
        }}
        metadata_file.write_text(json.dumps(meta), encoding="utf-8")
    sys.exit(0)

if writer["type"] == "writers.raster":
    # TIN stand-in (no triangulation): crop, class filter, then every cell centre inside
    # the kept points' bounding box gets their mean Z. Plumbing only, not PDAL behaviour.
    names = [s["type"] for s in stages]
    assert names[-4:] == ["filters.range", "filters.delaunay", "filters.faceraster",
                          "writers.raster"], names
    crop = next(s for s in stages if s["type"] == "filters.crop")["bounds"]
    (x0, x1), (y0, y1) = json.loads(crop.replace("(", "[").replace(")", "]"))
    limits = next(s for s in stages if s["type"] == "filters.range")["limits"]
    keep_classes = [int(p.split("[")[1].split(":")[0]) for p in limits.split(",")]
    keep = (x >= x0) & (x <= x1) & (y >= y0) & (y <= y1) & np.isin(c, keep_classes)
    face = stages[-2]
    r, ox, oy = face["resolution"], face["origin_x"], face["origin_y"]
    w, h = face["width"], face["height"]
    data = np.full((h, w), float(writer["nodata"]))
    if keep.any():
        cx = ox + (np.arange(w) + 0.5) * r
        cy = oy + (h - np.arange(h) - 0.5) * r
        inside = ((cy[:, None] >= y[keep].min()) & (cy[:, None] <= y[keep].max())
                  & (cx[None, :] >= x[keep].min()) & (cx[None, :] <= x[keep].max()))
        data[inside] = z[keep].mean()
    if mode != "nooutput":
        crs = readers[0].get("override_srs")
        with rasterio.open(
            writer["filename"], "w", driver="GTiff", width=w, height=h, count=1,
            dtype="float64", nodata=writer["nodata"], crs=crs,
            transform=from_origin(ox, oy + r * h, r, r),
        ) as dst:
            dst.write(data, 1)
            dst.set_band_description(1, "faceraster")
    if mode != "nometadata":
        meta = {"stages": {
            **{f"readers.las{'' if k == 0 else k}": {"count": n} for k, n in enumerate(counts)},
            "writers.raster": {"filename": [writer["filename"]]},
        }}
        metadata_file.write_text(json.dumps(meta), encoding="utf-8")
    sys.exit(0)

assert writer["type"] == "writers.gdal" and writer["binmode"] is True
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
    for key in ("VERSION", "OPTIONS", "DRIVERS", "HAG_OPTIONS", "LOG"):
        monkeypatch.delenv(f"FAKE_PDAL_{key}", raising=False)
    for key, value in extra.items():
        monkeypatch.setenv(f"FAKE_PDAL_{key.upper()}", value)
