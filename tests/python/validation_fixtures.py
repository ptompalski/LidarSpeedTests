"""Synthetic point-cloud and raster fixtures for validation tests.

The LAS writer is stdlib-only (LAS 1.2, point format 0) so fixtures do not
depend on the library used to decode them. Coordinates are quantized exactly
as a LAS writer would, and returned in their decoded (quantized) form so tests
and references see the same values.

The two-tile scene (``neighbour_scene``) covers, on a 20 m and a 1 m grid:

* a shared tile edge at X0+100 with points exactly on it, stored in the west
  tile's file; the east tile's header starts at X0+100.37, so its outward
  snapped grid begins at X0+100 and its first column is filled only by
  points from the *neighbouring* file;
* points exactly on interior cell edges, on the tiles' south (minimum-Y) and
  north edges and on corners (cell ownership conventions differ there);
* a deliberately empty 20 m cell and many empty 1 m cells (NoData, not zero);
* negative, zero, large and many-decimal Z values (unusual elevations).
"""

from __future__ import annotations

import random
import struct
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

X0 = 500000.0
Y0 = 5000000.0
XY_SCALE = 0.01
Z_SCALE = 0.001
EPSG = 2959


@dataclass(frozen=True)
class Cloud:
    """Decoded coordinates of one fixture file."""

    path: Path
    x: tuple[float, ...]
    y: tuple[float, ...]
    z: tuple[float, ...]


def _quantize(values: Sequence[float], scale: float, offset: float) -> list[int]:
    return [round((v - offset) / scale) for v in values]


def write_las(
    path: Path,
    x: Sequence[float],
    y: Sequence[float],
    z: Sequence[float],
    *,
    scale: tuple[float, float, float] = (XY_SCALE, XY_SCALE, Z_SCALE),
    offset: tuple[float, float, float] = (X0, Y0, 0.0),
    header_count: int | None = None,
    header_bounds: tuple[float, float, float, float, float, float] | None = None,
    classification: Sequence[int] | None = None,
) -> Cloud:
    """Write an uncompressed LAS 1.2 point-format-0 file; return decoded coordinates.

    ``header_count``/``header_bounds`` override the header to build corrupt files.
    ``header_bounds`` is ``(min_x, min_y, min_z, max_x, max_y, max_z)``.
    """
    if not (len(x) == len(y) == len(z)):
        raise ValueError("x, y and z must have equal length")
    qx = _quantize(x, scale[0], offset[0])
    qy = _quantize(y, scale[1], offset[1])
    qz = _quantize(z, scale[2], offset[2])
    dx = tuple(q * scale[0] + offset[0] for q in qx)
    dy = tuple(q * scale[1] + offset[1] for q in qy)
    dz = tuple(q * scale[2] + offset[2] for q in qz)
    classes = list(classification) if classification is not None else [1] * len(x)
    if dx:
        bounds = (min(dx), min(dy), min(dz), max(dx), max(dy), max(dz))
    else:
        bounds = (0.0,) * 6
    if header_bounds is not None:
        bounds = header_bounds
    count = len(qx) if header_count is None else header_count
    header = bytearray(227)
    header[0:4] = b"LASF"
    header[24], header[25] = 1, 2
    header[26:58] = b"lidarbench fixture".ljust(32, b"\0")
    header[58:90] = b"lidarbench tests".ljust(32, b"\0")
    struct.pack_into("<HHHII", header, 90, 1, 2026, 227, 227, 0)
    struct.pack_into("<BHI", header, 104, 0, 20, count)
    struct.pack_into("<5I", header, 111, count, 0, 0, 0, 0)
    struct.pack_into("<3d", header, 131, *scale)
    struct.pack_into("<3d", header, 155, *offset)
    min_x, min_y, min_z, max_x, max_y, max_z = bounds
    struct.pack_into("<6d", header, 179, max_x, min_x, max_y, min_y, max_z, min_z)
    records = bytearray()
    for i in range(len(qx)):
        # X, Y, Z, intensity, return byte (return 1 of 1), class, angle, user, source.
        records += struct.pack("<iiiHBBbBH", qx[i], qy[i], qz[i], 0, 0b001001, classes[i], 0, 0, 1)
    path.write_bytes(bytes(header) + bytes(records))
    return Cloud(path, dx, dy, dz)


def mark_copc(path: Path) -> Path:
    """Insert a COPC info VLR (``copc``/1, zero payload) as the first VLR of an
    uncompressed LAS file, so header readers see a COPC file; points are unchanged."""
    data = bytearray(path.read_bytes())
    header_size = struct.unpack_from("<H", data, 94)[0]
    offset, count = struct.unpack_from("<II", data, 96)
    vlr = struct.pack("<H16sHH32s", 0, b"copc", 1, 160, b"copc info") + bytes(160)
    struct.pack_into("<II", data, 96, offset + len(vlr), count + 1)
    path.write_bytes(bytes(data[:header_size]) + vlr + bytes(data[header_size:]))
    return path


def _grid_points(rng: random.Random, xmin: float, xmax: float, n: int) -> list[tuple[float, float]]:
    pts = []
    for _ in range(n):
        pts.append((rng.uniform(xmin, xmax), rng.uniform(Y0, Y0 + 100.0)))
    return pts


def _z(rng: random.Random) -> float:
    return round(rng.uniform(140.0, 185.0), 3)


def neighbour_scene(directory: Path, seed: int = 20261028) -> dict[str, Cloud]:
    """Write the west/east fixture tiles into ``directory``; return their clouds."""
    rng = random.Random(seed)
    directory.mkdir(parents=True, exist_ok=True)

    west: list[tuple[float, float, float]] = []
    for px, py in _grid_points(rng, X0 + 0.5, X0 + 99.5, 700):
        if X0 + 40 <= px < X0 + 60 and Y0 + 40 < py <= Y0 + 60:
            continue  # deliberately empty 20 m cell
        west.append((px, py, _z(rng)))
    west += [
        (X0, Y0 + 50.0, 151.0),  # west outer edge
        (X0 + 20.0, Y0 + 30.0, 152.25),  # interior column edge
        (X0 + 30.0, Y0 + 40.0, 153.5),  # interior row edge
        (X0 + 20.0, Y0 + 80.0, 154.125),  # interior corner
        (X0 + 100.0, Y0 + 10.0, 170.0),  # shared tile edge (east)
        (X0 + 100.0, Y0 + 70.0, 171.5),
        (X0 + 100.0, Y0 + 40.0, 172.0),  # shared edge on a row edge
        (X0 + 10.0, Y0, 160.0),  # south (minimum-Y) edge
        (X0 + 55.5, Y0, 161.0),
        (X0 + 90.0, Y0, 162.0),
        (X0, Y0, 163.0),  # south-west corner
        (X0 + 100.0, Y0, 164.0),  # south-east corner on the shared edge
        (X0 + 25.0, Y0 + 100.0, 165.0),  # north edge
        (X0 + 61.0, Y0 + 12.0, -12.5),  # negative elevation
        (X0 + 62.0, Y0 + 13.0, 0.0),  # zero elevation
        (X0 + 63.0, Y0 + 14.0, 2500.123),  # very high outlier
        (X0 + 64.25, Y0 + 15.75, 123.456),  # many decimals
        (X0 + 64.25, Y0 + 15.75, 123.457),  # duplicate XY, different Z
        (X0 + 19.99, Y0 + 19.99, 150.001),  # just inside a cell corner
        (X0 + 20.01, Y0 + 20.01, 150.002),  # just across it
    ]
    east: list[tuple[float, float, float]] = []
    for px, py in _grid_points(rng, X0 + 100.37, X0 + 199.5, 700):
        east.append((px, py, _z(rng)))
    east += [
        (X0 + 100.37, Y0 + 55.0, 175.0),  # defines the east tile's fractional min X
        (X0 + 140.0, Y0 + 60.0, 176.0),  # interior corner
        (X0 + 200.0, Y0 + 30.0, 177.0),  # isolated outer east edge
        (X0 + 150.0, Y0, 178.0),  # south edge
        (X0 + 120.0, Y0 + 100.0, 179.0),  # north edge, column edge
        (X0 + 180.0, Y0 + 50.0, -0.001),  # tiny negative
    ]
    clouds = {}
    for name, pts in (("west", west), ("east", east)):
        xs, ys, zs = zip(*pts, strict=True)
        clouds[name] = write_las(directory / f"{name}.las", xs, ys, zs)
    return clouds


def write_raster(
    path: Path,
    bands: Sequence[object],
    *,
    xmin: float,
    ymax: float,
    resolution: float,
    nodata: float | None = -9999.0,
    dtype: str = "float32",
    epsg: int | None = EPSG,
    descriptions: Sequence[str] | None = None,
    transform: tuple[float, float, float, float, float, float] | None = None,
) -> Path:
    """Write a GeoTIFF from north-up arrays (NaN cells become ``nodata``)."""
    import numpy as np
    import rasterio
    from rasterio.crs import CRS
    from rasterio.transform import Affine

    arrays = [np.asarray(b, dtype=np.float64) for b in bands]
    height, width = arrays[0].shape
    if transform is None:
        affine = Affine(resolution, 0.0, xmin, 0.0, -resolution, ymax)
    else:
        x0, dx, rx, y0, ry, dy = transform
        affine = Affine(dx, rx, x0, ry, dy, y0)
    profile = {
        "driver": "GTiff",
        "width": width,
        "height": height,
        "count": len(arrays),
        "dtype": dtype,
        "transform": affine,
        "crs": CRS.from_epsg(epsg) if epsg is not None else None,
        "nodata": nodata,
    }
    with rasterio.open(path, "w", **profile) as ds:
        for i, array in enumerate(arrays, start=1):
            data = array.copy()
            if nodata is not None:
                data[np.isnan(data)] = nodata
            ds.write(data.astype(dtype), i)
            if descriptions:
                ds.set_band_description(i, descriptions[i - 1])
    return path
