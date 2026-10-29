"""Optional format backends: GeoTIFF via rasterio and LAS/LAZ via laspy.

These only *decode* files; no processing algorithm from these packages is used
as a reference. The core never imports them at module import time: callers
probe availability first and record ``unavailable`` when a backend is missing.
Install them with ``pip install "lidarbench[validation]"``.
"""

from __future__ import annotations

import importlib
import importlib.metadata
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from lidarbench.datasets.lasheader import LasHeader, read_las_header


@dataclass(frozen=True)
class BackendStatus:
    """Availability of one optional backend."""

    name: str
    available: bool
    detail: str
    versions: dict[str, str]

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "name": self.name,
            "available": self.available,
            "detail": self.detail,
            "versions": dict(self.versions),
        }


def _probe(name: str, modules: tuple[str, ...]) -> BackendStatus:
    versions: dict[str, str] = {}
    for module in modules:
        try:
            importlib.import_module(module)
        except Exception as exc:  # ImportError, or a broken native library
            return BackendStatus(
                name,
                False,
                f"optional dependency {module!r} is not usable ({type(exc).__name__}: {exc}); "
                'install with pip install "lidarbench[validation]"',
                versions,
            )
        try:
            versions[module] = importlib.metadata.version(module)
        except importlib.metadata.PackageNotFoundError:
            versions[module] = "unknown"
    return BackendStatus(name, True, "available", versions)


def probe_raster_backend() -> BackendStatus:
    """Probe numpy + rasterio (GDAL)."""
    status = _probe("rasterio", ("numpy", "rasterio"))
    if status.available:
        import rasterio

        status.versions["gdal"] = str(rasterio.__gdal_version__)
    return status


def probe_point_backend() -> BackendStatus:
    """Probe numpy + laspy (LAZ decoding needs a laspy LAZ backend such as lazrs)."""
    return _probe("laspy", ("numpy", "laspy"))


class ReadError(RuntimeError):
    """A file could not be decoded."""


@dataclass(frozen=True)
class CrsInfo:
    """Horizontal CRS facts of a product."""

    epsg: int | None
    wkt: str | None
    linear_units: str | None
    is_projected: bool | None

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form (WKT omitted for brevity)."""
        return {
            "epsg": self.epsg,
            "linear_units": self.linear_units,
            "is_projected": self.is_projected,
            "defined": self.wkt is not None,
        }


@dataclass(frozen=True)
class RasterData:
    """A fully decoded raster. ``values`` is ``(bands, rows, cols)`` float64, NaN = NoData."""

    path: Path
    driver: str
    width: int
    height: int
    count: int
    dtypes: tuple[str, ...]
    nodata: tuple[float | None, ...]
    transform: tuple[float, float, float, float, float, float]
    crs: CrsInfo
    descriptions: tuple[str | None, ...]
    mask_flags: tuple[tuple[str, ...], ...]
    values: Any
    raw_nan_cells: tuple[int, ...]
    """Cells stored as NaN (counted as NoData) per band."""

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable metadata (no values)."""
        return {
            "driver": self.driver,
            "width": self.width,
            "height": self.height,
            "count": self.count,
            "dtypes": list(self.dtypes),
            "nodata": [
                None if v is None else ("nan" if math.isnan(v) else float(v)) for v in self.nodata
            ],
            "transform": list(self.transform),
            "crs": self.crs.to_record(),
            "descriptions": list(self.descriptions),
            "mask_flags": [list(f) for f in self.mask_flags],
            "raw_nan_cells": list(self.raw_nan_cells),
        }


def read_raster(path: Path) -> RasterData:
    """Decode every band of a raster with rasterio; NoData from file metadata becomes NaN.

    Raises:
        ReadError: The file cannot be opened or decoded.
    """
    import numpy as np
    import rasterio

    try:
        with rasterio.open(path) as ds:
            raw = ds.read()
            nodata = tuple(ds.nodatavals)
            crs = ds.crs
            crs_info = CrsInfo(None, None, None, None)
            if crs is not None:
                try:
                    epsg = crs.to_epsg()
                except Exception:
                    epsg = None
                units = None
                try:
                    units = crs.linear_units
                except Exception:
                    units = None
                crs_info = CrsInfo(epsg, crs.to_wkt(), units, bool(crs.is_projected))
            t = ds.transform
            transform = (t.c, t.a, t.b, t.f, t.d, t.e)
            descriptions = tuple(ds.descriptions)
            mask_flags = tuple(tuple(f.name for f in flags) for flags in ds.mask_flag_enums)
            driver = ds.driver
            width, height, count = ds.width, ds.height, ds.count
            dtypes = tuple(ds.dtypes)
    except Exception as exc:
        raise ReadError(f"{path.name}: cannot decode raster ({type(exc).__name__}: {exc})") from exc
    values = raw.astype(np.float64)
    nan_cells = []
    for band in range(count):
        layer = values[band]
        stored_nan = np.isnan(raw[band]) if raw.dtype.kind == "f" else np.zeros_like(layer, bool)
        nan_cells.append(int(stored_nan.sum()))
        code = nodata[band]
        if code is not None and not np.isnan(code):
            # Compare in the storage type: the sentinel is defined there, not in float64.
            layer[raw[band] == np.asarray(code).astype(raw.dtype)] = np.nan
    return RasterData(
        path=path,
        driver=driver,
        width=width,
        height=height,
        count=count,
        dtypes=dtypes,
        nodata=nodata,
        transform=transform,
        crs=crs_info,
        descriptions=descriptions,
        mask_flags=mask_flags,
        values=values,
        raw_nan_cells=tuple(nan_cells),
    )


@dataclass(frozen=True)
class PointData:
    """Decoded XYZ (float64, scaled) of one point-cloud file and its header."""

    path: Path
    header: LasHeader
    x: Any
    y: Any
    z: Any

    @property
    def count(self) -> int:
        """Decoded point count."""
        return int(self.x.shape[0])


def read_points(path: Path) -> PointData:
    """Decode all points with laspy and check them against the stdlib header.

    Raises:
        ReadError: The file cannot be decoded, or the payload count differs from
            the header count.
    """
    import laspy
    import numpy as np

    header = read_las_header(path)
    try:
        las = laspy.read(path)
        x = np.asarray(las.x, dtype=np.float64)
        y = np.asarray(las.y, dtype=np.float64)
        z = np.asarray(las.z, dtype=np.float64)
    except Exception as exc:
        raise ReadError(f"{path.name}: cannot decode points ({type(exc).__name__}: {exc})") from exc
    if not (x.shape[0] == y.shape[0] == z.shape[0] == header.point_count):
        raise ReadError(
            f"{path.name}: header declares {header.point_count} points but "
            f"{x.shape[0]} were decoded"
        )
    return PointData(path, header, x, y, z)
