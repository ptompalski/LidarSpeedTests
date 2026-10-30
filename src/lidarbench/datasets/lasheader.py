"""Stdlib-only reader for the LAS/LAZ public header and VLR directory.

The public header is uncompressed in both LAS and LAZ files, so tile bounds,
point counts, scales and offsets are available without any LiDAR library.
Validation uses these header bounds to define each tile's nominal extent and
cross-checks the header count against the decoded payload.
"""

from __future__ import annotations

import os
import struct
from dataclasses import dataclass
from pathlib import Path
from typing import Any

_MIN_HEADER = 227
_VLR_HEADER = 54
WKT_RECORD_ID = 2112
GEOKEY_RECORD_ID = 34735


class LasHeaderError(ValueError):
    """The file is not a readable LAS/LAZ file."""


@dataclass(frozen=True)
class LasHeader:
    """Facts from a LAS/LAZ public header."""

    path: Path
    version: tuple[int, int]
    point_format: int
    compressed: bool
    point_record_length: int
    point_count: int
    scale: tuple[float, float, float]
    offset: tuple[float, float, float]
    min_xyz: tuple[float, float, float]
    max_xyz: tuple[float, float, float]
    header_size: int
    offset_to_points: int
    vlr_count: int
    crs_wkt: str | None
    has_geotiff_keys: bool
    copc: bool = False
    """COPC (cloud-optimized point cloud): the first VLR is the ``copc`` info record,
    so the file carries its own spatial index (no ``.lax`` needed)."""

    @property
    def bounds_xy(self) -> tuple[float, float, float, float]:
        """``(min_x, min_y, max_x, max_y)``."""
        return (self.min_xyz[0], self.min_xyz[1], self.max_xyz[0], self.max_xyz[1])

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable summary."""
        return {
            "file": self.path.name,
            "version": f"{self.version[0]}.{self.version[1]}",
            "point_format": self.point_format,
            "compressed": self.compressed,
            "point_count": self.point_count,
            "scale": list(self.scale),
            "offset": list(self.offset),
            "min": list(self.min_xyz),
            "max": list(self.max_xyz),
            "crs": "wkt" if self.crs_wkt else ("geotiff" if self.has_geotiff_keys else None),
            **({"copc": True} if self.copc else {}),
        }


def read_las_header(path: str | os.PathLike[str]) -> LasHeader:
    """Read the public header and VLR directory of a LAS 1.0-1.4 or LAZ file.

    Raises:
        LasHeaderError: The file is missing, truncated or not LAS/LAZ.
    """
    file = Path(path)
    try:
        with open(file, "rb") as handle:
            head = handle.read(375)
            if len(head) < _MIN_HEADER or head[:4] != b"LASF":
                raise LasHeaderError(f"{file}: not a LAS/LAZ file (missing LASF signature)")
            major, minor = head[24], head[25]
            if major != 1 or minor > 4:
                raise LasHeaderError(f"{file}: unsupported LAS version {major}.{minor}")
            header_size, offset_to_points, vlr_count = struct.unpack_from("<HII", head, 94)
            pdf, record_length, legacy_count = struct.unpack_from("<BHI", head, 104)
            scale = struct.unpack_from("<3d", head, 131)
            offset = struct.unpack_from("<3d", head, 155)
            max_x, min_x, max_y, min_y, max_z, min_z = struct.unpack_from("<6d", head, 179)
            count = legacy_count
            if minor >= 4:
                if len(head) < 375 or header_size < 375:
                    raise LasHeaderError(f"{file}: truncated LAS 1.4 header")
                (count64,) = struct.unpack_from("<Q", head, 247)
                if legacy_count not in (0, count64):
                    raise LasHeaderError(f"{file}: legacy and 64-bit point counts disagree")
                count = count64
            if header_size < _MIN_HEADER or offset_to_points < header_size:
                raise LasHeaderError(f"{file}: inconsistent header size/point offset")
            crs_wkt: str | None = None
            geotiff = False
            copc = False
            handle.seek(header_size)
            for index in range(vlr_count):
                raw = handle.read(_VLR_HEADER)
                if len(raw) < _VLR_HEADER:
                    raise LasHeaderError(f"{file}: truncated VLR directory")
                user_id = raw[2:18].rstrip(b"\0").decode("ascii", "replace")
                record_id, length = struct.unpack_from("<HH", raw, 18)
                body = handle.read(length)
                if len(body) < length:
                    raise LasHeaderError(f"{file}: truncated VLR body")
                if index == 0 and user_id == "copc" and record_id == 1:
                    copc = True
                if user_id == "LASF_Projection" and record_id == WKT_RECORD_ID:
                    crs_wkt = body.rstrip(b"\0").decode("utf-8", "replace") or None
                elif user_id == "LASF_Projection" and record_id == GEOKEY_RECORD_ID:
                    geotiff = True
    except OSError as exc:
        raise LasHeaderError(f"{file}: cannot read: {exc}") from exc
    if any(s <= 0 for s in scale):
        raise LasHeaderError(f"{file}: non-positive coordinate scale {scale}")
    return LasHeader(
        path=file,
        version=(major, minor),
        point_format=pdf & 0x3F,
        compressed=bool(pdf & 0x80),
        point_record_length=record_length,
        point_count=count,
        scale=(scale[0], scale[1], scale[2]),
        offset=(offset[0], offset[1], offset[2]),
        min_xyz=(min_x, min_y, min_z),
        max_xyz=(max_x, max_y, max_z),
        header_size=header_size,
        offset_to_points=offset_to_points,
        vlr_count=vlr_count,
        crs_wkt=crs_wkt,
        has_geotiff_keys=geotiff,
        copc=copc,
    )
