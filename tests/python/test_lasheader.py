"""Tests for the stdlib LAS/LAZ header reader."""

from __future__ import annotations

import struct
from pathlib import Path

import pytest
from validation_fixtures import X0, Y0, write_las

from lidarbench.datasets.lasheader import LasHeaderError, read_las_header


def test_reads_fixture_header(tmp_path: Path):
    cloud = write_las(tmp_path / "a.las", [X0 + 1.234, X0 + 50.0], [Y0, Y0 + 99.99], [-1.5, 300.0])
    header = read_las_header(cloud.path)
    assert header.version == (1, 2)
    assert header.point_format == 0 and not header.compressed
    assert header.point_count == 2
    assert header.bounds_xy == (min(cloud.x), min(cloud.y), max(cloud.x), max(cloud.y))
    assert header.min_xyz[2] == -1.5 and header.max_xyz[2] == 300.0
    assert header.crs_wkt is None and not header.has_geotiff_keys
    assert header.to_record()["version"] == "1.2"


def test_rejects_non_las_and_truncated(tmp_path: Path):
    junk = tmp_path / "junk.las"
    junk.write_bytes(b"NOTLAS" + bytes(300))
    with pytest.raises(LasHeaderError, match="LASF"):
        read_las_header(junk)
    short = tmp_path / "short.las"
    short.write_bytes(b"LASF" + bytes(20))
    with pytest.raises(LasHeaderError):
        read_las_header(short)
    with pytest.raises(LasHeaderError, match="cannot read"):
        read_las_header(tmp_path / "missing.las")


def test_truncated_vlr_and_bad_scale(tmp_path: Path):
    cloud = write_las(tmp_path / "a.las", [X0], [Y0], [1.0])
    data = bytearray(cloud.path.read_bytes())
    struct.pack_into("<I", data, 100, 1)  # claims one VLR that is not there
    bad = tmp_path / "vlr.las"
    bad.write_bytes(bytes(data[:227]))
    with pytest.raises(LasHeaderError, match="VLR"):
        read_las_header(bad)
    data = bytearray(cloud.path.read_bytes())
    struct.pack_into("<d", data, 131, 0.0)
    zero = tmp_path / "scale.las"
    zero.write_bytes(bytes(data))
    with pytest.raises(LasHeaderError, match="scale"):
        read_las_header(zero)


def test_las14_and_laz_written_by_laspy(tmp_path: Path):
    laspy = pytest.importorskip("laspy")
    np = pytest.importorskip("numpy")
    header = laspy.LasHeader(point_format=6, version="1.4")
    header.scales = np.array([0.01, 0.01, 0.001])
    header.offsets = np.array([X0, Y0, 0.0])
    las = laspy.LasData(header)
    las.x = np.array([X0 + 1.0, X0 + 2.0, X0 + 3.0])
    las.y = np.array([Y0 + 1.0, Y0 + 2.0, Y0 + 3.0])
    las.z = np.array([1.0, 2.0, 3.0])
    path = tmp_path / "a.las"
    las.write(path)
    parsed = read_las_header(path)
    assert parsed.version == (1, 4) and parsed.point_format == 6 and parsed.point_count == 3
    try:
        laz = tmp_path / "a.laz"
        las.write(laz)
    except Exception as exc:  # no LAZ backend installed
        pytest.skip(f"no LAZ backend: {exc}")
    compressed = read_las_header(laz)
    assert compressed.compressed and compressed.point_format == 6 and compressed.point_count == 3
