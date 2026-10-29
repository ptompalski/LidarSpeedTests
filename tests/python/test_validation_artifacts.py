"""Tests for the stdlib artifact inventory."""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

from lidarbench.validation.artifacts import take_inventory
from lidarbench.workloads import default_suite_dir, load_suite

TIFF = b"II*\x00" + bytes(16)
CONTRACT = load_suite(default_suite_dir()).get("pixel_metrics_1a")


def _setup(tmp_path: Path, stems=("a", "b")):
    inputs = [tmp_path / "in" / f"{s}.laz" for s in stems]
    out = tmp_path / "out"
    out.mkdir()
    return inputs, out


def test_complete_inventory_with_ignored_sidecars(tmp_path: Path):
    inputs, out = _setup(tmp_path)
    for name in ("a.tif", "b.tif", "a.tif.aux.xml", "mosaic.vrt"):
        (out / name).write_bytes(TIFF)
    inv = take_inventory(CONTRACT, CONTRACT.implementation("lidR"), inputs, out)
    assert inv.complete, inv.problems
    assert [a.name for a in inv.expected] == ["a.tif", "b.tif"]
    assert inv.ignored == ("a.tif.aux.xml", "mosaic.vrt")
    assert inv.to_record()["expected"][0]["bytes"] == len(TIFF)


def test_missing_empty_bad_signature_unexpected(tmp_path: Path):
    inputs, out = _setup(tmp_path, ("a", "b", "c"))
    (out / "b.tif").write_bytes(b"")
    (out / "c.tif").write_bytes(b"PNG\x00junk")
    (out / "halo_a.tif").write_bytes(TIFF)  # extra primary product
    sub = out / "sub"
    sub.mkdir()
    (sub / "a.tif").write_bytes(TIFF)  # right name, wrong place
    inv = take_inventory(CONTRACT, CONTRACT.implementation("lidR"), inputs, out)
    problems = "\n".join(inv.problems)
    assert "a.tif: missing" in problems
    assert "b.tif: zero bytes" in problems
    assert "c.tif: content is not a .tif file" in problems
    assert "unexpected file: halo_a.tif" in problems
    assert "unexpected file: sub/a.tif" in problems
    assert not inv.complete


def test_tool_specific_names(tmp_path: Path):
    inputs, out = _setup(tmp_path, ("a",))
    (out / "a.tif").write_bytes(TIFF)
    inv = take_inventory(CONTRACT, CONTRACT.implementation("lastools"), inputs, out)
    assert "a_avg.tif: missing" in inv.problems
    assert "unexpected file: a.tif" in inv.problems


def test_duplicate_stems_and_missing_directory(tmp_path: Path):
    inputs = [tmp_path / "x" / "a.las", tmp_path / "y" / "A.laz"]
    inv = take_inventory(CONTRACT, CONTRACT.implementation("lidR"), inputs, tmp_path / "nowhere")
    text = "\n".join(inv.problems)
    assert "duplicate input tile stems" in text
    assert "output directory does not exist" in text
    empty = take_inventory(CONTRACT, CONTRACT.implementation("lidR"), [], tmp_path)
    assert "no input tiles" in "\n".join(empty.problems)


@pytest.mark.skipif(sys.platform == "win32", reason="case-sensitive file systems only")
def test_case_variants_are_duplicates(tmp_path: Path):
    inputs, out = _setup(tmp_path, ("a",))
    (out / "a.tif").write_bytes(TIFF)
    (out / "A.TIF").write_bytes(TIFF)
    if len(list(out.iterdir())) < 2:
        pytest.skip("case-insensitive file system")
    inv = take_inventory(CONTRACT, CONTRACT.implementation("lidR"), inputs, out)
    assert any("differing only by case" in p for p in inv.problems)


@pytest.mark.skipif(not hasattr(os, "symlink"), reason="no symlinks")
def test_symlinked_artifact_is_rejected(tmp_path: Path):
    inputs, out = _setup(tmp_path, ("a",))
    target = tmp_path / "elsewhere.tif"
    target.write_bytes(TIFF)
    try:
        (out / "a.tif").symlink_to(target)
    except OSError:
        pytest.skip("symlinks not permitted")
    inv = take_inventory(CONTRACT, CONTRACT.implementation("lidR"), inputs, out)
    assert any("not a regular file" in p for p in inv.problems)
