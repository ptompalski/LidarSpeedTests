"""Tests for the dataset manifest schema, inventory scanning and synthetic fixtures."""

from __future__ import annotations

import copy
import hashlib
import json
import os
from pathlib import Path

import pytest
from dataset_helpers import REPO_REGISTRY, write_tiles

from lidarbench.campaign.schedule import SplitMix64
from lidarbench.core.canonical import pretty_bytes
from lidarbench.datasets import fixtures
from lidarbench.datasets.inventory import (
    InventoryError,
    classify,
    hash_file,
    interrupted_marker,
    scan_directory,
)
from lidarbench.datasets.lasheader import read_las_header
from lidarbench.datasets.manifest import (
    DatasetManifest,
    FileEntry,
    Layout,
    License,
    Location,
    ManifestError,
    Provenance,
    content_digest,
    manifest_sha256,
    parse_manifest,
)
from lidarbench.datasets.registry import DatasetRegistry, load_manifest


def _manifest(tmp_path: Path, **overrides) -> DatasetManifest:
    folder = tmp_path / "tiles"
    write_tiles(folder)
    scan = scan_directory(folder, Layout(inputs=("*.las",)), md5=True)
    fields = {
        "dataset_id": "demo/tiles",
        "version": "v1",
        "title": "Demo",
        "kind": "derived",
        "location": Location("data", "stage/tiles"),
        "layout": Layout(inputs=("*.las",)),
        "files": scan.entries(),
        "license": License("Test", redistribution="permitted"),
        "provenance": Provenance("Test source", details={"k": [1, 2.5, None]}),
    }
    fields.update(overrides)
    return DatasetManifest(**fields)


# --------------------------------------------------------------------------- manifest


def test_manifest_round_trip_through_json(tmp_path):
    manifest = _manifest(tmp_path)
    record = manifest.to_record()
    again = parse_manifest(json.loads(pretty_bytes(record)))
    assert again == manifest
    assert again.to_record() == record
    assert manifest_sha256(again) == manifest_sha256(manifest)
    assert record["content_sha256"] == manifest.content_sha256
    assert record["file_count"] == 2 and all(f["md5"] for f in record["files"])
    assert manifest.inputs == manifest.files


def test_content_identity_ignores_metadata_and_order(tmp_path):
    a = _manifest(tmp_path)
    b = _manifest(tmp_path / "other", title="Other", version="v2")
    assert a.content_sha256 == b.content_sha256
    assert manifest_sha256(a) != manifest_sha256(b)
    assert content_digest(list(reversed(a.files))) == a.content_sha256
    changed = (FileEntry(a.files[0].path, "input", a.files[0].bytes, "0" * 64), *a.files[1:])
    assert content_digest(changed) != a.content_sha256


@pytest.mark.parametrize(
    "mutate, message",
    [
        (lambda r: r.update(extra=1), "unknown key"),
        (lambda r: r.update(schema="lidarbench.dataset/0"), "schema"),
        (lambda r: r.update(dataset_id="Bad ID"), "dataset_id"),
        (lambda r: r.update(version="has space"), "version"),
        (lambda r: r.update(kind="benchmark"), "kind"),
        (lambda r: r["files"][0].update(bytes="14227"), r"files\[0\].bytes"),
        (lambda r: r["files"][0].update(sha256="ABC"), r"files\[0\].sha256"),
        (lambda r: r["files"][0].update(path="../x.las"), r"files\[0\].path"),
        (lambda r: r["files"][0].update(role="data"), r"files\[0\].role"),
        (lambda r: r["files"][0]["las"].update(compressed=1), "compressed"),
        (lambda r: r.update(content_sha256="0" * 64), "content_sha256"),
        (lambda r: r.update(total_bytes=1), "total_bytes"),
        (lambda r: r["location"].update(root="Bad Root"), "location.root"),
        (lambda r: r["license"].update(redistribution="yes"), "license.redistribution"),
        (lambda r: r["provenance"].pop("source_citation"), "source_citation"),
        (lambda r: r["provenance"].update(records=[{"location": {}}]), "records"),
        (lambda r: r["layout"].update(las_headers="maybe"), "las_headers"),
        (lambda r: r["declared_metadata"].update(horizontal_epsg="2959"), "horizontal_epsg"),
    ],
)
def test_manifest_parse_is_strict(tmp_path, mutate, message):
    record = _manifest(tmp_path).to_record()
    broken = copy.deepcopy(record)
    mutate(broken)
    with pytest.raises(ManifestError, match=message):
        parse_manifest(broken)


def test_manifest_invariants(tmp_path):
    manifest = _manifest(tmp_path)
    with pytest.raises(ManifestError, match="sorted"):
        _manifest(tmp_path / "a", files=tuple(reversed(manifest.files)))
    entry = manifest.files[0]
    twin = FileEntry(entry.path.upper(), "index", 1, "0" * 64)
    with pytest.raises(ManifestError, match="case"):
        _manifest(tmp_path / "b", files=tuple(sorted((entry, twin), key=lambda f: f.path)))
    with pytest.raises(ManifestError, match="input"):
        _manifest(tmp_path / "c", files=(FileEntry("x.lax", "index", 1, "0" * 64),))
    assert manifest.file(entry.path) is entry
    with pytest.raises(KeyError):
        manifest.file("nope")


def test_load_manifest_checks_location_matches_id(tmp_path):
    registry = DatasetRegistry(tmp_path)
    manifest = _manifest(tmp_path / "m")
    path, created = registry.publish(manifest)
    assert created and path == tmp_path / "manifests" / "demo" / "tiles.json"
    moved = tmp_path / "manifests" / "demo" / "other.json"
    moved.write_bytes(path.read_bytes())
    with pytest.raises(ManifestError, match="does not match its location"):
        load_manifest(moved, "demo/other")


# --------------------------------------------------------------------------- inventory


def test_classify_roles_and_exclusions():
    layout = Layout(
        inputs=("*.laz",), indexes=("*.lax",), receipts=("*.laz.rds",), exclude=("*.part",)
    )
    assert classify("a.laz", layout) == "input"
    assert classify("a.lax", layout) == "index"
    assert classify("a.laz.rds", layout) == "receipt"
    assert classify("a.laz.part", layout) is None
    with pytest.raises(InventoryError, match="no declared role"):
        classify("notes.txt", layout)
    with pytest.raises(InventoryError, match="several roles"):
        classify("a.laz", Layout(inputs=("*.laz",), auxiliary=("a.*",)))


def test_scan_hashes_and_reads_headers(tmp_path):
    paths = write_tiles(tmp_path / "d")
    (tmp_path / "d" / "old.bak").write_bytes(b"x")
    scan = scan_directory(tmp_path / "d", Layout(inputs=("*.las",), exclude=("*.bak",)), md5=True)
    assert [f.path for f in scan.files] == sorted(p.name for p in paths)
    assert scan.excluded == ("old.bak",)
    first = scan.files[0]
    data = paths[0].read_bytes()
    assert first.sha256 == hashlib.sha256(data).hexdigest()
    assert first.md5 == hashlib.md5(data).hexdigest()
    assert first.las is not None and first.las.points == 50 and first.las.version == "1.2"
    quick = scan_directory(
        tmp_path / "d", Layout(inputs=("*.las",), exclude=("*.bak",)), hash_files=False
    )
    assert quick.files[0].sha256 is None
    with pytest.raises(InventoryError, match="not hashed"):
        quick.entries()


def test_scan_handles_nested_directories(tmp_path):
    write_tiles(tmp_path / "d" / "sub")
    scan = scan_directory(tmp_path / "d", Layout(inputs=("*.las",)))
    assert all(f.path.startswith("sub/") for f in scan.files)


@pytest.mark.parametrize(
    "name",
    [".x.lidarbench-partial-abc", "a.las.pending-123", ".preparation-lock"],
)
def test_scan_refuses_interrupted_preparation(tmp_path, name):
    write_tiles(tmp_path / "d")
    (tmp_path / "d" / name).mkdir()
    assert interrupted_marker(name)
    with pytest.raises(InventoryError, match="interrupted"):
        scan_directory(tmp_path / "d", Layout(inputs=("*.las",)))


def test_scan_refuses_unclassified_and_non_las_inputs(tmp_path):
    write_tiles(tmp_path / "d")
    (tmp_path / "d" / "notes.txt").write_text("x", encoding="utf-8")
    with pytest.raises(InventoryError, match="no declared role"):
        scan_directory(tmp_path / "d", Layout(inputs=("*.las",)))
    (tmp_path / "e").mkdir()
    (tmp_path / "e" / "fake.las").write_bytes(b"not a las file")
    with pytest.raises(InventoryError, match="not a readable LAS"):
        scan_directory(tmp_path / "e", Layout(inputs=("*.las",)))
    lenient = scan_directory(tmp_path / "e", Layout(inputs=("*.las",), las_headers="optional"))
    assert lenient.files[0].las is None
    none = scan_directory(tmp_path / "e", Layout(inputs=("*.las",), las_headers="none"))
    assert none.files[0].las is None


def test_scan_requires_inputs_and_directory(tmp_path):
    (tmp_path / "d").mkdir()
    (tmp_path / "d" / "a.lax").write_bytes(b"x")
    with pytest.raises(InventoryError, match="no input files"):
        scan_directory(tmp_path / "d", Layout(inputs=("*.las",), indexes=("*.lax",)))
    with pytest.raises(InventoryError, match="not found"):
        scan_directory(tmp_path / "missing", Layout())


def test_scan_refuses_non_portable_names(tmp_path):
    write_tiles(tmp_path / "d")
    (tmp_path / "d" / "trailing.").write_bytes(b"x")
    with pytest.raises(InventoryError, match="not portable"):
        scan_directory(tmp_path / "d", Layout(inputs=("*.las",), auxiliary=("*",)))


def test_scan_refuses_case_collisions(tmp_path):
    write_tiles(tmp_path / "d")
    (tmp_path / "d" / "A.las.txt").write_bytes(b"x")
    (tmp_path / "d" / "a.las.txt").write_bytes(b"y")
    if len(os.listdir(tmp_path / "d")) < 4:
        pytest.skip("case-insensitive file system")
    with pytest.raises(InventoryError, match="differ only in case"):
        scan_directory(tmp_path / "d", Layout(inputs=("*.las",), auxiliary=("*.txt",)))


def test_scan_refuses_links(tmp_path):
    write_tiles(tmp_path / "d")
    try:
        (tmp_path / "d" / "link.las").symlink_to(tmp_path / "d" / "500000_5000000.las")
    except (OSError, NotImplementedError):
        pytest.skip("symlinks not permitted")
    with pytest.raises(InventoryError, match="links"):
        scan_directory(tmp_path / "d", Layout(inputs=("*.las",)))


def test_hash_file_reports_progress(tmp_path):
    path = tmp_path / "f"
    path.write_bytes(b"a" * 10)
    seen = []
    sha, md5, size = hash_file(path, md5=False, progress=seen.append)
    assert size == 10 and md5 is None and sum(seen) == 10
    assert sha == hashlib.sha256(b"a" * 10).hexdigest()


# --------------------------------------------------------------------------- fixtures


def test_splitmix_matches_schedule_generator():
    ours, theirs = fixtures._SplitMix64(42), SplitMix64(42)
    assert [ours.next_u64() for _ in range(5)] == [theirs.next_u64() for _ in range(5)]
    assert fixtures._SplitMix64(1).below(7) == SplitMix64(1).below(7)


@pytest.mark.parametrize("dataset", ["pair", "grid"])
def test_fixture_bytes_match_committed_manifest(tmp_path, dataset):
    """The same generator yields the same bytes (and digests) on every OS."""
    registry = DatasetRegistry(REPO_REGISTRY)
    recipe = registry.recipe("synthetic-tiles-v1")
    manifest = registry.manifest(f"synthetic-tiles-v1/{dataset}")
    params = {**recipe.preparation.parameters, **recipe.dataset(manifest.dataset_id).parameters}
    paths = fixtures.generate_synthetic_tiles(tmp_path, params)
    assert [p.name for p in paths] == [f.path for f in manifest.files]
    for path, entry in zip(paths, manifest.files, strict=True):
        assert hashlib.sha256(path.read_bytes()).hexdigest() == entry.sha256
        header = read_las_header(path)
        assert header.point_count == entry.las.points


def test_fixture_golden_digest():
    name, data = fixtures.tile_bytes(fixtures.SyntheticTilesParameters(), 0, 0)
    assert name == "500000_5000000.las"
    assert len(data) == 227 + 28 * 500
    assert hashlib.sha256(data).hexdigest().startswith("2520fddb066396ac")


def test_fixture_tiles_are_valid_las(tmp_path):
    laspy = pytest.importorskip("laspy")
    path = fixtures.generate_synthetic_tiles(tmp_path, {"points_per_tile": 100, "tiles_x": 1})[0]
    las = laspy.read(path)
    assert len(las.points) == 100
    np = pytest.importorskip("numpy")
    assert set(np.asarray(las.classification).tolist()) <= {1, 2}
    assert (np.asarray(las.return_number) <= np.asarray(las.number_of_returns)).all()
    assert (np.asarray(las.return_number) >= 1).all()
    header = read_las_header(path)
    assert las.x.min() >= header.min_xyz[0] and las.x.max() <= header.max_xyz[0]


@pytest.mark.parametrize(
    "params, message",
    [
        ({"extra": 1}, "unknown"),
        ({"seed": "1"}, "seed"),
        ({"seed": True}, "seed"),
        ({"tile_size_m": 0}, "out of range"),
        ({"tiles_x": 0}, "tiles"),
        ({"tiles_x": 9, "tiles_y": 8}, "tiles"),
    ],
)
def test_fixture_parameters_are_strict(params, message):
    with pytest.raises(ValueError, match=message):
        fixtures.SyntheticTilesParameters.parse(params)


def test_fixture_generation_never_overwrites(tmp_path):
    fixtures.generate_synthetic_tiles(tmp_path, {"tiles_x": 1})
    with pytest.raises(FileExistsError):
        fixtures.generate_synthetic_tiles(tmp_path, {"tiles_x": 1})
