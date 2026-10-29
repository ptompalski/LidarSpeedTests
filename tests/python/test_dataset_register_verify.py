"""Tests for recipes, registration, the registry and verification."""

from __future__ import annotations

import copy
import json
from pathlib import Path

import pytest
from dataset_helpers import DERIVED_RECIPE, REPO_REGISTRY, demo_layout, make_registry, write_tiles

from lidarbench.datasets import legacy_petawawa
from lidarbench.datasets.inventory import hash_file
from lidarbench.datasets.manifest import ManifestError, manifest_sha256
from lidarbench.datasets.recipes import load_recipe, parse_recipe
from lidarbench.datasets.register import RegistrationError, register_dataset
from lidarbench.datasets.registry import DatasetRegistry, RegistryError, default_registry_dir
from lidarbench.datasets.roots import DataRoots
from lidarbench.datasets.verify import verify_dataset


def _register(tmp_path: Path):
    registry = make_registry(tmp_path, {"demo": DERIVED_RECIPE})
    roots, tiles = demo_layout(tmp_path)
    recipe = registry.recipe("demo")
    result = register_dataset(recipe, recipe.datasets[0], registry, roots)
    return registry, roots, tiles, result


# --------------------------------------------------------------------------- recipes


def test_repository_recipes_and_manifests_are_valid():
    registry = DatasetRegistry(REPO_REGISTRY)
    recipes = {r.id: r for r in registry.recipes()}
    assert {"synthetic-tiles-v1", "petawawa-fresh-v4", "petawawa-als-2012"} <= set(recipes)
    for manifest in registry.manifests():
        recipe = recipes[manifest.recipe]
        assert manifest.version == recipe.version and manifest.kind == recipe.kind
        assert recipe.dataset(manifest.dataset_id).path == manifest.location.path
    repo = REPO_REGISTRY.parent
    for recipe in recipes.values():
        prep = recipe.preparation
        for name in (prep.entry_point, prep.config_example, prep.documentation):
            assert name is None or (repo / name).is_file(), name


def test_petawawa_recipe_pins_the_r_policy_version():
    """The recipe names the policy of R/prepare_fresh_data.R; it does not restate or alter it."""
    recipe = DatasetRegistry(REPO_REGISTRY).recipe("petawawa-fresh-v4")
    source = (REPO_REGISTRY.parent / "R" / "prepare_fresh_data.R").read_text(encoding="utf-8")
    assert f'version = "{recipe.preparation.method}"' in source
    assert recipe.version == recipe.preparation.method == "fresh-petawawa-v4"
    assert recipe.license.redistribution == "unknown" and recipe.download is None
    assert recipe.declared_metadata.horizontal_epsg == 2959
    assert recipe.declared_metadata.vertical_epsg == 5713
    for stage in recipe.preparation.stages:
        assert f'"{stage}"' in source


def test_default_registry_dir(tmp_path):
    assert default_registry_dir({"LIDARBENCH_DATASET_REGISTRY": str(tmp_path)}) == tmp_path
    assert default_registry_dir({}).name == "datasets"


@pytest.mark.parametrize(
    "edit, message",
    [
        (lambda d: d.update(extra=1), "unknown key"),
        (lambda d: d.update(id="a/b"), "id"),
        (lambda d: d["datasets"][0].update(id="other/tiles"), "must start with"),
        (lambda d: d["preparation"].update(engine="none"), "engine 'none'"),
        (lambda d: d["preparation"].pop("stages"), "entry_point and stages"),
        (lambda d: d["preparation"].update(engine="python"), "generator"),
        (lambda d: d.update(datasets=[]), "at least one"),
        (lambda d: d.update(root="Bad"), "root"),
        (lambda d: d["layout"].update(inputs=[]), "inputs"),
        (lambda d: d["layout"].update(inputs=["a/b"]), "inputs"),
    ],
)
def test_recipe_parse_is_strict(edit, message):
    import tomllib

    data = tomllib.loads(DERIVED_RECIPE)
    edit(data)
    with pytest.raises(ManifestError, match=message):
        parse_recipe(data)


def _source_recipe(**download):
    import tomllib

    data = tomllib.loads(DERIVED_RECIPE)
    data.update(kind="source")
    data["preparation"] = {"engine": "none", "method": "original"}
    data["download"] = {
        "url": "https://example.org/a.zip",
        "file": "a.zip",
        "bytes": 3,
        "md5": "0" * 32,
        "checksum_origin": "test",
        "terms_url": "https://example.org/terms",
        **download,
    }
    return data


def test_download_requires_redistributable_source():
    assert parse_recipe(_source_recipe()).download is not None
    data = _source_recipe()
    data["license"]["redistribution"] = "unknown"
    with pytest.raises(ManifestError, match="redistribution"):
        parse_recipe(data)
    with pytest.raises(ManifestError, match="https"):
        parse_recipe(_source_recipe(url="http://example.org/a.zip"))
    no_sum = _source_recipe()
    no_sum["download"].pop("md5")
    with pytest.raises(ManifestError, match="md5 or sha256"):
        parse_recipe(no_sum)


def test_recipe_file_name_must_match_id(tmp_path):
    path = tmp_path / "wrong.toml"
    path.write_text(DERIVED_RECIPE, encoding="utf-8")
    with pytest.raises(ManifestError, match=r"demo\.toml"):
        load_recipe(path)


def test_unknown_recipe_and_dataset(tmp_path):
    registry = make_registry(tmp_path, {"demo": DERIVED_RECIPE})
    with pytest.raises(RegistryError, match="known"):
        registry.recipe("nope")
    with pytest.raises(RegistryError):
        registry.manifest("demo/tiles")
    with pytest.raises(RegistryError):
        registry.recipe_for_dataset("x/y")
    assert registry.recipe_for_dataset("demo/tiles").id == "demo"
    assert DatasetRegistry(tmp_path / "empty").recipes() == []
    assert DatasetRegistry(tmp_path / "empty").manifests() == []


# --------------------------------------------------------------------------- registration


def test_register_creates_write_once_manifest(tmp_path):
    registry, roots, tiles, result = _register(tmp_path)
    manifest = result.manifest
    assert result.created and result.path.is_file()
    assert sorted(f.role for f in manifest.files) == ["index", "input", "input", "receipt"]
    assert manifest.location.to_record() == {"root": "data", "path": "stage/tiles"}
    record = manifest.provenance.records[0]
    assert record.location.path == "ready.txt" and record.md5 is not None
    assert manifest.provenance.preparation["engine"] == "r-cli"
    assert manifest.registration["registered_on"]
    assert registry.manifest("demo/tiles") == manifest
    stored = json.loads(result.path.read_text(encoding="utf-8"))
    assert all(
        "C:" not in json.dumps(v) and str(tmp_path) not in json.dumps(v) for v in stored.values()
    )

    again = register_dataset(
        registry.recipe("demo"), registry.recipe("demo").datasets[0], registry, roots
    )
    assert not again.created and again.manifest.content_sha256 == manifest.content_sha256
    before = result.path.read_bytes()
    (tiles / "extra.json").write_text("{}", encoding="utf-8")
    with pytest.raises(RegistrationError, match="new dataset version"):
        register_dataset(
            registry.recipe("demo"), registry.recipe("demo").datasets[0], registry, roots
        )
    assert result.path.read_bytes() == before


def test_register_refuses_missing_record_and_legacy_lock(tmp_path):
    registry = make_registry(tmp_path, {"demo": DERIVED_RECIPE})
    roots, _ = demo_layout(tmp_path)
    recipe = registry.recipe("demo")
    (roots.path("data") / "ready.txt").unlink()
    with pytest.raises(RegistrationError, match=r"ready\.txt"):
        register_dataset(recipe, recipe.datasets[0], registry, roots)
    (roots.path("data") / "ready.txt").write_text("ready", encoding="utf-8")
    (roots.path("data") / ".preparation-lock").mkdir()
    with pytest.raises(RegistrationError, match="active or was interrupted"):
        register_dataset(recipe, recipe.datasets[0], registry, roots)


def test_register_refuses_interrupted_staging(tmp_path):
    registry = make_registry(tmp_path, {"demo": DERIVED_RECIPE})
    roots, tiles = demo_layout(tmp_path)
    (tiles / "x.las.pending-abc").write_bytes(b"")
    recipe = registry.recipe("demo")
    with pytest.raises(RegistrationError, match="interrupted"):
        register_dataset(recipe, recipe.datasets[0], registry, roots)
    assert not registry.has_manifest("demo/tiles")


def test_derived_from_picks_up_registered_content(tmp_path):
    text = DERIVED_RECIPE + '\n[[derived_from]]\ndescription = "src"\ndataset_id = "demo/tiles"\n'
    other = text.replace('id = "demo"', 'id = "demo2"', 1).replace("demo/tiles", "demo2/tiles", 1)
    registry = make_registry(tmp_path, {"demo": DERIVED_RECIPE, "demo2": other})
    roots, _ = demo_layout(tmp_path)
    first = register_dataset(
        registry.recipe("demo"), registry.recipe("demo").datasets[0], registry, roots
    )
    recipe2 = registry.recipe("demo2")
    second = register_dataset(recipe2, recipe2.datasets[0], registry, roots)
    (link,) = second.manifest.provenance.derived_from
    assert link.dataset_id == "demo/tiles"
    assert link.content_sha256 == first.manifest.content_sha256


# --------------------------------------------------------------------------- verification


def test_verify_full_quick_and_provenance(tmp_path):
    _, roots, tiles, result = _register(tmp_path)
    manifest = result.manifest
    full = verify_dataset(manifest, roots, provenance=True)
    assert full.status == "verified" and full.ok and full.provenance_checked == 1
    assert full.bytes_hashed == manifest.total_bytes
    assert full.to_record()["manifest_sha256"] == manifest_sha256(manifest)
    quick = verify_dataset(manifest, roots, mode="quick")
    assert quick.status == "consistent" and quick.bytes_hashed == 0
    (tiles / "keep.bak").write_bytes(b"excluded")
    assert verify_dataset(manifest, roots).excluded_present == ["keep.bak"]
    with pytest.raises(ValueError):
        verify_dataset(manifest, roots, mode="slow")


def test_verify_detects_same_size_change_only_in_full_mode(tmp_path):
    _, roots, tiles, result = _register(tmp_path)
    receipt = tiles / "receipt.json"
    receipt.write_text("[]", encoding="utf-8")
    assert verify_dataset(result.manifest, roots, mode="quick").status == "consistent"
    report = verify_dataset(result.manifest, roots)
    assert report.status == "failed" and "SHA-256" in report.problems[0]


def test_verify_detects_missing_extra_resized_and_header_changes(tmp_path):
    _, roots, tiles, result = _register(tmp_path)
    (tiles / "500000_5000000.lax").unlink()
    (tiles / "new.json").write_text("{}", encoding="utf-8")
    las = tiles / "500100_5000000.las"
    data = bytearray(las.read_bytes())
    data[107] ^= 1
    las.write_bytes(bytes(data))
    problems = "\n".join(verify_dataset(result.manifest, roots, mode="quick").problems)
    assert "500000_5000000.lax: missing" in problems
    assert "new.json: not in the manifest" in problems
    assert "header facts differ" in problems
    (tiles / "receipt.json").write_text("{}   ", encoding="utf-8")
    assert "size" in "\n".join(verify_dataset(result.manifest, roots).problems)


def test_verify_detects_changed_md5_and_receipts(tmp_path):
    _, roots, _, result = _register(tmp_path)
    manifest = result.manifest
    (roots.path("data") / "ready.txt").write_text("READY", encoding="utf-8")
    report = verify_dataset(manifest, roots, provenance=True)
    assert report.status == "failed" and "receipt bytes changed" in report.problems[0]
    (roots.path("data") / "ready.txt").unlink()
    report = verify_dataset(manifest, roots, provenance=True)
    assert "missing" in report.problems[0]
    report = verify_dataset(manifest, DataRoots({"data": tmp_path / "nowhere"}))
    assert report.status == "failed" and "not found" in report.problems[0]


def test_verify_unavailable_without_root(tmp_path):
    _, _, _, result = _register(tmp_path)
    report = verify_dataset(result.manifest, DataRoots())
    assert report.status == "unavailable" and not report.ok
    assert "not configured" in report.problems[0]


# --------------------------------------------------------------------------- legacy receipts

PETAWAWA_RECIPE = (REPO_REGISTRY / "recipes" / "petawawa-fresh-v4.toml").read_text(encoding="utf-8")
ARCHIVE_RECIPE = (REPO_REGISTRY / "recipes" / "petawawa-als-2012.toml").read_text(encoding="utf-8")
TILES = ["298000_5093000", "298000_5094000"]


def _petawawa_root(tmp_path: Path) -> tuple[DataRoots, dict]:
    """A miniature fresh-preparation root with valid LAS tiles and a matching export."""
    root = tmp_path / "fresh"
    full = root / "full"
    repaired = full / "repaired"
    repaired.mkdir(parents=True)
    (root / "download").mkdir()
    (root / "download" / "Petawawa2012.zip").write_bytes(b"archive bytes")
    (root / "download" / "Petawawa2012.zip.part").write_bytes(b"partial")
    for name in ("fresh-root.rds", "negative-z-transition.rds", "download/download.rds"):
        (root / name).write_bytes(name.encode())
    for name in ("stage-identity.rds", "parallel-transition.rds", "ready.rds"):
        (full / name).write_bytes(name.encode())
    (full / "prepared").mkdir()
    (full / "prepared" / "preparation.rds").write_bytes(b"plan")

    def md5(path: Path) -> str:
        return hash_file(path, md5=True)[1]

    repairs, tiles = [], []
    for i, tile in enumerate(TILES):
        write_tiles(repaired / "tmp", count=1, seed=i + 1)
        (repaired / "tmp" / "500000_5000000.las").rename(repaired / f"{tile}.laz")
        (repaired / "tmp").rmdir()
        (repaired / f"{tile}.laz.rds").write_bytes(f"repair {tile}".encode())
        repairs.append(
            {
                "tile": tile,
                "md5": md5(repaired / f"{tile}.laz"),
                "policy_version": None,
                "negative_source_z": 0,
            }
        )
    archive_md5 = md5(root / "download" / "Petawawa2012.zip")
    for density in (1, 2, 5, 10, 20, 50):
        folder = full / "prepared" / "1_org" / f"tile_1000_density_{density}"
        folder.mkdir(parents=True)
        for i, tile in enumerate(TILES):
            write_tiles(folder / "tmp", count=1, points=10 + density, seed=100 * density + i)
            (folder / "tmp" / "500000_5000000.las").rename(folder / f"{tile}.laz")
            (folder / "tmp").rmdir()
            (folder / f"{tile}.lax").write_bytes(b"LASX" + tile.encode())
            (folder / f"{tile}.laz.rds").write_bytes(b"receipt")
            tiles.append(
                {
                    "density": density,
                    "tile": tile,
                    "laz_md5": md5(folder / f"{tile}.laz"),
                    "lax_md5": md5(folder / f"{tile}.lax"),
                    "source_md5": repairs[i]["md5"],
                    "seed": 1,
                    "points": 10 + density,
                    "index_validation": "passed",
                }
            )
    export = {
        "schema": legacy_petawawa.EXPORT_SCHEMA,
        "stage": "full",
        "root": {"policy_version": "fresh-petawawa-v3", "metadata_confirmation": "tester"},
        "negative_z_transition": {
            "previous_policy": "fresh-petawawa-v3",
            "current_policy": "fresh-petawawa-v4",
        },
        "parallel_transition": True,
        "download": {"url": "u", "md5": archive_md5, "bytes": 13, "authenticity": "none"},
        "stage_identity": {"archive_md5": archive_md5, "members": ["a"]},
        "preparation": {
            "method": "fresh-petawawa-v4",
            "archive_md5": archive_md5,
            "tiles": TILES,
            "source_md5": [r["md5"] for r in repairs],
            "repairs_md5": [md5(repaired / f"{t}.laz.rds") for t in TILES],
        },
        "ready": {
            "status": "data-qc-passed",
            "policy_version": "fresh-petawawa-v4",
            "archive_md5": archive_md5,
            "preparation_md5": md5(full / "prepared" / "preparation.rds"),
        },
        "repairs": repairs,
        "tiles": tiles,
    }
    return DataRoots({"petawawa-fresh": root}), export


def _write_export(tmp_path: Path, export: dict) -> Path:
    path = tmp_path / f"export-{len(list(tmp_path.glob('export-*')))}.json"
    path.write_text(json.dumps(export), encoding="utf-8")
    return path


def test_petawawa_registration_cross_checks_receipts(tmp_path):
    registry = make_registry(
        tmp_path, {"petawawa-fresh-v4": PETAWAWA_RECIPE, "petawawa-als-2012": ARCHIVE_RECIPE}
    )
    roots, export = _petawawa_root(tmp_path)
    path = _write_export(tmp_path, export)
    archive_recipe = registry.recipe("petawawa-als-2012")
    archive = register_dataset(
        archive_recipe, archive_recipe.datasets[0], registry, roots, legacy_export=path
    ).manifest
    assert [f.path for f in archive.files] == ["Petawawa2012.zip"]
    assert archive.kind == "source"
    recipe = registry.recipe("petawawa-fresh-v4")
    for spec in recipe.datasets:
        manifest = register_dataset(recipe, spec, registry, roots, legacy_export=path).manifest
        assert manifest.kind == "derived"
        assert manifest.provenance.derived_from[0].content_sha256 == archive.content_sha256
        details = manifest.provenance.details["legacy_receipts"]
        assert details["ready"]["status"] == "data-qc-passed"
        assert len(manifest.provenance.records) == 7
        assert "cross-checked" in manifest.provenance.notes[-1]
    d1 = registry.manifest("petawawa-fresh-v4/tile_1000_density_1")
    assert {f.role for f in d1.files} == {"input", "index", "receipt"}
    assert [t["tile"] for t in d1.provenance.details["legacy_receipts"]["tiles"]] == TILES
    assert verify_dataset(d1, roots, provenance=True).status == "verified"


@pytest.mark.parametrize(
    "spec_id, edit, message",
    [
        ("tile_1000_density_2", lambda e: e["tiles"][2].update(laz_md5="0" * 32), "MD5"),
        ("tile_1000_density_2", lambda e: e["tiles"][2].update(points=1), "point count"),
        ("tile_1000_density_2", lambda e: e["tiles"][2].update(index_validation="x"), "index"),
        ("tile_1000_density_2", lambda e: e["tiles"][2].update(source_md5="1" * 32), "source MD5"),
        ("tile_1000_density_1", lambda e: e["ready"].update(status="failed"), "status"),
        ("tile_1000_density_1", lambda e: e["ready"].update(preparation_md5="2" * 32), "readiness"),
        ("tile_1000_density_1", lambda e: e["download"].update(md5="3" * 32), "archive MD5"),
        ("tile_1000_density_1", lambda e: e["preparation"].update(method="v5"), "policy"),
        (
            "tile_1000_density_1",
            lambda e: e["negative_z_transition"].update(current_policy="x"),
            "transition",
        ),
        ("repaired", lambda e: e["repairs"][0].update(negative_source_z=2), "negative source Z"),
        ("repaired", lambda e: e["repairs"][0].update(policy_version="v9"), "repair policy"),
        ("repaired", lambda e: e["preparation"].update(repairs_md5=["x", "y"]), "repairs_md5"),
        ("repaired", lambda e: e["repairs"][1].update(md5="4" * 32), "MD5 differs"),
    ],
)
def test_petawawa_registration_refuses_disagreeing_receipts(tmp_path, spec_id, edit, message):
    registry = make_registry(tmp_path, {"petawawa-fresh-v4": PETAWAWA_RECIPE})
    roots, export = _petawawa_root(tmp_path)
    broken = copy.deepcopy(export)
    edit(broken)
    recipe = registry.recipe("petawawa-fresh-v4")
    with pytest.raises(RegistrationError, match=message):
        register_dataset(
            recipe,
            recipe.dataset(f"petawawa-fresh-v4/{spec_id}"),
            registry,
            roots,
            legacy_export=_write_export(tmp_path, broken),
        )
    assert not registry.has_manifest(f"petawawa-fresh-v4/{spec_id}")


def test_petawawa_registration_without_export_must_be_explicit(tmp_path):
    registry = make_registry(tmp_path, {"petawawa-fresh-v4": PETAWAWA_RECIPE})
    roots, _ = _petawawa_root(tmp_path)
    recipe = registry.recipe("petawawa-fresh-v4")
    spec = recipe.datasets[1]
    with pytest.raises(RegistrationError, match="--legacy-export"):
        register_dataset(recipe, spec, registry, roots)
    manifest = register_dataset(recipe, spec, registry, roots, skip_legacy_crosscheck=True).manifest
    assert "WITHOUT" in manifest.provenance.notes[-1]
    assert "legacy_receipts" not in manifest.provenance.details


def test_petawawa_export_errors(tmp_path):
    bad = tmp_path / "bad.json"
    bad.write_text('{"schema": "other"}', encoding="utf-8")
    with pytest.raises(legacy_petawawa.LegacyReceiptError, match="schema"):
        legacy_petawawa.load_export(bad)
    bad.write_text(json.dumps({"schema": legacy_petawawa.EXPORT_SCHEMA}), encoding="utf-8")
    with pytest.raises(legacy_petawawa.LegacyReceiptError, match="missing section"):
        legacy_petawawa.load_export(bad)
    with pytest.raises(legacy_petawawa.LegacyReceiptError, match="cannot read"):
        legacy_petawawa.load_export(tmp_path / "missing.json")


def test_archive_crosscheck(tmp_path):
    registry = make_registry(tmp_path, {"petawawa-als-2012": ARCHIVE_RECIPE})
    roots, export = _petawawa_root(tmp_path)
    export["download"]["bytes"] = 99
    recipe = registry.recipe("petawawa-als-2012")
    with pytest.raises(RegistrationError, match="size differs"):
        register_dataset(
            recipe,
            recipe.datasets[0],
            registry,
            roots,
            legacy_export=_write_export(tmp_path, export),
        )


def test_committed_legacy_export_matches_manifests():
    registry = DatasetRegistry(REPO_REGISTRY)
    path = REPO_REGISTRY / "legacy-receipts" / "petawawa-fresh-v4-full.json"
    manifests = [m for m in registry.manifests() if m.recipe == "petawawa-fresh-v4"]
    if not manifests:
        pytest.skip("Petawawa manifests not registered")
    export, sha = legacy_petawawa.load_export(path)
    assert "non-profit" not in path.read_text(encoding="utf-8")
    for manifest in manifests:
        assert manifest.provenance.details["legacy_receipts"]["export_sha256"] == sha
    by_id = {m.dataset_id: m for m in manifests}
    d1 = by_id.get("petawawa-fresh-v4/tile_1000_density_1")
    if d1 is not None:
        md5s = {f.path: f.md5 for f in d1.files}
        for tile in export["tiles"]:
            if tile["density"] == 1:
                assert md5s[f"{tile['tile']}.laz"] == tile["laz_md5"]
                assert md5s[f"{tile['tile']}.lax"] == tile["lax_md5"]
