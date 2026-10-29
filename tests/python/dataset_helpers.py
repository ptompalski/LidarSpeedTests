"""Helpers for dataset-management tests: temporary registries, recipes and LAS tiles."""

from __future__ import annotations

import shutil
from pathlib import Path

from lidarbench.datasets.fixtures import SyntheticTilesParameters, tile_bytes
from lidarbench.datasets.registry import DatasetRegistry, default_registry_dir
from lidarbench.datasets.roots import DataRoots

REPO_REGISTRY = default_registry_dir({})

DERIVED_RECIPE = """
schema = "lidarbench.dataset-recipe/1"
id = "demo"
version = "demo-v1"
title = "Demo derived tiles"
kind = "derived"
root = "data"

[layout]
inputs = ["*.las"]
indexes = ["*.lax"]
receipts = ["*.json"]
exclude = ["*.bak"]

[license]
name = "Test licence"
redistribution = "permitted"

[source]
citation = "Test source"

[declared_metadata]
horizontal_epsg = 2959
xyz_units = "metre"

[preparation]
engine = "r-cli"
method = "demo-v1"
entry_point = "R/prepare.R"
stages = ["--all"]

[provenance]
records = [{ path = "ready.txt", description = "readiness" }]

[[datasets]]
id = "demo/tiles"
path = "stage/tiles"
title = "Demo tiles"
"""


def write_tiles(folder: Path, count: int = 2, points: int = 50, seed: int = 7) -> list[Path]:
    """Write ``count`` small valid LAS tiles (plus a ``.lax`` stand-in for the first)."""
    folder.mkdir(parents=True, exist_ok=True)
    params = SyntheticTilesParameters(seed=seed, tiles_x=count, points_per_tile=points)
    paths = []
    for ix in range(count):
        name, data = tile_bytes(params, ix, 0)
        (folder / name).write_bytes(data)
        paths.append(folder / name)
    return paths


def make_registry(tmp_path: Path, recipes: dict[str, str] | None = None) -> DatasetRegistry:
    """A registry under ``tmp_path/registry`` with the given recipe texts (name -> TOML)."""
    root = tmp_path / "registry"
    (root / "recipes").mkdir(parents=True)
    for name, text in (recipes or {}).items():
        (root / "recipes" / f"{name}.toml").write_text(text, encoding="utf-8")
    return DatasetRegistry(root)


def copy_repo_recipes(tmp_path: Path) -> DatasetRegistry:
    """A registry with copies of the repository recipes and no manifests."""
    root = tmp_path / "registry"
    root.mkdir()
    shutil.copytree(REPO_REGISTRY / "recipes", root / "recipes")
    return DatasetRegistry(root)


def demo_layout(tmp_path: Path) -> tuple[DataRoots, Path]:
    """Root ``data`` with ``stage/tiles`` (two tiles, an index, a receipt) and ``ready.txt``."""
    root = tmp_path / "data root"
    tiles = root / "stage" / "tiles"
    write_tiles(tiles)
    (tiles / "500000_5000000.lax").write_bytes(b"LASX index")
    (tiles / "receipt.json").write_text("{}", encoding="utf-8")
    (root / "ready.txt").write_text("ready", encoding="utf-8")
    return DataRoots({"data": root}), tiles
