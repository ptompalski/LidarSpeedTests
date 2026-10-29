"""Validation cost controls: edge strips, shared references and trusted input digests."""

from __future__ import annotations

from pathlib import Path

import pytest
from validation_fixtures import neighbour_scene, write_las

np = pytest.importorskip("numpy")
pytest.importorskip("laspy")

from lidarbench.validation import population as population_module  # noqa: E402
from lidarbench.validation.io import read_points  # noqa: E402
from lidarbench.validation.population import PopulationLoader, build_manifest  # noqa: E402
from lidarbench.validation.validators import ReferenceCache, TileReference  # noqa: E402


def _grid_scene(directory: Path) -> list[Path]:
    """3 x 3 tiles of 100 m with points on and near every shared edge."""
    directory.mkdir(parents=True, exist_ok=True)
    rng = np.random.default_rng(7)
    paths = []
    for row in range(3):
        for col in range(3):
            x0, y0 = 500000 + 100 * col, 5000000 + 100 * row
            xs = np.concatenate([rng.uniform(x0, x0 + 100, 300), [x0, x0 + 100, x0 + 20, x0 + 80]])
            ys = np.concatenate([rng.uniform(y0, y0 + 100, 300), [y0, y0 + 100, y0 + 80, y0 + 20]])
            zs = rng.uniform(100, 200, xs.size)
            path = directory / f"t{row}{col}.las"
            write_las(path, list(xs), list(ys), list(zs))
            paths.append(path)
    return paths


def _brute_force(paths: list[Path], stem: str, buffer: float) -> np.ndarray:
    manifest = build_manifest(paths)
    min_x, min_y, max_x, max_y = manifest.tiles[stem].bounds
    rows = []
    for path in paths:
        data = read_points(path)
        keep = (
            (data.x >= min_x - buffer)
            & (data.x <= max_x + buffer)
            & (data.y >= min_y - buffer)
            & (data.y <= max_y + buffer)
        )
        rows.append(np.column_stack([data.x[keep], data.y[keep], data.z[keep]]))
    out = np.concatenate(rows)
    return out[np.lexsort(out.T[::-1])]


def _sorted(pop) -> np.ndarray:
    out = np.column_stack([pop.x, pop.y, pop.z])
    return out[np.lexsort(out.T[::-1])]


def test_edge_strips_give_the_same_population_as_full_decoding(tmp_path):
    paths = _grid_scene(tmp_path / "grid")
    loader = PopulationLoader(build_manifest(paths), 20.0, cache_size=1)
    for path in paths:
        stem = path.stem
        assert np.array_equal(_sorted(loader.population(stem)), _brute_force(paths, stem, 20.0))
    # Each file is decoded far fewer times than once per neighbour visit (9 tiles, 49 visits).
    assert loader.decoded_files <= 2 * len(paths)


def test_neighbour_scene_population_unchanged(tmp_path):
    neighbour_scene(tmp_path)
    paths = sorted(tmp_path.glob("*.las"))
    loader = PopulationLoader(build_manifest(paths), 20.0)
    for path in paths:
        assert np.array_equal(
            _sorted(loader.population(path.stem)), _brute_force(paths, path.stem, 20.0)
        )


def test_trusted_digests_skip_hashing(tmp_path, monkeypatch):
    neighbour_scene(tmp_path)
    paths = sorted(tmp_path.glob("*.las"))

    def no_hashing(path):
        raise AssertionError(f"input hashed: {path}")

    monkeypatch.setattr(population_module, "sha256_file", no_hashing)
    known = {str(p): f"{i:064x}" for i, p in enumerate(paths)}
    loader = PopulationLoader(build_manifest(paths), 20.0, known_digests=known)
    loader.population(paths[0].stem)
    assert loader.population_key(paths[0].stem)[0] == (paths[0].stem, known[str(paths[0])])
    assert loader.trusted == {p.stem for p in paths}
    assert loader.changed_inputs() == []


def test_reference_cache_is_byte_bounded_lru():
    def entry(n: int) -> TileReference:
        return TileReference(1, ("a",), {"c": np.zeros(n)}, {"c": 1}, {"c": 0})

    cache = ReferenceCache(max_bytes=8 * 100)
    cache.put(("a",), entry(50), 400)
    cache.put(("b",), entry(50), 400)
    assert cache.get(("a",)) is not None  # a is now most recently used
    cache.put(("c",), entry(50), 400)
    assert cache.get(("b",)) is None and cache.get(("a",)) is not None
    assert cache.bytes == 800 and (cache.hits, cache.misses) == (2, 1)
    cache.put(("huge",), entry(1), 10_000)  # larger than the budget: not cached
    assert cache.get(("huge",)) is None
