"""Input tile manifest and buffered reference populations.

The manifest is built from the stdlib LAS header reader: unique stems,
finite non-degenerate header XY bounds and a non-zero point count per tile.
A tile's reference population is every decoded point of the tile and of each
neighbouring tile whose header bounds intersect the tile bounds grown by the
contract buffer, restricted to that closed buffered rectangle. Every
contributing file is decoded in full; its decoded count must equal the header
count, every XYZ must be finite and every point must lie inside its own header
bounds. Input bytes are hashed before decoding and re-checked afterwards.
"""

from __future__ import annotations

import math
from collections import OrderedDict
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import numpy as np

from lidarbench.core.canonical import sha256_file
from lidarbench.datasets.lasheader import LasHeader, LasHeaderError, read_las_header
from lidarbench.validation.artifacts import tile_stem
from lidarbench.validation.io import PointData, ReadError, read_points


class InputError(RuntimeError):
    """An input tile cannot support a reference calculation."""


@dataclass(frozen=True)
class Tile:
    """One input file with its header facts."""

    stem: str
    path: Path
    header: LasHeader

    @property
    def bounds(self) -> tuple[float, float, float, float]:
        """Header ``(min_x, min_y, max_x, max_y)``."""
        return self.header.bounds_xy


@dataclass
class Manifest:
    """All input tiles, keyed by stem, plus problems found while reading headers."""

    tiles: dict[str, Tile] = field(default_factory=dict)
    problems: list[str] = field(default_factory=list)

    def to_record(self) -> list[dict[str, Any]]:
        """Per-tile header summary."""
        return [t.header.to_record() for t in self.tiles.values()]


def build_manifest(inputs: Sequence[Path]) -> Manifest:
    """Read every input header (stdlib) and check the manifest invariants."""
    manifest = Manifest()
    for path in inputs:
        stem = tile_stem(Path(path))
        if stem in manifest.tiles:
            manifest.problems.append(f"duplicate input tile stem {stem!r}")
            continue
        try:
            header = read_las_header(path)
        except LasHeaderError as exc:
            manifest.problems.append(str(exc))
            continue
        min_x, min_y, max_x, max_y = header.bounds_xy
        if not all(math.isfinite(v) for v in (*header.min_xyz, *header.max_xyz)):
            manifest.problems.append(f"{Path(path).name}: non-finite header bounds")
        elif not (min_x < max_x and min_y < max_y):
            manifest.problems.append(f"{Path(path).name}: zero-area header XY bounds")
        if header.point_count == 0:
            manifest.problems.append(f"{Path(path).name}: header declares zero points")
        manifest.tiles[stem] = Tile(stem, Path(path), header)
    return manifest


def _intersects(a: tuple[float, float, float, float], b: tuple[float, float, float, float]):
    return a[2] >= b[0] and a[0] <= b[2] and a[3] >= b[1] and a[1] <= b[3]


@dataclass(frozen=True)
class Population:
    """Reference population of one tile (core plus buffer)."""

    x: Any
    y: Any
    z: Any
    sources: tuple[str, ...]

    @property
    def count(self) -> int:
        """Number of points."""
        return int(self.x.shape[0])


class PopulationLoader:
    """Decodes input files on demand and assembles buffered populations.

    Decoding dominates validation cost, so two caches bound it: a small LRU of
    fully decoded tiles and a larger cache of *edge strips* (the points within
    ``buffer`` of a tile's own header edges). A neighbour whose interior does
    not overlap the tile contributes only points inside its edge strip, so a
    neighbour is decoded in full at most once while its strip stays cached.

    ``known_digests`` (file path -> SHA-256) are digests already verified by the
    caller (for example the campaign identity, re-checked by stat fingerprints);
    they replace hashing the inputs before and after decoding, and
    :meth:`changed_inputs` then leaves change detection to the caller.
    """

    def __init__(
        self,
        manifest: Manifest,
        buffer: float,
        cache_size: int = 9,
        known_digests: Mapping[str, str] | None = None,
        strip_cache_size: int = 512,
    ) -> None:
        self.manifest = manifest
        self.buffer = buffer
        self.cache_size = cache_size
        self.strip_cache_size = strip_cache_size
        self.known_digests = dict(known_digests or {})
        self._cache: OrderedDict[str, PointData] = OrderedDict()
        self._strips: OrderedDict[str, tuple[Any, Any, Any]] = OrderedDict()
        self.digests: dict[str, str] = {}
        self.trusted: set[str] = set()
        self.decoded_files = 0
        self.decoded_points = 0

    def digest(self, tile: Tile) -> str:
        """SHA-256 of a tile's bytes (trusted digest when the caller supplied one)."""
        if tile.stem not in self.digests:
            known = self.known_digests.get(str(tile.path))
            if known is not None:
                self.digests[tile.stem] = known
                self.trusted.add(tile.stem)
            else:
                self.digests[tile.stem] = sha256_file(tile.path)
        return self.digests[tile.stem]

    def population_key(self, stem: str) -> tuple[tuple[str, str], ...]:
        """Content key of a tile's population: (stem, SHA-256) of every contributing file."""
        return tuple((t.stem, self.digest(t)) for t in self.neighbours(stem))

    def _decoded(self, tile: Tile) -> PointData:
        if tile.stem in self._cache:
            self._cache.move_to_end(tile.stem)
            return self._cache[tile.stem]
        self.digest(tile)
        try:
            data = read_points(tile.path)
        except (ReadError, LasHeaderError) as exc:
            raise InputError(str(exc)) from exc
        self.decoded_files += 1
        self.decoded_points += int(data.x.shape[0])
        name = tile.path.name
        header = tile.header
        if not header.compressed and header.version < (1, 4):
            expected = header.offset_to_points + header.point_count * header.point_record_length
            actual = tile.path.stat().st_size
            if actual != expected:
                raise InputError(
                    f"{name}: payload is {actual - header.offset_to_points} bytes but the header "
                    f"declares {header.point_count} records of {header.point_record_length} bytes"
                )
        if not (np.isfinite(data.x).all() and np.isfinite(data.y).all()):
            raise InputError(f"{name}: non-finite X/Y coordinates")
        if not np.isfinite(data.z).all():
            raise InputError(f"{name}: non-finite Z values")
        min_x, min_y, max_x, max_y = tile.bounds
        sx, sy = tile.header.scale[0], tile.header.scale[1]
        # Header bounds may round the scaled extremes; allow half a coordinate unit.
        outside = (
            (data.x < min_x - sx / 2)
            | (data.x > max_x + sx / 2)
            | (data.y < min_y - sy / 2)
            | (data.y > max_y + sy / 2)
        )
        if outside.any():
            raise InputError(
                f"{name}: {int(outside.sum())} point(s) lie outside the header XY bounds; "
                "header bounds cannot define the tile extent"
            )
        self._cache[tile.stem] = data
        while len(self._cache) > self.cache_size:
            self._cache.popitem(last=False)
        return data

    def _strip(self, tile: Tile) -> tuple[Any, Any, Any]:
        """Points of ``tile`` within ``buffer`` of its own header edges (inclusive)."""
        if tile.stem in self._strips:
            self._strips.move_to_end(tile.stem)
            return self._strips[tile.stem]
        data = self._decoded(tile)
        min_x, min_y, max_x, max_y = tile.bounds
        b = self.buffer
        keep = (
            (data.x <= min_x + b)
            | (data.x >= max_x - b)
            | (data.y <= min_y + b)
            | (data.y >= max_y - b)
        )
        strip = (data.x[keep], data.y[keep], data.z[keep])
        self._strips[tile.stem] = strip
        while len(self._strips) > self.strip_cache_size:
            self._strips.popitem(last=False)
        return strip

    @staticmethod
    def _separated(a: Tile, b: Tile) -> bool:
        """Whether the interiors of two tiles' header rectangles do not overlap."""
        a0, a1, a2, a3 = a.bounds
        b0, b1, b2, b3 = b.bounds
        return a2 <= b0 or b2 <= a0 or a3 <= b1 or b3 <= a1

    def neighbours(self, stem: str) -> list[Tile]:
        """The tile itself first, then every tile within the buffer (stable order)."""
        tile = self.manifest.tiles[stem]
        b = self.buffer
        min_x, min_y, max_x, max_y = tile.bounds
        grown = (min_x - b, min_y - b, max_x + b, max_y + b)
        others = [
            t for s, t in self.manifest.tiles.items() if s != stem and _intersects(t.bounds, grown)
        ]
        return [tile, *others]

    def population(self, stem: str) -> Population:
        """Core plus buffered points of tile ``stem``."""
        tile = self.manifest.tiles[stem]
        b = self.buffer
        min_x, min_y, max_x, max_y = tile.bounds
        xs, ys, zs, sources = [], [], [], []
        for source in self.neighbours(stem):
            if source.stem != stem and self._separated(tile, source):
                # The buffered rectangle meets a separated neighbour only within
                # ``buffer`` of that neighbour's own edges.
                sx, sy, sz = self._strip(source)
            else:
                data = self._decoded(source)
                sx, sy, sz = data.x, data.y, data.z
            keep = (sx >= min_x - b) & (sx <= max_x + b) & (sy >= min_y - b) & (sy <= max_y + b)
            xs.append(sx[keep])
            ys.append(sy[keep])
            zs.append(sz[keep])
            sources.append(source.stem)
        return Population(
            np.concatenate(xs), np.concatenate(ys), np.concatenate(zs), tuple(sources)
        )

    def changed_inputs(self) -> list[str]:
        """Inputs whose bytes changed since they were first hashed.

        Inputs with caller-supplied (trusted) digests are not re-hashed here; the
        caller is responsible for detecting their changes.
        """
        changed = []
        for stem, sha in self.digests.items():
            if stem in self.trusted:
                continue
            path = self.manifest.tiles[stem].path
            try:
                now = sha256_file(path)
            except OSError:
                now = None
            if now != sha:
                changed.append(path.name)
        return changed
