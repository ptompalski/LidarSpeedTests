"""Deterministic synthetic LAS fixtures for automated tests (``synthetic-tiles-v1``).

The generator writes small, valid LAS 1.2 point-format-1 tiles using only the
standard library. Every value is derived from integer arithmetic and a
SplitMix64 stream (the same generator as ``schedule-v2``), and all floating
point values are produced by single correctly rounded IEEE operations, so the
bytes -- and therefore the SHA-256 digests in the committed fixture manifests
-- are identical on Linux, Windows and macOS.

Fixture data are clearly synthetic: no CRS is written, the system identifier
says ``lidarbench synthetic`` and the dataset kind is ``fixture``. They exercise
data management and pipelines; they are not benchmark data.
"""

from __future__ import annotations

import os
import struct
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any

GENERATOR = "synthetic-tiles-v1"
_SCALE = 0.01
_HEADER_SIZE = 227
_RECORD_LENGTH = 28
_POINT = struct.Struct("<iiiHBBbBHd")
_MASK = (1 << 64) - 1


class _SplitMix64:
    """SplitMix64 (identical to ``lidarbench.campaign.schedule.SplitMix64``)."""

    def __init__(self, seed: int) -> None:
        self._state = seed & _MASK

    def next_u64(self) -> int:
        self._state = (self._state + 0x9E3779B97F4A7C15) & _MASK
        z = self._state
        z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & _MASK
        z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & _MASK
        return z ^ (z >> 31)

    def below(self, n: int) -> int:
        limit = (1 << 64) - ((1 << 64) % n)
        while True:
            value = self.next_u64()
            if value < limit:
                return value % n


@dataclass(frozen=True)
class SyntheticTilesParameters:
    """Parameters of ``synthetic-tiles-v1`` (all integers; metres and points)."""

    seed: int = 20261028
    origin_x: int = 500000
    origin_y: int = 5000000
    tile_size_m: int = 100
    tiles_x: int = 2
    tiles_y: int = 1
    points_per_tile: int = 500

    @classmethod
    def parse(cls, value: Mapping[str, Any], where: str = "parameters") -> SyntheticTilesParameters:
        """Strictly parse a parameter table (integers only, unknown keys are errors)."""
        defaults = cls()
        names = set(defaults.__dataclass_fields__)
        unknown = sorted(set(value) - names)
        if unknown:
            raise ValueError(f"{where}: unknown key(s) {unknown}; allowed: {sorted(names)}")
        out: dict[str, int] = {}
        for name in sorted(names):
            item = value.get(name, getattr(defaults, name))
            if isinstance(item, bool) or not isinstance(item, int) or item < 0:
                raise ValueError(f"{where}.{name} must be a non-negative integer, got {item!r}")
            out[name] = item
        params = cls(**out)
        if not 1 <= params.tile_size_m <= 10_000 or not 1 <= params.points_per_tile <= 1_000_000:
            raise ValueError(f"{where}: tile_size_m or points_per_tile out of range")
        if not 1 <= params.tiles_x * params.tiles_y <= 64:
            raise ValueError(f"{where}: between 1 and 64 tiles are supported")
        return params


def _text(value: str, size: int) -> bytes:
    return value.encode("ascii")[:size].ljust(size, b"\0")


def tile_bytes(params: SyntheticTilesParameters, ix: int, iy: int) -> tuple[str, bytes]:
    """File name and complete LAS bytes of tile ``(ix, iy)``."""
    tile_index = iy * params.tiles_x + ix
    rng = _SplitMix64(params.seed * 1_000_003 + tile_index)
    ox = params.origin_x + ix * params.tile_size_m
    oy = params.origin_y + iy * params.tile_size_m
    extent = params.tile_size_m * 100
    points = []
    by_return = [0, 0, 0, 0, 0]
    xs, ys, zs = [], [], []
    for i in range(params.points_per_tile):
        x = rng.below(extent)
        y = rng.below(extent)
        ground = 20000 + ((ox * 100 + x) // 100 * 3 + (oy * 100 + y) // 100 * 2) % 500
        returns = 1 + rng.below(3)
        number = 1 + rng.below(returns)
        if number == returns and rng.below(2) == 0:
            z, cls = ground, 2
        else:
            z, cls = ground + rng.below(2500), 1
        intensity = rng.below(1000)
        gps = (tile_index * 1_000_000 + i) / 1000.0
        flags = number | (returns << 3)
        points.append(_POINT.pack(x, y, z, intensity, flags, cls, 0, 0, 1, gps))
        by_return[number - 1] += 1
        xs.append(x)
        ys.append(y)
        zs.append(z)
    header = b"".join(
        [
            b"LASF",
            struct.pack("<HH", 0, 0),
            bytes(16),
            struct.pack("<BB", 1, 2),
            _text("lidarbench synthetic", 32),
            _text(f"lidarbench {GENERATOR}", 32),
            struct.pack("<HH", 1, 2026),
            struct.pack("<HII", _HEADER_SIZE, _HEADER_SIZE, 0),
            struct.pack("<BHI", 1, _RECORD_LENGTH, len(points)),
            struct.pack("<5I", *by_return),
            struct.pack("<3d", _SCALE, _SCALE, _SCALE),
            struct.pack("<3d", float(ox), float(oy), 0.0),
            struct.pack(
                "<6d",
                max(xs) * _SCALE + ox,
                min(xs) * _SCALE + ox,
                max(ys) * _SCALE + oy,
                min(ys) * _SCALE + oy,
                max(zs) * _SCALE,
                min(zs) * _SCALE,
            ),
        ]
    )
    if len(header) != _HEADER_SIZE:
        raise RuntimeError("internal error: LAS header size")
    return f"{ox}_{oy}.las", header + b"".join(points)


def generate_synthetic_tiles(output_dir: Path, parameters: Mapping[str, Any]) -> list[Path]:
    """Write every tile into the existing, empty ``output_dir`` (exclusive create).

    Returns:
        The written paths, sorted.
    """
    params = SyntheticTilesParameters.parse(parameters)
    written = []
    for iy in range(params.tiles_y):
        for ix in range(params.tiles_x):
            name, data = tile_bytes(params, ix, iy)
            path = output_dir / name
            with open(path, "xb") as handle:
                handle.write(data)
                handle.flush()
                os.fsync(handle.fileno())
            written.append(path)
    return sorted(written)


GENERATORS = {GENERATOR: generate_synthetic_tiles}
