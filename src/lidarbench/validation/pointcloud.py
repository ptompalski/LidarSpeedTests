"""Structural checks of point-cloud products (requires numpy and laspy).

A point-cloud contract (``[point_cloud]``) defines no numerical reference, so these
checks establish only that each artifact is a plausible output of the declared
transformation, never that its Z values are correct:

* **Ownership**: every output point is an input point with unchanged XY and
  unchanged ``preserved_attributes``, lying inside the tile's closed header
  bounds (the tile's own points, or neighbour points exactly on a shared edge).
  Matching is a multiset match on XY (rounded to the input coordinate quantum)
  plus the preserved attributes; output Z is free.
* **Completeness**: at most ``max_missing_fraction`` of the tile's own points
  are absent (points outside the ground surface, edge points written to a
  neighbour instead).
* **Plausible heights** (``z = "height_above_ground"``): every output Z is finite
  and the implied ground elevation, input Z minus output Z, lies within the
  elevation range of the ground-class points of the tile and its buffer. A linear
  TIN, and any convex (nearest-neighbour or inverse-distance) extrapolation, stays
  in that range; corrupt values such as integer overflows never do.

Nothing is repaired, cropped or rewritten; inputs are only read.
"""

from __future__ import annotations

from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import numpy as np

from lidarbench.datasets.lasheader import LasHeader, read_las_header
from lidarbench.validation.io import ReadError
from lidarbench.validation.population import InputError, Manifest, Tile
from lidarbench.workloads.contract import Implementation, PointCloudProduct, WorkloadContract

PACKED_ATTRIBUTES = {
    "return_number": 0,
    "number_of_returns": 4,
    "classification": 8,
    "intensity": 16,
}
"""Bit offsets of the integer attributes packed into one int64 matching key."""
EXAMPLE_LIMIT = 5


@dataclass(frozen=True)
class PointRecords:
    """Decoded coordinates and matching attributes of one LAS/LAZ file."""

    path: Path
    header: LasHeader
    x: Any
    y: Any
    z: Any
    attributes: dict[str, Any]
    """Decoded attribute arrays; absent from the point format = not a key."""

    @property
    def count(self) -> int:
        """Decoded point count."""
        return int(self.x.shape[0])

    def subset(self, keep: Any) -> PointRecords:
        """The records selected by a boolean mask."""
        return PointRecords(
            self.path,
            self.header,
            self.x[keep],
            self.y[keep],
            self.z[keep],
            {k: v[keep] for k, v in self.attributes.items()},
        )


def read_point_records(path: Path, attributes: tuple[str, ...]) -> PointRecords:
    """Decode XYZ and the requested attributes (those the point format has).

    Raises:
        ReadError: The file cannot be decoded or its count differs from the header.
    """
    import laspy

    try:
        header = read_las_header(path)
        las = laspy.read(path)
        x = np.asarray(las.x, dtype=np.float64)
        y = np.asarray(las.y, dtype=np.float64)
        z = np.asarray(las.z, dtype=np.float64)
        names = set(las.point_format.dimension_names)
        attrs = {
            name: np.asarray(las[name])
            for name in attributes
            if name not in ("x", "y") and name in names
        }
    except Exception as exc:  # LasHeaderError, OSError, laspy errors, broken LAZ backends
        raise ReadError(f"{path.name}: cannot decode points ({type(exc).__name__}: {exc})") from exc
    if x.shape[0] != header.point_count:
        raise ReadError(
            f"{path.name}: header declares {header.point_count} points but "
            f"{x.shape[0]} were decoded"
        )
    return PointRecords(path, header, x, y, z, attrs)


def _inside(records: PointRecords, box: tuple[float, float, float, float]) -> Any:
    min_x, min_y, max_x, max_y = box
    return (records.x >= min_x) & (records.x <= max_x) & (records.y >= min_y) & (records.y <= max_y)


class InputPoints:
    """Decodes input tiles on demand for ownership and ground-range checks.

    Keeps a small LRU of fully decoded tiles and, for neighbours, only the strip
    of points within ``buffer`` of their own header edges (the only part that can
    reach another tile's buffered rectangle).
    """

    def __init__(
        self,
        manifest: Manifest,
        buffer: float,
        attributes: tuple[str, ...],
        cache_size: int = 2,
        strip_cache_size: int = 64,
    ) -> None:
        self.manifest = manifest
        self.buffer = buffer
        self.attributes = attributes
        self.cache_size = cache_size
        self.strip_cache_size = strip_cache_size
        self._cache: OrderedDict[str, PointRecords] = OrderedDict()
        self._strips: OrderedDict[str, PointRecords] = OrderedDict()
        self.decoded_files = 0

    def _decoded(self, tile: Tile) -> PointRecords:
        if tile.stem in self._cache:
            self._cache.move_to_end(tile.stem)
            return self._cache[tile.stem]
        try:
            records = read_point_records(tile.path, self.attributes)
        except ReadError as exc:
            raise InputError(str(exc)) from exc
        if not (np.isfinite(records.x).all() and np.isfinite(records.y).all()):
            raise InputError(f"{tile.path.name}: non-finite X/Y coordinates")
        if not np.isfinite(records.z).all():
            raise InputError(f"{tile.path.name}: non-finite Z values")
        self.decoded_files += 1
        self._cache[tile.stem] = records
        while len(self._cache) > self.cache_size:
            self._cache.popitem(last=False)
        # Keep the edge strip too, so a later neighbour never decodes this tile again.
        self._remember_strip(tile, records)
        return records

    def _remember_strip(self, tile: Tile, records: PointRecords) -> PointRecords:
        min_x, min_y, max_x, max_y = tile.bounds
        b = self.buffer
        strip = records.subset(
            (records.x <= min_x + b)
            | (records.x >= max_x - b)
            | (records.y <= min_y + b)
            | (records.y >= max_y - b)
        )
        self._strips[tile.stem] = strip
        while len(self._strips) > self.strip_cache_size:
            self._strips.popitem(last=False)
        return strip

    def _strip(self, tile: Tile) -> PointRecords:
        if tile.stem in self._strips:
            self._strips.move_to_end(tile.stem)
            return self._strips[tile.stem]
        return self._remember_strip(tile, self._decoded(tile))

    def neighbours(self, stem: str) -> list[Tile]:
        """Tiles (other than ``stem``) whose header bounds meet its buffered bounds."""
        min_x, min_y, max_x, max_y = self.manifest.tiles[stem].bounds
        b = self.buffer
        return [
            t
            for s, t in self.manifest.tiles.items()
            if s != stem
            and t.bounds[0] <= max_x + b
            and t.bounds[2] >= min_x - b
            and t.bounds[1] <= max_y + b
            and t.bounds[3] >= min_y - b
        ]

    def tile(self, stem: str) -> PointRecords:
        """Every point of tile ``stem``."""
        return self._decoded(self.manifest.tiles[stem])

    def edge_points(self, stem: str) -> list[PointRecords]:
        """Neighbour points lying inside the closed header bounds of tile ``stem``."""
        box = self.manifest.tiles[stem].bounds
        out = []
        for tile in self.neighbours(stem):
            strip = self._strip(tile)
            keep = _inside(strip, box)
            if keep.any():
                out.append(strip.subset(keep))
        return out

    def ground_range(self, stem: str, classes: tuple[int, ...]) -> tuple[float, float, int]:
        """``(min Z, max Z, count)`` of ground-class points of the buffered tile."""
        min_x, min_y, max_x, max_y = self.manifest.tiles[stem].bounds
        b = self.buffer
        box = (min_x - b, min_y - b, max_x + b, max_y + b)
        parts = [self.tile(stem)] + [self._strip(t) for t in self.neighbours(stem)]
        lo, hi, n = np.inf, -np.inf, 0
        for records in parts:
            if "classification" not in records.attributes:
                raise InputError(f"{records.path.name}: no classification attribute")
            keep = _inside(records, box) & np.isin(records.attributes["classification"], classes)
            if keep.any():
                z = records.z[keep]
                lo, hi, n = min(lo, float(z.min())), max(hi, float(z.max())), n + int(keep.sum())
        return lo, hi, n


@dataclass
class PointCloudStructure:
    """Structural verdict for one point-cloud artifact."""

    errors: list[str] = field(default_factory=list)
    numerical_caps: list[str] = field(default_factory=list)
    accepted_exceptions: list[str] = field(default_factory=list)
    counts: dict[str, Any] = field(default_factory=dict)

    @property
    def passed(self) -> bool:
        """No structural error."""
        return not self.errors

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "passed": self.passed,
            "errors": list(self.errors),
            "accepted_exceptions": list(self.accepted_exceptions),
            "numerical_caps": list(self.numerical_caps),
            **self.counts,
        }


@dataclass(frozen=True)
class _XYPacking:
    """Packs XY grid indices relative to a tile into one int64 sort key.

    Candidates lie inside the tile's closed bounds, so their indices are in
    ``[1, sentinel)``; any coordinate outside that range maps to ``sentinel``, which
    no candidate has, so such a point can never match. ``None`` from :meth:`for_tile`
    means the tile is too large to pack and X and Y stay separate keys.
    """

    quantum: float
    x0: int
    y0: int
    bits: int

    @staticmethod
    def for_tile(bounds: tuple[float, float, float, float], quantum: float) -> _XYPacking | None:
        min_x, min_y, max_x, max_y = bounds
        x0 = int(np.floor(min_x / quantum)) - 1
        y0 = int(np.floor(min_y / quantum)) - 1
        span = max(int(np.ceil(max_x / quantum)) - x0, int(np.ceil(max_y / quantum)) - y0) + 2
        bits = max(1, int(span).bit_length())
        return _XYPacking(quantum, x0, y0, bits) if 2 * bits <= 62 else None

    def index(self, values: Any, origin: int) -> Any:
        sentinel = (1 << self.bits) - 1
        idx = np.rint(values / self.quantum).astype(np.int64) - origin
        return np.where((idx < 1) | (idx >= sentinel), sentinel, idx)

    def pack(self, records: PointRecords) -> Any:
        return (self.index(records.x, self.x0) << self.bits) | self.index(records.y, self.y0)


def _keys(
    records: PointRecords,
    quantum: float,
    attributes: tuple[str, ...],
    packing: _XYPacking | None = None,
) -> list[Any]:
    """Matching key columns: XY on the input quantum, GPS time bits, packed integers."""
    if packing is not None:
        columns = [packing.pack(records)]
    else:
        columns = [
            np.rint(records.x / quantum).astype(np.int64),
            np.rint(records.y / quantum).astype(np.int64),
        ]
    if "gps_time" in attributes:
        columns.append(
            np.ascontiguousarray(records.attributes["gps_time"], dtype=np.float64).view(np.int64)
        )
    packed = np.zeros(records.count, dtype=np.int64)
    for name, shift in PACKED_ATTRIBUTES.items():
        if name in attributes:
            packed |= records.attributes[name].astype(np.int64) << shift
    columns.append(packed)
    return columns


def _group_order(cols: list[Any]) -> Any:
    """A permutation that makes rows with equal keys (all ``cols``) contiguous.

    Equivalent to ``np.lexsort`` for grouping, but sorts only the first column in
    full: most first-column values occur at most twice (an input point and its
    output point), and a pair is contiguous whatever its other keys. Only groups
    of three or more rows are sorted on every key.
    """
    first = cols[0]
    order = np.argsort(first)
    if len(cols) == 1:
        return order
    sorted_first = first[order]
    new = np.empty(order.shape[0], dtype=bool)
    new[0] = True
    np.not_equal(sorted_first[1:], sorted_first[:-1], out=new[1:])
    starts = np.flatnonzero(new)
    sizes = np.diff(np.append(starts, order.shape[0]))
    large = np.repeat(sizes >= 3, sizes)
    if large.any():
        idx = np.flatnonzero(large)
        group = np.repeat(np.arange(starts.shape[0]), sizes)[idx]
        rows = order[idx]
        order[idx] = rows[np.lexsort((*(c[rows] for c in cols[:0:-1]), group))]
    return order


def check_point_cloud(
    output: PointRecords,
    stem: str,
    inputs: InputPoints,
    contract: WorkloadContract,
    implementation: Implementation,
) -> PointCloudStructure:
    """Run every structural check on one decoded point-cloud artifact."""
    product: PointCloudProduct | None = contract.point_cloud
    if product is None:
        raise ValueError(f"contract {contract.key} declares no point_cloud product")
    out = PointCloudStructure()
    tile = inputs.manifest.tiles[stem]
    try:
        own = inputs.tile(stem)
        edges = inputs.edge_points(stem)
        ground_lo, ground_hi, ground_n = inputs.ground_range(stem, product.ground_classes)
    except InputError as exc:
        out.errors.append(f"input cannot support the checks: {exc}")
        return out

    keyed = []
    for name in product.preserved_attributes:
        if name in ("x", "y"):
            continue
        if name not in own.attributes:
            out.numerical_caps.append(f"input point format has no {name}; it is not compared")
        elif name not in output.attributes:
            out.errors.append(f"output point format lost the {name} attribute")
        else:
            keyed.append(name)
    attributes = tuple(keyed)
    if out.errors:
        return out

    nonfinite = int((~np.isfinite(output.z)).sum())
    if not (np.isfinite(output.x).all() and np.isfinite(output.y).all()):
        out.errors.append("non-finite output X/Y coordinates")
        return out
    if output.count == 0:
        out.errors.append("no points written")
    if nonfinite:
        out.errors.append(f"{nonfinite} non-finite output Z value(s)")
    if ground_n == 0:
        out.errors.append(
            f"no ground points (classes {list(product.ground_classes)}) in the buffered tile"
        )
    if out.errors:
        return out

    quantum = min(tile.header.scale[0], tile.header.scale[1])
    packing = _XYPacking.for_tile(tile.bounds, quantum)
    candidates = [own, *edges]
    cand_cols = [
        np.concatenate(c)
        for c in zip(*(_keys(r, quantum, attributes, packing) for r in candidates), strict=True)
    ]
    cand_z = np.concatenate([r.z for r in candidates])
    cand_own = np.zeros(cand_z.shape[0], dtype=bool)
    cand_own[: own.count] = True
    out_cols = _keys(output, quantum, attributes, packing)

    n_cand = cand_z.shape[0]
    cols = [np.concatenate([a, b]) for a, b in zip(cand_cols, out_cols, strict=True)]
    del cand_cols, out_cols
    # Group statistics below do not depend on the order inside a group.
    order = _group_order(cols)
    sorted_cols = [c[order] for c in cols]
    del cols
    is_cand = order < n_cand
    z = np.concatenate([cand_z, output.z])[order]
    is_own = np.concatenate([cand_own, np.zeros(output.count, dtype=bool)])[order]
    new = np.zeros(order.shape[0], dtype=bool)
    new[0] = True
    for c in sorted_cols:
        new[1:] |= c[1:] != c[:-1]
    del sorted_cols, order
    starts = np.flatnonzero(new)
    group = np.cumsum(new) - 1
    n_c = np.bincount(group, weights=is_cand).astype(np.int64)
    n_o = np.bincount(group, weights=~is_cand).astype(np.int64)
    n_own = np.bincount(group, weights=is_own).astype(np.int64)
    z_min = np.minimum.reduceat(np.where(is_cand, z, np.inf), starts)
    z_max = np.maximum.reduceat(np.where(is_cand, z, -np.inf), starts)

    unmatched = int(np.maximum(n_o - n_c, 0).sum())
    missing = int(np.maximum(n_own - n_o, 0).sum())
    from_neighbours = int(np.maximum(np.minimum(n_o, n_c) - n_own, 0).sum())
    if unmatched:
        out.errors.append(
            f"{unmatched} output point(s) are not unchanged input points inside the tile's "
            f"closed header bounds (compared: x, y{''.join(', ' + a for a in attributes)})"
        )
    fraction = missing / own.count
    if fraction > product.max_missing_fraction:
        out.errors.append(
            f"{missing} of the tile's {own.count} points ({fraction:.4%}) are missing; the "
            f"contract allows {product.max_missing_fraction:.4%}"
        )

    rows = ~is_cand
    out_groups = group[rows]
    matched = n_c[out_groups] > 0
    z_out = z[rows][matched]
    implied_lo = z_min[out_groups][matched] - z_out
    implied_hi = z_max[out_groups][matched] - z_out
    tolerance = tile.header.scale[2] + output.header.scale[2] + 1e-9
    below = implied_hi < ground_lo - tolerance
    above = implied_lo > ground_hi + tolerance
    implausible = int((below | above).sum())
    if implausible:
        worst = np.concatenate(
            [z_out[below][:EXAMPLE_LIMIT], z_out[above][:EXAMPLE_LIMIT]]
        ).tolist()
        out.errors.append(
            f"{implausible} output height(s) imply a ground elevation outside the "
            f"[{ground_lo}, {ground_hi}] range of the buffered ground points (example output "
            f"Z: {worst})"
        )
    out.counts = {
        "output_points": output.count,
        "tile_points": own.count,
        "missing_tile_points": missing,
        "neighbour_edge_points_written": from_neighbours,
        "unmatched_output_points": unmatched,
        "implausible_heights": implausible,
        "ground_points": ground_n,
        "ground_z_range": [ground_lo, ground_hi],
        "output_z_range": (
            [float(output.z.min()), float(output.z.max())] if output.count else None
        ),
        "compared_attributes": ["x", "y", *attributes],
        "outside_surface": implementation.outside_surface,
    }
    return out


# -- per-artifact driver (sequential or a process pool) ------------------------------

WORKERS_ENV = "LIDARBENCH_VALIDATION_WORKERS"
"""Overrides the number of point-cloud validation processes (positive integer)."""
BYTES_PER_POINT = 800
"""Peak memory per worker per point of the largest input tile (measured ~620 B on
10 M-point Petawawa tiles: output, tile, neighbour strips and the matching sort)."""

_WORKER: dict[str, Any] = {}


@dataclass(frozen=True)
class ArtifactTask:
    """One point-cloud artifact to check against its input tile."""

    name: str
    tile: str
    path: Path
    size: int | None


def validation_workers(manifest: Manifest, artifacts: int) -> tuple[int, str]:
    """Number of validation processes and how it was chosen.

    Validation runs between attempts, outside any timed interval, so it may use every
    CPU this process may run on, bounded by half of the available memory.
    """
    import os

    override = os.environ.get(WORKERS_ENV)
    if override is not None:
        try:
            value = int(override)
        except ValueError:
            value = 0
        if value < 1:
            raise ValueError(f"{WORKERS_ENV} must be a positive integer, got {override!r}")
        return max(1, min(value, artifacts)), f"${WORKERS_ENV}"
    import psutil

    try:
        cpus = len(os.sched_getaffinity(0))  # type: ignore[attr-defined,unused-ignore]
    except AttributeError:
        cpus = os.cpu_count() or 1
    largest = max((t.header.point_count for t in manifest.tiles.values()), default=1)
    memory = int(psutil.virtual_memory().available * 0.5) // max(1, BYTES_PER_POINT * largest)
    workers = max(1, min(cpus, artifacts, memory))
    return workers, f"min(cpus {cpus}, artifacts {artifacts}, memory {memory})"


def _init_worker(
    manifest: Manifest,
    buffer: float,
    contract: WorkloadContract,
    implementation: Implementation,
) -> None:
    product = contract.point_cloud
    assert product is not None
    _WORKER.update(
        inputs=InputPoints(manifest, buffer, product.preserved_attributes),
        contract=contract,
        implementation=implementation,
    )


def _check_task(task: ArtifactTask) -> dict[str, Any]:
    """Decode and check one artifact in the current (worker) process."""
    contract: WorkloadContract = _WORKER["contract"]
    inputs: InputPoints = _WORKER["inputs"]
    product = contract.point_cloud
    assert product is not None
    before = inputs.decoded_files
    try:
        output = read_point_records(task.path, product.preserved_attributes)
    except ReadError as exc:
        return {"name": task.name, "read_error": str(exc), "decoded_inputs": 0}
    entry: dict[str, Any] = {
        "name": task.name,
        "tile": task.tile,
        "bytes": task.size,
        "points": output.header.to_record(),
    }
    result: dict[str, Any] = {"name": task.name, "entry": entry}
    if not inputs.manifest.problems:
        structure = check_point_cloud(
            output, task.tile, inputs, contract, _WORKER["implementation"]
        )
        entry["structural"] = structure.to_record()
        result["errors"] = list(structure.errors)
        result["accepted"] = [*structure.accepted_exceptions, *structure.numerical_caps]
    result["decoded_inputs"] = inputs.decoded_files - before
    return result


def check_artifacts(
    tasks: list[ArtifactTask],
    manifest: Manifest,
    contract: WorkloadContract,
    implementation: Implementation,
    workers: int,
) -> list[dict[str, Any]]:
    """Check every artifact; results in task order, identical for any ``workers``.

    With several workers each process gets one contiguous block of tasks (tiles in
    name order are spatial neighbours), so its neighbour-strip cache is reused.
    """
    if workers <= 1 or len(tasks) <= 1:
        _init_worker(manifest, contract.buffer, contract, implementation)
        try:
            return [_check_task(t) for t in tasks]
        finally:
            _WORKER.clear()
    import math
    import multiprocessing
    from concurrent.futures import ProcessPoolExecutor

    with ProcessPoolExecutor(
        max_workers=workers,
        mp_context=multiprocessing.get_context("spawn"),
        initializer=_init_worker,
        initargs=(manifest, contract.buffer, contract, implementation),
    ) as pool:
        return list(pool.map(_check_task, tasks, chunksize=math.ceil(len(tasks) / workers)))
