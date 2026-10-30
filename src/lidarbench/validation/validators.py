"""Numerical reference validators and their entry-point registry.

Validators are discovered through the ``lidarbench.validators`` entry-point
group. Each declares a ``name``, ``version``, ``level`` and which contracts it
applies to (``applicable``), and probes its optional dependencies (``probe``)
so a missing package becomes an explicit ``unavailable`` verdict, never an
import error. New reference methods (for example TIN surfaces) are added as new
validators without changing the pipeline.
"""

from __future__ import annotations

from collections import OrderedDict
from collections.abc import Iterable
from dataclasses import dataclass, field, replace
from importlib.metadata import entry_points
from typing import Any, Protocol, runtime_checkable

from lidarbench.validation.artifacts import ArtifactFile
from lidarbench.validation.io import BackendStatus, RasterData, probe_point_backend
from lidarbench.validation.status import Status
from lidarbench.workloads.contract import Implementation, WorkloadContract

VALIDATOR_ENTRY_POINT_GROUP = "lidarbench.validators"


@dataclass
class ArtifactNumerical:
    """Numerical verdict for one artifact."""

    status: Status
    reasons: list[str] = field(default_factory=list)
    boundary_difference_points: int | None = None
    detail: dict[str, Any] = field(default_factory=dict)

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "status": self.status.value,
            "reasons": list(self.reasons),
            "boundary_difference_points": self.boundary_difference_points,
            **self.detail,
        }


@runtime_checkable
class ReferenceValidator(Protocol):
    """Protocol of a numerical (reference) validator plugin."""

    name: str
    version: str
    level: str

    def applicable(self, contract: WorkloadContract) -> bool:
        """Whether this validator has a reference for ``contract``."""
        ...

    def probe(self) -> BackendStatus:
        """Availability of the validator's optional dependencies."""
        ...

    def validate_artifact(
        self,
        contract: WorkloadContract,
        implementation: Implementation,
        artifact: ArtifactFile,
        raster: RasterData,
        structure: Any,
        loader: Any,
    ) -> ArtifactNumerical:
        """Compare one structurally valid artifact with its reference."""
        ...


class ReferenceCache:
    """Byte-bounded LRU of per-tile references, shared across validations.

    A reference is a deterministic function of the contract (statistic, grid
    rules), the cell convention, the expected grid and the exact bytes of every
    contributing input file, so it is keyed by those and reused across attempts,
    repetitions, CPU counts and tools that share a convention.
    """

    def __init__(self, max_bytes: int = 1 << 30) -> None:
        self.max_bytes = max_bytes
        self._items: OrderedDict[tuple[Any, ...], Any] = OrderedDict()
        self._sizes: dict[tuple[Any, ...], int] = {}
        self.bytes = 0
        self.hits = 0
        self.misses = 0

    def get(self, key: tuple[Any, ...]) -> Any:
        """Cached value or ``None``."""
        if key in self._items:
            self._items.move_to_end(key)
            self.hits += 1
            return self._items[key]
        self.misses += 1
        return None

    def put(self, key: tuple[Any, ...], value: Any, nbytes: int) -> None:
        """Insert ``value`` and evict least recently used entries above the budget."""
        if nbytes > self.max_bytes:
            return
        if key in self._items:
            self.bytes -= self._sizes.pop(key)
            del self._items[key]
        self._items[key] = value
        self._sizes[key] = nbytes
        self.bytes += nbytes
        while self.bytes > self.max_bytes and self._items:
            old, _ = self._items.popitem(last=False)
            self.bytes -= self._sizes.pop(old)


@dataclass
class TileReference:
    """References of one tile population under several cell conventions."""

    population_points: int
    population_sources: tuple[str, ...]
    references: dict[str, Any]
    owned_points: dict[str, int]
    boundaries: dict[str, int]
    """Points owned by a different cell than under the contract convention, per convention."""
    counts: dict[str, Any] = field(default_factory=dict)
    """Owned points per cell (``(rows, cols)`` int32), per convention."""

    @property
    def nbytes(self) -> int:
        """Approximate memory use."""
        return sum(int(r.nbytes) for r in self.references.values()) + sum(
            int(c.nbytes) for c in self.counts.values()
        )


def _cell_counts(cells: Any, grid: Any) -> Any:
    import numpy as np

    owned = cells[cells >= 0]
    return np.bincount(owned, minlength=grid.size).astype(np.int32).reshape(grid.shape)


def tile_reference(
    population: Any, grid: Any, contract: WorkloadContract, conventions: Iterable[str]
) -> TileReference:
    """Compute the contract-convention reference plus one per extra declared convention."""
    from lidarbench.validation.grid import assign_cells, ownership_differences
    from lidarbench.validation.reference import cell_statistic

    base = contract.raster.cell_ownership
    contract_cells = assign_cells(population.x, population.y, grid, base)
    out = TileReference(
        population.count,
        tuple(population.sources),
        {base: cell_statistic(population.z, contract_cells, grid, contract.statistic)},
        {base: int((contract_cells >= 0).sum())},
        {base: 0},
        {base: _cell_counts(contract_cells, grid)},
    )
    for convention in sorted(set(conventions) - {base}):
        cells = assign_cells(population.x, population.y, grid, convention)
        out.references[convention] = cell_statistic(population.z, cells, grid, contract.statistic)
        out.owned_points[convention] = int((cells >= 0).sum())
        out.boundaries[convention] = ownership_differences(cells, contract_cells)
        out.counts[convention] = _cell_counts(cells, grid)
    return out


def declared_conventions(contract: WorkloadContract) -> set[str]:
    """Every declared cell convention of the contract and its implementations."""
    found = {contract.raster.cell_ownership}
    found.update(
        impl.cell_ownership for impl in contract.implementations.values() if impl.ownership_declared
    )
    return found


class CellStatisticValidator:
    """Independent per-cell mean/max of original Z on the contract grid."""

    name = "cell-statistic"
    version = "1"
    level = "numerical"

    def __init__(self, cache: ReferenceCache | None = None) -> None:
        self.cache = cache

    def applicable(self, contract: WorkloadContract) -> bool:
        """Contracts with ``reference.method = "cell_statistic"``."""
        return contract.reference_method == "cell_statistic" and contract.statistic in (
            "mean",
            "max",
        )

    def probe(self) -> BackendStatus:
        """Needs numpy and laspy (plus a LAZ backend for compressed inputs)."""
        return probe_point_backend()

    def validate_artifact(
        self,
        contract: WorkloadContract,
        implementation: Implementation,
        artifact: ArtifactFile,
        raster: RasterData,
        structure: Any,
        loader: Any,
    ) -> ArtifactNumerical:
        """See :class:`ReferenceValidator`."""
        import numpy as np

        from lidarbench.validation.population import InputError
        from lidarbench.validation.reference import compare_band

        grid = structure.expected_grid
        declared = implementation.ownership_declared
        base = contract.raster.cell_ownership
        convention = implementation.cell_ownership if declared else base
        try:
            entry: TileReference | None = None
            key: tuple[Any, ...] | None = None
            if self.cache is not None:
                key = (
                    contract.digest,
                    (grid.resolution, grid.col0, grid.row0, grid.ncols, grid.nrows),
                    loader.population_key(artifact.tile),
                )
                entry = self.cache.get(key)
            cache_state = "disabled" if self.cache is None else "hit"
            if entry is None:
                population = loader.population(artifact.tile)
                wanted = declared_conventions(contract) if self.cache else {base, convention}
                entry = tile_reference(population, grid, contract, wanted)
                if self.cache is not None and key is not None:
                    self.cache.put(key, entry, entry.nbytes)
                    cache_state = "miss"
        except InputError as exc:
            return ArtifactNumerical(Status.FAILED, [f"input cannot support a reference: {exc}"])
        statistic = contract.statistic
        tolerance = contract.tolerance
        if implementation.value_precision is not None:
            # The tool declares that it rounds values to this quantum (contract-level).
            tolerance = replace(
                tolerance, absolute=tolerance.absolute + implementation.value_precision / 2
            )
        contract_ref = entry.references[base]
        reference = entry.references[convention]
        boundary: int | None = entry.boundaries[convention] if declared else None
        trimmed: dict[str, Any] | None = None
        compare_grid = grid
        if structure.inset is not None:
            # Declared inward extent rule: compare the covered cells; count what was cut.
            counts = entry.counts[convention]
            dropped = int(counts.sum()) - int(structure.covered(counts).sum())
            lost_cells = int((~np.isnan(reference)).sum()) - int(
                (~np.isnan(structure.covered(reference))).sum()
            )
            trimmed = {
                "rule": implementation.extent,
                "cells_wesn": list(structure.trimmed),
                "dropped_points": dropped,
                "occupied_cells_cut": lost_cells,
            }
            reference = structure.covered(reference)
            contract_ref = structure.covered(contract_ref)
            compare_grid = structure.actual_grid
            if boundary is not None:
                boundary += dropped

        dtype = np.dtype(raster.dtypes[0])
        nodata = raster.nodata[0]
        sentinels = tuple(contract.raster.forbidden_values)
        if nodata is not None and not np.isnan(nodata):
            sentinels = (*sentinels, float(nodata))
        actual = (
            raster.values[0] if structure.inset is not None else structure.core(raster.values[0])
        )
        encode = dtype.type if dtype.kind == "f" else None
        primary = compare_band(
            actual, reference, tolerance, compare_grid, tuple(sorted(set(sentinels))), encode
        )
        detail: dict[str, Any] = {
            "reference": {
                "method": f"{self.name}/{self.version}",
                "statistic": statistic,
                "accumulation": "float64",
                "convention": convention,
                "population_points": entry.population_points,
                "population_sources": list(entry.population_sources),
                "owned_points": entry.owned_points[base],
                "cache": cache_state,
            },
            "tolerance": {
                "absolute": tolerance.absolute,
                "relative": tolerance.relative,
                "contract_absolute": contract.tolerance.absolute,
                "value_precision": implementation.value_precision,
            },
            "comparison": primary.to_record(),
        }
        if trimmed is not None:
            detail["trimmed_extent"] = trimmed
        if declared and implementation.cell_ownership != contract.raster.cell_ownership:
            versus_contract = compare_band(actual, contract_ref, tolerance, compare_grid)
            detail["contract_convention_comparison"] = {
                "convention": contract.raster.cell_ownership,
                "passed": versus_contract.passed,
                "mask_differences": versus_contract.mask_differences,
                "values_exceeding_tolerance": versus_contract.values_exceeding_tolerance,
            }

        reasons = primary.problems()
        if primary.sentinel_collisions:
            return ArtifactNumerical(Status.FAILED, reasons, boundary, detail)
        if not declared:
            verdict = "agrees" if primary.passed else "does not agree"
            return ArtifactNumerical(
                Status.UNVERIFIED,
                [
                    f"tool cell ownership is undeclared; the product {verdict} with the "
                    f"{contract.raster.cell_ownership} contract reference (diagnostic only)",
                    *reasons,
                ],
                boundary,
                detail,
            )
        if not primary.passed:
            return ArtifactNumerical(Status.FAILED, reasons, boundary, detail)
        if implementation.value_precision is not None:
            reasons.append(
                f"values compared within the declared output precision of "
                f"{implementation.value_precision} (tolerance widened by half of it)"
            )
        if boundary:
            reasons.append(
                f"{boundary} point(s) are owned by a different cell under "
                f"{implementation.cell_ownership} than under {contract.raster.cell_ownership}"
                + (
                    f", including {trimmed['dropped_points']} point(s) in "
                    f"{trimmed['occupied_cells_cut']} occupied edge cell(s) cut by the declared "
                    f"extent rule {trimmed['rule']}"
                    if trimmed and trimmed["dropped_points"]
                    else ""
                )
            )
        caps = list(structure.numerical_caps)
        if implementation.status.value == "diagnostic_only":
            caps.append("diagnostic_only implementation: agreement is never certified")
        if caps:
            return ArtifactNumerical(Status.UNVERIFIED, [*caps, *reasons], boundary, detail)
        if structure.accepted_exceptions:
            return ArtifactNumerical(
                Status.ACCEPTED_EXCEPTION,
                [*structure.accepted_exceptions, *reasons],
                boundary,
                detail,
            )
        return ArtifactNumerical(Status.PASSED, reasons, boundary, detail)


BUILTIN_VALIDATORS: tuple[type[CellStatisticValidator], ...] = (CellStatisticValidator,)


def discover_validators() -> tuple[list[ReferenceValidator], list[str]]:
    """Instantiate every registered validator; return them plus load errors.

    Built-ins are always present even if package metadata is missing (for
    example when running from a source tree without installation).
    """
    found: list[ReferenceValidator] = []
    errors: list[str] = []
    names: set[str] = set()
    for ep in entry_points(group=VALIDATOR_ENTRY_POINT_GROUP):
        try:
            validator = ep.load()()
        except Exception as exc:
            errors.append(f"validator {ep.name!r} failed to load: {type(exc).__name__}: {exc}")
            continue
        if not isinstance(validator, ReferenceValidator) or validator.name != ep.name:
            errors.append(f"validator {ep.name!r} does not implement the validator protocol")
            continue
        found.append(validator)
        names.add(validator.name)
    for cls in BUILTIN_VALIDATORS:
        if cls.name not in names:
            found.append(cls())
    return found, errors


def select_validator(
    contract: WorkloadContract, validators: Iterable[ReferenceValidator]
) -> ReferenceValidator | None:
    """First applicable validator, by name order (deterministic)."""
    for validator in sorted(validators, key=lambda v: v.name):
        if validator.applicable(contract):
            return validator
    return None
