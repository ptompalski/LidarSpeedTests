"""Numerical reference validators and their entry-point registry.

Validators are discovered through the ``lidarbench.validators`` entry-point
group. Each declares a ``name``, ``version``, ``level`` and which contracts it
applies to (``applicable``), and probes its optional dependencies (``probe``)
so a missing package becomes an explicit ``unavailable`` verdict, never an
import error. New reference methods (for example TIN surfaces) are added as new
validators without changing the pipeline.
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass, field
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


class CellStatisticValidator:
    """Independent per-cell mean/max of original Z on the contract grid."""

    name = "cell-statistic"
    version = "1"
    level = "numerical"

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

        from lidarbench.validation.grid import assign_cells, ownership_differences
        from lidarbench.validation.population import InputError
        from lidarbench.validation.reference import cell_statistic, compare_band

        grid = structure.expected_grid
        try:
            population = loader.population(artifact.tile)
        except InputError as exc:
            return ArtifactNumerical(Status.FAILED, [f"input cannot support a reference: {exc}"])
        statistic = contract.statistic
        contract_cells = assign_cells(
            population.x, population.y, grid, contract.raster.cell_ownership
        )
        contract_ref = cell_statistic(population.z, contract_cells, grid, statistic)
        declared = implementation.ownership_declared
        boundary: int | None = None
        reference = contract_ref
        if declared and implementation.cell_ownership != contract.raster.cell_ownership:
            tool_cells = assign_cells(
                population.x, population.y, grid, implementation.cell_ownership
            )
            boundary = ownership_differences(tool_cells, contract_cells)
            reference = cell_statistic(population.z, tool_cells, grid, statistic)
        elif declared:
            boundary = 0

        dtype = np.dtype(raster.dtypes[0])
        nodata = raster.nodata[0]
        sentinels = tuple(contract.raster.forbidden_values)
        if nodata is not None and not np.isnan(nodata):
            sentinels = (*sentinels, float(nodata))
        actual = structure.core(raster.values[0])
        encode = dtype.type if dtype.kind == "f" else None
        primary = compare_band(
            actual, reference, contract.tolerance, grid, tuple(sorted(set(sentinels))), encode
        )
        detail: dict[str, Any] = {
            "reference": {
                "method": f"{self.name}/{self.version}",
                "statistic": statistic,
                "accumulation": "float64",
                "convention": (
                    implementation.cell_ownership if declared else (contract.raster.cell_ownership)
                ),
                "population_points": population.count,
                "population_sources": list(population.sources),
                "owned_points": int((contract_cells >= 0).sum()),
            },
            "tolerance": {
                "absolute": contract.tolerance.absolute,
                "relative": contract.tolerance.relative,
            },
            "comparison": primary.to_record(),
        }
        if declared and implementation.cell_ownership != contract.raster.cell_ownership:
            versus_contract = compare_band(actual, contract_ref, contract.tolerance, grid)
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
        if boundary:
            reasons.append(
                f"{boundary} point(s) are owned by a different cell under "
                f"{implementation.cell_ownership} than under {contract.raster.cell_ownership}"
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
