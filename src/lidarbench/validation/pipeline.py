"""Validation pipeline: artifacts -> structural -> numerical, outside any timing.

:func:`validate_outputs` inspects the outputs of one finished attempt. It never
launches a benchmarked tool, never modifies outputs or inputs, and runs after
the timed interval has ended. Each level only runs when the previous level
passed; otherwise it is ``not_run`` with the reason, so a failed or incomplete
check can never produce a validated result (see :mod:`.status`).
"""

from __future__ import annotations

import time
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.core.clock import iso_utc
from lidarbench.validation.artifacts import Inventory, take_inventory
from lidarbench.validation.io import BackendStatus, probe_raster_backend
from lidarbench.validation.policies import Policies
from lidarbench.validation.status import (
    EXECUTION_SUCCEEDED,
    AttemptVerdicts,
    LevelVerdict,
    Status,
    combine,
)
from lidarbench.validation.validators import (
    ReferenceValidator,
    discover_validators,
    select_validator,
)
from lidarbench.workloads.contract import Implementation, WorkloadContract

REPORT_SCHEMA = "lidarbench.validation-report/1"
VALIDATION_MODES = ("numerical", "performance-only")


@dataclass(frozen=True)
class ValidationRequest:
    """Everything needed to validate one attempt's outputs.

    ``inputs`` must list every input tile given to the tool (neighbours supply the
    buffer). ``expected_epsg`` is the declared horizontal CRS of the inputs, or
    ``None`` when unknown.
    """

    contract: WorkloadContract
    tool: str
    inputs: tuple[Path, ...]
    output_dir: Path
    execution_status: str = EXECUTION_SUCCEEDED
    validation_mode: str = "numerical"
    expected_epsg: int | None = None
    policies: Policies = field(default_factory=Policies)

    def __post_init__(self) -> None:
        if self.validation_mode not in VALIDATION_MODES:
            raise ValueError(f"validation_mode must be one of {VALIDATION_MODES}")


@dataclass
class ValidationReport:
    """Outcome of :func:`validate_outputs`."""

    request: ValidationRequest
    verdicts: AttemptVerdicts
    inventory: Inventory | None
    artifacts: list[dict[str, Any]]
    backends: dict[str, Any]
    inputs: list[dict[str, Any]]
    input_digests: dict[str, str]
    started_wall_ns: int
    duration_ns: int

    def claims(self) -> dict[str, list[str]]:
        """Scientific claims this report establishes and those it leaves unverified."""
        v = self.verdicts
        c = self.request.contract
        impl = c.implementation(self.request.tool)
        established: list[str] = []
        open_: list[str] = [
            "Equivalence on inputs other than the ones validated here.",
            "Vertical datum and vertical units of Z (taken as supplied, never converted).",
            "Correctness of the input data or its provenance.",
            "Timing quality (validation never measures or adjusts performance).",
        ]
        if v.artifacts.status is Status.PASSED:
            established.append(
                "Every input tile has exactly its declared primary artifact(s); each is "
                "non-empty, has the expected format signature and decodes completely; no "
                "unexpected product is present."
            )
        if v.structural.status is Status.PASSED:
            established.append(
                f"Each raster is a north-up GeoTIFF with {c.raster.resolution} m cells on the "
                "zero-origin grid, the required band layout, no infinite values and the "
                "expected (or explicitly accepted) extent of its tile."
            )
        if v.numerical.status in (Status.PASSED, Status.ACCEPTED_EXCEPTION):
            convention = impl.cell_ownership if impl else "?"
            established.append(
                f"Every owned cell value and the NoData mask match an independent float64 "
                f"{c.statistic} of original Z (all points, {c.buffer} m buffer from "
                f"neighbouring tiles) under the {convention} cell convention within "
                f"|error| <= {c.tolerance.absolute} + {c.tolerance.relative}*|reference|."
            )
        else:
            open_.append(
                f"Numerical agreement of values and NoData masks ({v.numerical.status.value})."
            )
        if v.boundary_difference_points == 0 and v.numerical.status is Status.PASSED:
            established.append(
                f"The tool's cell ownership equals the contract's "
                f"{c.raster.cell_ownership} partition for every point of these inputs."
            )
        else:
            open_.append(
                "Cell ownership identical to the contract partition "
                f"(boundary_difference_points={v.boundary_difference_points})."
            )
        if v.comparability()[0].value != "comparable":
            open_.append("Cross-tool comparability of this timing.")
        if self.request.expected_epsg is None:
            open_.append("Horizontal CRS and metre units (no CRS was declared for the inputs).")
        return {"established": established, "not_established": open_}

    def to_record(self) -> dict[str, Any]:
        """The JSON report (``lidarbench.validation-report/1``)."""
        request = self.request
        contract = request.contract
        impl = contract.implementation(request.tool)
        return {
            "schema": REPORT_SCHEMA,
            "contract": {
                "key": contract.key,
                "digest": contract.digest,
                "title": contract.title,
                "equivalence_group": contract.equivalence_group,
                "cell_ownership": contract.raster.cell_ownership,
            },
            "tool": request.tool,
            "implementation": (
                None
                if impl is None
                else {
                    "status": impl.status.value,
                    "equivalence": impl.equivalence,
                    "cell_ownership": impl.cell_ownership,
                    "declared_comparability": contract.declared_comparability(request.tool),
                    "artifacts": list(impl.artifacts),
                    "note": impl.note,
                }
            ),
            "validation_mode": request.validation_mode,
            "expected_epsg": request.expected_epsg,
            "policies": request.policies.to_record(),
            "output_dir": str(request.output_dir),
            "inputs": self.inputs,
            "input_sha256": dict(sorted(self.input_digests.items())),
            "backends": self.backends,
            "inventory": self.inventory.to_record() if self.inventory else None,
            "artifacts": self.artifacts,
            "verdicts": self.verdicts.to_record(),
            "claims": self.claims(),
            "started_at": iso_utc(self.started_wall_ns),
            "validation_duration_ns": self.duration_ns,
            "timing_note": "Validation ran after the timed interval and is not part of any timing.",
        }


def _verdicts(
    request: ValidationRequest,
    impl: Implementation | None,
    artifacts: LevelVerdict,
    structural: LevelVerdict | None = None,
    numerical: LevelVerdict | None = None,
    boundary: int | None = None,
) -> AttemptVerdicts:
    def not_run(reason: str) -> LevelVerdict:
        return LevelVerdict.of(Status.NOT_RUN, reason)

    structural = structural or not_run(f"artifacts level is {artifacts.status.value}")
    numerical = numerical or not_run(f"structural level is {structural.status.value}")
    return AttemptVerdicts(
        execution_status=request.execution_status,
        artifacts=artifacts,
        structural=structural,
        numerical=numerical,
        implementation_status=impl.status.value if impl else "unsupported",
        equivalence=impl.equivalence if impl else "non_comparable",
        boundary_difference_points=boundary,
        validation_mode=request.validation_mode,
    )


def validate_outputs(
    request: ValidationRequest,
    validators: Sequence[ReferenceValidator] | None = None,
    *,
    known_digests: Mapping[str, str] | None = None,
    reference_cache: Any = None,
) -> ValidationReport:
    """Validate the outputs of one attempt against its workload contract.

    Args:
        request: What to validate.
        validators: Reference validators (default: discovered plugins).
        known_digests: Input path -> SHA-256 already verified by the caller (the
            campaign identity); inputs are then not re-hashed here.
        reference_cache: Optional :class:`~.validators.ReferenceCache` shared
            across calls; given to validators that accept a ``cache``.
    """
    started_wall = time.time_ns()
    started = time.perf_counter_ns()
    contract = request.contract
    impl = contract.implementation(request.tool)
    raster_backend = probe_raster_backend()
    if validators is None:
        validators, load_errors = discover_validators()
    else:
        load_errors = []
    if reference_cache is not None:
        for v in validators:
            if hasattr(v, "cache"):
                v.cache = reference_cache
    backends: dict[str, Any] = {"raster": raster_backend.to_record(), "load_errors": load_errors}
    report = ValidationReport(
        request=request,
        verdicts=AttemptVerdicts(request.execution_status),
        inventory=None,
        artifacts=[],
        backends=backends,
        inputs=[],
        input_digests={},
        started_wall_ns=started_wall,
        duration_ns=0,
    )

    def finish(verdicts: AttemptVerdicts) -> ValidationReport:
        report.verdicts = verdicts
        report.duration_ns = time.perf_counter_ns() - started
        return report

    if impl is None:
        reason = f"contract {contract.key} declares no implementation for tool {request.tool!r}"
        return finish(
            _verdicts(
                request,
                impl,
                LevelVerdict.of(Status.NOT_RUN, reason),
                LevelVerdict.of(Status.UNSUPPORTED, reason),
                LevelVerdict.of(Status.UNSUPPORTED, reason),
            )
        )
    if request.execution_status != EXECUTION_SUCCEEDED:
        reason = f"execution status is {request.execution_status}; outputs are not inspected"
        return finish(_verdicts(request, impl, LevelVerdict.of(Status.NOT_RUN, reason)))
    if not impl.status.dispatchable:
        return finish(
            _verdicts(
                request,
                impl,
                LevelVerdict.of(
                    Status.FAILED,
                    f"implementation is {impl.status.value} ({impl.reason}); a successful "
                    "execution contradicts the contract",
                ),
            )
        )

    # -- artifacts: inventory (stdlib), then complete decoding ------------------------
    inventory = take_inventory(contract, impl, request.inputs, request.output_dir)
    report.inventory = inventory
    if not inventory.complete:
        return finish(_verdicts(request, impl, LevelVerdict.of(Status.FAILED, *inventory.problems)))
    if not raster_backend.available:
        return finish(
            _verdicts(
                request,
                impl,
                LevelVerdict.of(
                    Status.UNAVAILABLE,
                    "inventory complete, but decoding cannot be checked: " + raster_backend.detail,
                ),
            )
        )
    from lidarbench.validation.io import ReadError, read_raster

    decoded = {}
    read_errors = []
    for artifact in inventory.expected:
        try:
            decoded[artifact.name] = read_raster(artifact.path)
        except ReadError as exc:
            read_errors.append(str(exc))
    if read_errors:
        return finish(_verdicts(request, impl, LevelVerdict.of(Status.FAILED, *read_errors)))
    artifacts_ok = LevelVerdict.of(Status.PASSED)

    # -- structural ----------------------------------------------------------------
    from lidarbench.validation.population import PopulationLoader, build_manifest
    from lidarbench.validation.structural import check_raster

    manifest = build_manifest(request.inputs)
    report.inputs = manifest.to_record()
    if manifest.problems:
        return finish(
            _verdicts(
                request,
                impl,
                artifacts_ok,
                LevelVerdict.of(
                    Status.FAILED, *(f"input manifest: {p}" for p in manifest.problems)
                ),
            )
        )
    structures = {}
    structural_errors: list[str] = []
    for artifact in inventory.expected:
        raster = decoded[artifact.name]
        structure = check_raster(
            raster,
            contract,
            impl,
            manifest.tiles[artifact.tile].bounds,
            request.expected_epsg,
            request.policies,
        )
        if (
            request.validation_mode == "performance-only" or not contract.has_reference
        ) and structure.finite_cells == 0:
            structure.errors.append(
                "no finite values; coverage cannot be confirmed without a reference"
            )
        structures[artifact.name] = structure
        structural_errors.extend(f"{artifact.name}: {e}" for e in structure.errors)
        report.artifacts.append(
            {
                "name": artifact.name,
                "tile": artifact.tile,
                "bytes": artifact.size,
                "raster": raster.to_record(),
                "structural": structure.to_record(),
            }
        )
    if structural_errors:
        return finish(
            _verdicts(
                request, impl, artifacts_ok, LevelVerdict.of(Status.FAILED, *structural_errors)
            )
        )
    structural_ok = LevelVerdict.of(Status.PASSED)

    # -- numerical -----------------------------------------------------------------
    if request.validation_mode == "performance-only":
        return finish(
            _verdicts(
                request,
                impl,
                artifacts_ok,
                structural_ok,
                LevelVerdict.of(
                    Status.UNVERIFIED,
                    "performance-only validation mode: no numerical reference was computed",
                ),
            )
        )
    if not contract.has_reference:
        accepted = sorted(
            {
                f"structural: {e}"
                for s in structures.values()
                for e in (*s.accepted_exceptions, *s.numerical_caps)
            }
        )
        return finish(
            _verdicts(
                request,
                impl,
                artifacts_ok,
                structural_ok,
                LevelVerdict.of(
                    Status.UNSUPPORTED,
                    f"contract {contract.key} declares no numerical reference "
                    "(reference.method = none): structural validation only",
                    *accepted,
                ),
            )
        )
    validator = select_validator(contract, validators)
    if validator is None:
        return finish(
            _verdicts(
                request,
                impl,
                artifacts_ok,
                structural_ok,
                LevelVerdict.of(
                    Status.UNSUPPORTED,
                    f"no validator provides reference method {contract.reference_method!r}",
                ),
            )
        )
    status: BackendStatus = validator.probe()
    backends["numerical"] = {
        "validator": f"{validator.name}/{validator.version}",
        **status.to_record(),
    }
    if not status.available:
        return finish(
            _verdicts(
                request,
                impl,
                artifacts_ok,
                structural_ok,
                LevelVerdict.of(Status.UNAVAILABLE, status.detail),
            )
        )
    loader = PopulationLoader(manifest, contract.buffer, known_digests=known_digests)
    statuses: list[Status] = []
    reasons: list[str] = []
    boundaries: list[int | None] = []
    for entry, artifact in zip(report.artifacts, inventory.expected, strict=True):
        result = validator.validate_artifact(
            contract, impl, artifact, decoded[artifact.name], structures[artifact.name], loader
        )
        entry["numerical"] = result.to_record()
        statuses.append(result.status)
        boundaries.append(result.boundary_difference_points)
        reasons.extend(f"{artifact.name}: {r}" for r in result.reasons)
    changed = loader.changed_inputs()
    if changed:
        statuses.append(Status.FAILED)
        reasons.append(f"inputs changed during validation: {changed}")
    report.input_digests = {
        manifest.tiles[stem].path.name: sha for stem, sha in loader.digests.items()
    }
    backends["decoding"] = {
        "decoded_files": loader.decoded_files,
        "decoded_points": loader.decoded_points,
        "input_digests": (
            "trusted from the caller (campaign identity)"
            if loader.trusted and len(loader.trusted) == len(loader.digests)
            else "hashed before decoding and re-checked after validation"
        ),
    }
    boundary = None if any(b is None for b in boundaries) else sum(b or 0 for b in boundaries)
    return finish(
        _verdicts(
            request,
            impl,
            artifacts_ok,
            structural_ok,
            LevelVerdict.of(combine(statuses), *reasons),
            boundary,
        )
    )
