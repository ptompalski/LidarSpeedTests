"""Untimed validation of attempt outputs inside the campaign runner.

After an attempt's process has finished and its outputs are inventoried, and
before its scratch directory is disposed, the runner validates the outputs
against the workload contract frozen in the campaign identity and writes
``validation/report.json`` (``lidarbench.validation-report/1``) into the
attempt, so the report is sealed by the attempt's digest chain. Validation is
never part of any timed interval.

Validation cost is bounded without weakening it:

* **Verdict reuse.** Validation is a deterministic function of the output bytes,
  the input bytes, the contract, the tool's declared implementation, the
  policies, the mode and the validator versions. When every output of an
  attempt is byte-identical (SHA-256) to the outputs of an attempt already
  validated under the same key, its verdict is reused and the report records
  the source attempt (``reuse``). Repetitions and CPU counts of deterministic
  tools therefore cost one hash comparison.
* **Shared references.** Per-tile references are cached across attempts
  (:class:`~lidarbench.validation.validators.ReferenceCache`) under all declared
  cell conventions, so the inputs are decoded once per workload and dataset
  rather than once per attempt.
* **Trusted input digests.** The inputs' SHA-256 come from the frozen identity
  (verified in full at the start of the run and by stat fingerprints around
  every entry) instead of being re-hashed twice per validation.
"""

from __future__ import annotations

import time
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.campaign.identity import CampaignIdentity, ResolvedTool
from lidarbench.campaign.store import AttemptInfo, timestamp
from lidarbench.config.campaign import CampaignConfig, WorkloadConfig
from lidarbench.core.canonical import digest, sha256_file
from lidarbench.core.store import publish_json, read_json
from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
from lidarbench.validation.policies import Policies
from lidarbench.validation.validators import (
    ReferenceCache,
    ReferenceValidator,
    discover_validators,
)
from lidarbench.workloads.contract import ContractError, WorkloadContract, load_suite

VALIDATION_DIR = "validation"
REPORT_REL = f"{VALIDATION_DIR}/report.json"
REUSE_KEY_SCHEMA = "lidarbench.validation-reuse-key/1"
REUSE_STATEMENT = (
    "Every output file is byte-identical (SHA-256) to the outputs of the source attempt, "
    "which was validated with the same contract, tool implementation, input bytes, policies, "
    "mode and validator versions. Validation is a deterministic function of these, so its "
    "verdict is reused."
)
FAILED_OUTCOMES = frozenset({"validation_failed", "validation_incomplete"})


@dataclass
class AttemptValidation:
    """What the runner records for one attempt's validation."""

    summary: dict[str, Any]
    statuses: dict[str, str] = field(default_factory=dict)
    """``artifacts_status``, ``structural_status``, ``numerical_status``."""

    @property
    def failed(self) -> bool:
        """Whether validation failed or could not complete."""
        return self.summary.get("outcome") in FAILED_OUTCOMES


class CampaignValidator:
    """Validates attempts of one run; holds the caches shared across attempts."""

    def __init__(
        self,
        config: CampaignConfig,
        identity: CampaignIdentity,
        published: Iterable[AttemptInfo] = (),
        validators: list[ReferenceValidator] | None = None,
        cache_bytes: int = 1 << 30,
    ) -> None:
        self.config = config
        self.identity = identity
        self.policies = Policies(
            accept_known_extent_discrepancies=config.validation.accept_known_extent_discrepancies
        )
        self.suite_error: str | None = None
        self.contracts: dict[str, WorkloadContract] = {}
        try:
            suite = load_suite(config.validation.suite_dir())
            self.contracts = dict(suite.contracts)
        except ContractError as exc:
            self.suite_error = str(exc)
        if validators is None:
            validators, self.load_errors = discover_validators()
        else:
            self.load_errors = []
        self.validators = validators
        self.cache = ReferenceCache(cache_bytes)
        self._index: dict[str, tuple[str, Path]] = {}
        for info in published:
            self._remember(info)
        self.validated = 0
        self.reused = 0

    # -- reuse index ------------------------------------------------------------

    def _remember(self, info: AttemptInfo) -> None:
        files = (info.result or {}).get("files") or {}
        if info.state != "completed" or REPORT_REL not in files:
            return
        try:
            record = read_json(info.path / REPORT_REL)
        except (OSError, ValueError):
            return
        key = record.get("reuse_key") if isinstance(record, dict) else None
        if isinstance(key, str):
            self._index.setdefault(key, (info.attempt_id, info.path / REPORT_REL))

    def frozen_contract(self, workload_id: str) -> tuple[WorkloadContract | None, str]:
        """The workload's contract if it matches the frozen identity, else a reason."""
        frozen = {
            w["id"]: w.get("contract") for w in self.identity.components["workloads"]["workloads"]
        }.get(workload_id)
        if self.suite_error:
            return None, f"workload suite unavailable: {self.suite_error}"
        contract = self.contracts.get(workload_id)
        if contract is None or not frozen or "digest" not in frozen:
            return None, f"no workload contract for {workload_id!r}; outputs are not validated"
        if frozen["digest"] != contract.digest:
            return None, (
                f"contract {contract.key} changed since the campaign was frozen "
                f"({frozen['digest'][:12]} != {contract.digest[:12]})"
            )
        return contract, ""

    def _reuse_key(
        self,
        contract: WorkloadContract,
        contract_tool: str,
        epsg: int | None,
        dataset_id: str,
        outputs: list[dict[str, Any]],
    ) -> str | None:
        if not self.config.validation.reuse_identical_outputs or not outputs:
            return None
        if any(not o.get("sha256") for o in outputs):
            return None
        dataset = self.identity.datasets[dataset_id].record
        return digest(
            {
                "schema": REUSE_KEY_SCHEMA,
                "contract": {"key": contract.key, "digest": contract.digest},
                "tool": contract_tool,
                "validation_mode": self.config.validation_mode,
                "expected_epsg": epsg,
                "policies": self.policies.to_record(),
                "validators": sorted(f"{v.name}/{v.version}" for v in self.validators),
                "inputs": dataset["inputs"],
                "input_files": sorted((f["path"], f["sha256"]) for f in dataset["files"]),
                "outputs": sorted((o["path"], o["bytes"], o["sha256"]) for o in outputs),
            }
        )

    # -- validation --------------------------------------------------------------

    def validate(
        self,
        *,
        attempt_id: str,
        attempt_dir: Path,
        workload: WorkloadConfig,
        tool: ResolvedTool,
        dataset_id: str,
        output_dir: Path,
        execution_status: str,
        outputs: list[dict[str, Any]],
    ) -> AttemptValidation | None:
        """Validate one attempt and publish its report; ``None`` if nothing applies.

        Only attempts whose execution succeeded are validated. Exceptions
        propagate; the runner records them without failing the campaign.
        """
        if execution_status != "succeeded":
            return None
        contract, reason = self.frozen_contract(workload.id)
        if contract is None:
            return AttemptValidation({"outcome": None, "report": None, "reason": reason})
        contract_tool = str(getattr(tool.adapter, "contract_tool", None) or tool.config.adapter)
        epsg_value = workload.parameters.get("epsg")
        epsg = (
            epsg_value if isinstance(epsg_value, int) and not isinstance(epsg_value, bool) else None
        )
        started = time.perf_counter_ns()
        key = self._reuse_key(contract, contract_tool, epsg, dataset_id, outputs)
        source = self._index.get(key) if key else None
        hits, misses = self.cache.hits, self.cache.misses
        if source is not None:
            source_id, source_path = source
            record = dict(read_json(source_path))
            record.update(
                output_dir=str(output_dir),
                started_at=timestamp(),
                validation_duration_ns=time.perf_counter_ns() - started,
                source_validation_duration_ns=record.get("validation_duration_ns"),
                reuse={
                    "source_attempt_id": source_id,
                    "source_report_sha256": sha256_file(source_path),
                    "statement": REUSE_STATEMENT,
                },
            )
            self.reused += 1
        else:
            dataset = self.identity.datasets[dataset_id]
            known = {
                str(dataset.root.joinpath(*f["path"].split("/"))): f["sha256"]
                for f in dataset.record["files"]
            }
            request = ValidationRequest(
                contract=contract,
                tool=contract_tool,
                inputs=tuple(dataset.inputs),
                output_dir=output_dir,
                execution_status=execution_status,
                validation_mode=self.config.validation_mode,
                expected_epsg=epsg,
                policies=self.policies,
            )
            report = validate_outputs(
                request, self.validators, known_digests=known, reference_cache=self.cache
            )
            record = report.to_record()
            record["reuse"] = None
            self.validated += 1
        record["reuse_key"] = key
        path = attempt_dir / REPORT_REL
        path.parent.mkdir(exist_ok=True)
        publish_json(path, record)
        if key is not None and source is None:
            self._index.setdefault(key, (attempt_id, path))
        verdicts: Mapping[str, Any] = record["verdicts"]
        statuses = {
            f"{level}_status": str(verdicts[level]["status"])
            for level in ("artifacts", "structural", "numerical")
        }
        summary = {
            "report": REPORT_REL,
            "contract": contract.key,
            "contract_tool": contract_tool,
            "outcome": verdicts.get("outcome"),
            "comparability": verdicts.get("comparability"),
            "boundary_difference_points": verdicts.get("boundary_difference_points"),
            "duration_ns": time.perf_counter_ns() - started,
            "reused_from": source[0] if source is not None else None,
            "reference_cache": {
                "hits": self.cache.hits - hits,
                "misses": self.cache.misses - misses,
            },
        }
        return AttemptValidation(summary, statuses)
