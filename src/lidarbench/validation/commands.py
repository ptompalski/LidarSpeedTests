"""``lidarbench validate``: validate one output directory against a workload contract."""

from __future__ import annotations

import json
import sys
from pathlib import Path

from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
from lidarbench.validation.policies import Policies
from lidarbench.validation.status import Outcome
from lidarbench.workloads.contract import ContractError, default_suite_dir, load_suite

EXIT_VALIDATED = 0
EXIT_INVALID = 1
EXIT_USAGE = 2
EXIT_DESCRIPTIVE = 3
INPUT_PATTERNS = ("*.las", "*.laz", "*.LAS", "*.LAZ")


def _inputs(files: list[str] | None, directory: str | None) -> list[Path]:
    paths = [Path(f).resolve() for f in files or []]
    if directory:
        root = Path(directory)
        found = {p.resolve() for pattern in INPUT_PATTERNS for p in root.glob(pattern)}
        paths.extend(sorted(found))
    return paths


def validate_command(
    *,
    workload: str,
    tool: str,
    outputs: str,
    input_files: list[str] | None,
    input_dir: str | None,
    suite: str | None,
    epsg: int | None,
    mode: str,
    execution_status: str,
    accept_known_extent_discrepancies: bool,
    report_path: str | None,
    as_json: bool,
) -> int:
    """Run validation and print a summary; returns an exit code.

    Exit codes: 0 validated; 3 structurally successful but only descriptive
    (accepted exception or unverified); 1 failed, incomplete or not run;
    2 usage/contract errors.
    """
    try:
        contracts = load_suite(Path(suite) if suite else default_suite_dir())
        contract = contracts.get(workload)
    except ContractError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return EXIT_USAGE
    inputs = _inputs(input_files, input_dir)
    if not inputs:
        print("Error: no input tiles given (--input or --input-dir)", file=sys.stderr)
        return EXIT_USAGE
    if report_path and Path(report_path).exists():
        print(f"Error: report already exists (write-once): {report_path}", file=sys.stderr)
        return EXIT_USAGE
    request = ValidationRequest(
        contract=contract,
        tool=tool,
        inputs=tuple(inputs),
        output_dir=Path(outputs).resolve(),
        execution_status=execution_status,
        validation_mode=mode,
        expected_epsg=epsg,
        policies=Policies(accept_known_extent_discrepancies=accept_known_extent_discrepancies),
    )
    report = validate_outputs(request)
    record = report.to_record()
    if report_path:
        from lidarbench.core.store import publish_json

        publish_json(Path(report_path), record)
    if as_json:
        print(json.dumps(record, indent=2, sort_keys=True))
    else:
        v = record["verdicts"]
        print(f"{contract.key} / {tool}: {v['outcome']}")
        for level in ("artifacts", "structural", "numerical"):
            print(f"  {level:<11} {v[level]['status']}")
            for reason in v[level]["reasons"][:10]:
                print(f"    - {reason}")
        print(f"  comparability {v['comparability']}")
        for reason in v["comparability_reasons"]:
            print(f"    - {reason}")
        print(f"  boundary_difference_points {v['boundary_difference_points']}")
    outcome = report.verdicts.outcome
    if outcome is Outcome.VALIDATED:
        return EXIT_VALIDATED
    if outcome in (Outcome.ACCEPTED_EXCEPTION, Outcome.UNVERIFIED):
        return EXIT_DESCRIPTIVE
    return EXIT_INVALID
