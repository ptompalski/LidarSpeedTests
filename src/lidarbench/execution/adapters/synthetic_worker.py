"""Worker process for the synthetic test adapter (stdlib only).

Run as a subprocess by :class:`~lidarbench.execution.adapters.synthetic.SyntheticAdapter`::

    python -m lidarbench.execution.adapters.synthetic_worker checksum --output-dir DIR INPUT...
    python -m lidarbench.execution.adapters.synthetic_worker hashchain --job JOB.json

The work is deterministic: the same input bytes and parameters always produce
byte-identical outputs. The pure functions here are also used by tests to
compute the known outputs. The worker performs no timing.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from collections.abc import Sequence
from pathlib import Path
from typing import Any

OUTPUT_SCHEMA = "lidarbench.synthetic-output/1"
JOB_SCHEMA = "lidarbench.synthetic-job/1"
HASHCHAIN_DOMAIN = b"lidarbench-synthetic-hashchain/1\n"
MAX_ITERATIONS = 100_000_000
_CHUNK = 1 << 20


def render(record: dict[str, Any]) -> str:
    """Canonical JSON text: sorted keys, compact, UTF-8, trailing newline."""
    return json.dumps(record, sort_keys=True, separators=(",", ":"), ensure_ascii=False) + "\n"


def file_sha256(path: Path) -> tuple[int, str]:
    """Return ``(byte_count, sha256_hex)`` of a file."""
    digest = hashlib.sha256()
    size = 0
    with open(path, "rb") as handle:
        while chunk := handle.read(_CHUNK):
            digest.update(chunk)
            size += len(chunk)
    return size, digest.hexdigest()


def checksum_output_name(input_path: Path) -> str:
    """Name of the checksum output for one input."""
    return f"{input_path.name}.checksum.json"


def checksum_record(input_path: Path) -> dict[str, Any]:
    """Checksum output record for one input."""
    size, sha = file_sha256(input_path)
    return {
        "schema": OUTPUT_SCHEMA,
        "workload_id": "synthetic.checksum",
        "input_name": input_path.name,
        "bytes": size,
        "sha256": sha,
    }


def hashchain_digest(input_sha256: Sequence[str], iterations: int) -> str:
    """Iterate SHA-256 ``iterations`` times over a seed built from input digests."""
    if not 1 <= iterations <= MAX_ITERATIONS:
        raise ValueError(f"iterations must be in 1..{MAX_ITERATIONS}")
    state = hashlib.sha256(HASHCHAIN_DOMAIN + "\n".join(input_sha256).encode("ascii")).digest()
    for _ in range(iterations):
        state = hashlib.sha256(state).digest()
    return state.hex()


def hashchain_record(inputs: Sequence[Path], iterations: int) -> dict[str, Any]:
    """Hash-chain output record for a set of inputs."""
    entries = []
    for path in inputs:
        _, sha = file_sha256(path)
        entries.append({"name": path.name, "sha256": sha})
    return {
        "schema": OUTPUT_SCHEMA,
        "workload_id": "synthetic.hashchain",
        "inputs": entries,
        "iterations": iterations,
        "digest": hashchain_digest([e["sha256"] for e in entries], iterations),
    }


def _write_new(path: Path, text: str) -> None:
    with open(path, "x", encoding="utf-8", newline="") as handle:
        handle.write(text)


def _run_checksum(output_dir: Path, inputs: list[Path]) -> None:
    for path in inputs:
        _write_new(output_dir / checksum_output_name(path), render(checksum_record(path)))


def _run_hashchain(job_path: Path) -> None:
    job = json.loads(job_path.read_text(encoding="utf-8"))
    if job.get("schema") != JOB_SCHEMA:
        raise ValueError(f"unexpected job schema: {job.get('schema')!r}")
    inputs = [Path(p) for p in job["inputs"]]
    record = hashchain_record(inputs, int(job["iterations"]))
    _write_new(Path(job["output"]), render(record))


def main(argv: Sequence[str] | None = None) -> int:
    """Worker entry point; returns a process exit code."""
    parser = argparse.ArgumentParser(prog="synthetic_worker")
    sub = parser.add_subparsers(dest="mode", required=True)
    checksum = sub.add_parser("checksum")
    checksum.add_argument("--output-dir", type=Path, required=True)
    checksum.add_argument("inputs", type=Path, nargs="+")
    hashchain = sub.add_parser("hashchain")
    hashchain.add_argument("--job", type=Path, required=True)
    args = parser.parse_args(argv)

    try:
        if args.mode == "checksum":
            _run_checksum(args.output_dir, args.inputs)
        else:
            _run_hashchain(args.job)
    except (OSError, ValueError, KeyError) as exc:
        print(f"ERROR: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
