#!/usr/bin/env python3
"""Convert the prepared LAZ tiles (every density variant) to COPC with PDAL.

Each ``<tile>.laz`` under ``SOURCE/tile_1000_density_*`` becomes
``DEST/tile_1000_density_*/<tile>.copc.laz`` (LAS 1.4, point format 6, the
original scale and offset, CRS forwarded). Nothing in SOURCE is modified.

Per file: ``pdal pipeline`` (readers.las -> writers.copc) into a ``.partial``
file, then verification, then an atomic rename. Verification (default
``full``) decodes both files with laspy and requires the same point count and
the identical multiset of (X, Y, Z, intensity, return number, number of
returns, classification, GPS time, point source ID); the COPC point order
differs by design. ``--verify header`` only compares counts, bounds and scales.

Finished files are skipped on re-runs (resume after interruption); their
manifest entries are kept. ``DEST/copc-manifest.json`` records the PDAL
version, the pipeline template and, per file, source/output SHA-256, point
counts and the verification result. With ``--protect`` (default) outputs and
density directories are made read-only once a density is complete, so no
benchmarked tool can modify them.

Usage::

    uv run python scripts/convert_to_copc.py --dry-run
    uv run python scripts/convert_to_copc.py --jobs 16
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import stat
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any

WORKDIR = Path("/data/nvme2/LidarSpeedTestWorkingDir")
DEFAULT_SOURCE = WORKDIR / "input_data_no_synthetic/full/prepared/1_org"
DEFAULT_DEST = WORKDIR / "input_data_copc/full/prepared/1_org"
DEFAULT_PDAL = Path.home() / "micromamba/envs/pdal/bin/pdal"
MANIFEST = "copc-manifest.json"
SCHEMA = "lidarbench.copc-conversion/1"
SUFFIX = ".copc.laz"
COMPARED = (
    "X",
    "Y",
    "Z",
    "intensity",
    "return_number",
    "number_of_returns",
    "classification",
    "gps_time",
    "point_source_id",
)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 22), b""):
            digest.update(block)
    return digest.hexdigest()


def pdal_version(pdal: Path) -> str:
    out = subprocess.run([pdal, "--version"], capture_output=True, text=True, check=True).stdout
    return next(line.strip() for line in out.splitlines() if line.strip().startswith("pdal "))


def header(path: Path) -> dict[str, Any]:
    import laspy

    with laspy.open(path) as reader:
        h = reader.header
        return {
            "version": str(h.version),
            "point_format": h.point_format.id,
            "count": int(h.point_count),
            "scales": [float(v) for v in h.scales],
            "offsets": [float(v) for v in h.offsets],
            "mins": [float(v) for v in h.mins],
            "maxs": [float(v) for v in h.maxs],
            "copc": any(getattr(v, "user_id", "") == "copc" for v in h.vlrs),
        }


def pipeline(source: Path, target: Path, src: dict[str, Any], threads: int) -> dict[str, Any]:
    sx, sy, sz = src["scales"]
    ox, oy, oz = src["offsets"]
    return {
        "pipeline": [
            {"type": "readers.las", "filename": os.fspath(source)},
            {
                "type": "writers.copc",
                "filename": os.fspath(target),
                # writers.copc defaults to a 0.01 scale; keep the source quantization exactly.
                "scale_x": sx,
                "scale_y": sy,
                "scale_z": sz,
                "offset_x": ox,
                "offset_y": oy,
                "offset_z": oz,
                "forward": "all",
                "threads": threads,
            },
        ]
    }


def columns(path: Path) -> dict[str, Any]:
    import laspy
    import numpy as np

    las = laspy.read(path)
    out = {}
    for i, name in enumerate(COMPARED):
        if name in ("X", "Y", "Z"):
            # Integer coordinates in the shared scale: exact, independent of offsets.
            scaled = np.asarray(getattr(las, name.lower()))
            out[name] = np.rint(scaled / las.header.scales[i]).astype(np.int64)
        else:
            out[name] = np.asarray(las[name]).astype(np.float64 if name == "gps_time" else np.int64)
    return out


def verify_full(source: Path, target: Path) -> str | None:
    import numpy as np

    a, b = columns(source), columns(target)
    if len(a["X"]) != len(b["X"]):
        return f"point count {len(a['X'])} != {len(b['X'])}"
    keys = list(reversed(COMPARED))
    ia, ib = np.lexsort([a[k] for k in keys]), np.lexsort([b[k] for k in keys])
    for name in COMPARED:
        if not np.array_equal(a[name][ia], b[name][ib]):
            return f"{name} values differ after sorting"
    return None


def verify_header(src: dict[str, Any], out: dict[str, Any]) -> str | None:
    if not out["copc"]:
        return "output has no COPC info VLR"
    if out["count"] != src["count"]:
        return f"point count {src['count']} != {out['count']}"
    if out["scales"] != src["scales"]:
        return f"scales {src['scales']} != {out['scales']}"
    for key in ("mins", "maxs"):
        if any(abs(p - q) > 1e-9 for p, q in zip(src[key], out[key], strict=True)):
            return f"bounds {key} {src[key]} != {out[key]}"
    return None


def convert(source: Path, target: Path, pdal: Path, threads: int, verify: str) -> dict[str, Any]:
    started = time.time()
    partial = target.with_name(target.name + ".partial")
    partial.unlink(missing_ok=True)
    src = header(source)
    document = pipeline(source, partial, src, threads)
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as handle:
        json.dump(document, handle)
        config = Path(handle.name)
    try:
        done = subprocess.run(
            [pdal, "pipeline", config], capture_output=True, text=True, check=False
        )
    finally:
        config.unlink(missing_ok=True)
    record: dict[str, Any] = {"source": os.fspath(source), "output": os.fspath(target)}
    if done.returncode != 0 or not partial.is_file():
        partial.unlink(missing_ok=True)
        record["error"] = f"pdal exited {done.returncode}: {done.stderr.strip()[-2000:]}"
        return record
    out = header(partial)
    problem = verify_header(src, out)
    if problem is None and verify == "full":
        problem = verify_full(source, partial)
    if problem is not None:
        partial.unlink(missing_ok=True)
        record["error"] = f"verification failed: {problem}"
        return record
    os.replace(partial, target)
    record.update(
        source_sha256=sha256(source),
        output_sha256=sha256(target),
        source_header=src,
        output_header=out,
        verification=verify,
        seconds=round(time.time() - started, 1),
    )
    return record


def protect(paths: list[Path]) -> None:
    for path in paths:
        mode = path.stat().st_mode
        path.chmod(mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=(__doc__ or "").split("\n\n")[0])
    parser.add_argument("--source", type=Path, default=DEFAULT_SOURCE)
    parser.add_argument("--dest", type=Path, default=DEFAULT_DEST)
    parser.add_argument("--pdal", type=Path, default=DEFAULT_PDAL)
    parser.add_argument("--densities", nargs="*", help="e.g. 1 10 (default: every variant)")
    parser.add_argument("--jobs", type=int, default=8, help="files converted concurrently")
    parser.add_argument("--threads", type=int, default=4, help="writers.copc threads per file")
    parser.add_argument("--verify", choices=("full", "header"), default="full")
    parser.add_argument("--no-protect", dest="protect", action="store_false")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)

    if not args.pdal.is_file():
        parser.error(f"pdal not found: {args.pdal}")
    dirs = sorted(
        (d for d in args.source.glob("tile_*_density_*") if d.is_dir()),
        key=lambda d: (len(d.name), d.name),
    )
    if args.densities:
        wanted = {f"density_{v}" for v in args.densities}
        dirs = [d for d in dirs if any(d.name.endswith(w) for w in wanted)]
    if not dirs:
        parser.error(f"no tile_*_density_* directories under {args.source}")

    jobs: list[tuple[Path, Path]] = []
    for d in dirs:
        for source in sorted(d.glob("*.laz")):
            jobs.append((source, args.dest / d.name / (source.stem + SUFFIX)))
    todo = [(s, t) for s, t in jobs if not t.exists()]
    size = sum(s.stat().st_size for s, _ in todo)
    print(
        f"{len(jobs)} file(s) in {len(dirs)} density variant(s); {len(todo)} to convert "
        f"({size / 2**30:.1f} GiB of LAZ) -> {args.dest}"
    )
    existing = next(p for p in (args.dest, *args.dest.parents) if p.exists())
    free = shutil.disk_usage(existing).free
    print(f"free space at destination: {free / 2**30:.0f} GiB; verification: {args.verify}")
    if args.dry_run:
        for d in dirs:
            n = sum(1 for s, _ in todo if s.parent == d)
            print(f"  {d.name}: {n} to convert")
        return 0
    if free < 1.5 * size:
        print("not enough free space (need ~1.5x the source size)", file=sys.stderr)
        return 2

    manifest_path = args.dest / MANIFEST
    args.dest.mkdir(parents=True, exist_ok=True)
    manifest: dict[str, Any] = {"files": {}}
    if manifest_path.is_file():
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    template_src = {"scales": ["source"] * 3, "offsets": ["source"] * 3}
    manifest.update(
        schema=SCHEMA,
        source_root=os.fspath(args.source),
        pdal=os.fspath(args.pdal),
        pdal_version=pdal_version(args.pdal),
        pipeline_template=pipeline(Path("SOURCE"), Path("TARGET"), template_src, args.threads),
        compared_dimensions=list(COMPARED),
    )
    for d in dirs:
        (args.dest / d.name).mkdir(parents=True, exist_ok=True)

    def save() -> None:
        tmp = manifest_path.with_suffix(".json.partial")
        tmp.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        os.replace(tmp, manifest_path)

    failures = 0
    started = time.time()
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {
            pool.submit(convert, s, t, args.pdal, args.threads, args.verify): t for s, t in todo
        }
        for i, future in enumerate(as_completed(futures), start=1):
            target = futures[future]
            try:
                record = future.result()
            except Exception as exc:
                record = {"output": os.fspath(target), "error": f"{type(exc).__name__}: {exc}"}
            key = os.fspath(target.relative_to(args.dest))
            manifest["files"][key] = record
            status = record.get("error") or f"ok ({record['seconds']} s)"
            failures += "error" in record
            elapsed = time.time() - started
            print(f"[{i}/{len(todo)}] {key}: {status}  [{elapsed / 60:.1f} min]", flush=True)
            if i % 10 == 0:
                save()
    save()

    for d in dirs:
        out_dir = args.dest / d.name
        expected = {t for s, t in jobs if s.parent == d}
        complete = all(t.is_file() for t in expected)
        if args.protect and complete:
            protect([*sorted(expected), out_dir])
        state = "complete" if complete else "INCOMPLETE"
        print(f"{d.name}: {state}{' (read-only)' if args.protect and complete else ''}")
    print(f"manifest: {manifest_path}")
    if failures:
        print(f"{failures} file(s) failed; re-run to retry them", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
