"""``lidarbench data`` commands: list, show, roots, verify, register, prepare, download.

Exit status: 0 success; 1 failure (verification failed, refused or failed
registration/preparation); 2 usage or configuration error; 3 nothing failed
but at least one dataset is unavailable on this machine (data root not
configured). ``verify --quick`` reports ``consistent`` (exit 0), never ``verified``.
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from collections.abc import Callable, Sequence
from pathlib import Path
from typing import Any

from lidarbench.datasets.download import DownloadError, download_source
from lidarbench.datasets.manifest import DatasetManifest, ManifestError
from lidarbench.datasets.prepare import (
    PreparationError,
    prepare_python,
    r_cli_plan,
    run_r_stage,
)
from lidarbench.datasets.recipes import DatasetSpec, Recipe
from lidarbench.datasets.register import RegistrationError, register_dataset
from lidarbench.datasets.registry import DatasetRegistry, RegistryError, default_registry_dir
from lidarbench.datasets.roots import (
    DataRootError,
    DataRoots,
    default_roots_file,
    load_data_roots,
    parse_root_overrides,
)
from lidarbench.datasets.verify import verify_dataset

EXIT_OK, EXIT_FAILED, EXIT_USAGE, EXIT_UNAVAILABLE = 0, 1, 2, 3


def add_data_parser(subparsers: Any) -> None:
    """Register ``lidarbench data`` and its subcommands."""
    data = subparsers.add_parser(
        "data",
        help="list, verify, register and prepare benchmark datasets",
        description=(
            "Dataset manifests make benchmark inputs discoverable and verifiable on any "
            "OS. Exit 0: ok; 1: failed/refused; 2: usage error; 3: unavailable here."
        ),
    )
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--registry", help="registry directory (default: <checkout>/datasets)")
    common.add_argument("--roots-file", help="data-roots TOML (default: per-user file)")
    common.add_argument(
        "--root", action="append", metavar="LABEL=PATH", help="data root override (repeatable)"
    )
    common.add_argument("--json", action="store_true", help="print JSON")
    sub = data.add_subparsers(dest="data_command", required=True)

    sub.add_parser("list", parents=[common], help="registered datasets and recipes")
    show = sub.add_parser("show", parents=[common], help="show one manifest")
    show.add_argument("dataset", help="dataset ID")
    show.add_argument("--files", action="store_true", help="list every file")
    sub.add_parser("roots", parents=[common], help="data roots configured on this machine")

    verify = sub.add_parser(
        "verify", parents=[common], help="verify local files against manifests (read-only)"
    )
    verify.add_argument("dataset", nargs="*", help="dataset IDs")
    verify.add_argument("--all", action="store_true", help="every registered dataset")
    verify.add_argument(
        "--quick", action="store_true", help="sizes and headers only (never 'verified')"
    )
    verify.add_argument(
        "--provenance", action="store_true", help="also verify external provenance receipts"
    )

    register = sub.add_parser(
        "register", parents=[common], help="hash existing data and publish manifests (no R)"
    )
    register.add_argument("recipe", help="recipe ID")
    register.add_argument("--dataset", action="append", metavar="ID", help="only these datasets")
    register.add_argument(
        "--legacy-export",
        metavar="FILE",
        help="R receipt export (bridges/r/export_petawawa_receipts.R) to cross-check MD5s",
    )
    register.add_argument(
        "--skip-legacy-crosscheck",
        action="store_true",
        help="register without the legacy receipt cross-check (recorded in the manifest)",
    )

    prepare = sub.add_parser(
        "prepare",
        parents=[common],
        help="generate fixture datasets; show or run the R preparation CLI",
    )
    prepare.add_argument("recipe", help="recipe ID")
    prepare.add_argument("--dataset", action="append", metavar="ID", help="only these datasets")
    prepare.add_argument(
        "--discard-interrupted",
        action="store_true",
        help="remove provably owned leftovers of an interrupted preparation first",
    )
    prepare.add_argument("--no-register", action="store_true", help="do not publish a manifest")
    prepare.add_argument("--plan", action="store_true", help="only print the preparation plan")
    prepare.add_argument(
        "--run-r-stage",
        metavar="STAGE",
        help="run one R CLI stage unchanged, e.g. --run-r-stage=--verify-full",
    )
    prepare.add_argument("--r-config", metavar="FILE", help="trusted R configuration for the stage")
    prepare.add_argument("--rscript", default="Rscript", help="Rscript executable")
    prepare.add_argument("--cores", type=int, help="passed to the R CLI as --cores N")

    download = sub.add_parser(
        "download", parents=[common], help="download original source data (explicit only)"
    )
    download.add_argument("recipe", help="source recipe ID")
    download.add_argument(
        "--accept-bytes", type=int, metavar="N", help="acknowledge the exact download size"
    )


def _emit(as_json: bool, record: Any, text: Callable[[], None]) -> None:
    if as_json:
        print(json.dumps(record, indent=2, sort_keys=True, ensure_ascii=False))
    else:
        text()


def _size(n: int) -> str:
    value = float(n)
    for unit in ("B", "KB", "MB", "GB", "TB"):
        if value < 1000 or unit == "TB":
            return f"{value:.1f} {unit}" if unit != "B" else f"{n} B"
        value /= 1000
    return f"{n} B"


class _Progress:
    """Throttled hashing progress on stderr."""

    def __init__(self, label: str, total: int, enabled: bool) -> None:
        self.label, self.total, self.enabled = label, total, enabled
        self.last = time.monotonic()

    def __call__(self, path: str, done: int) -> None:
        now = time.monotonic()
        if self.enabled and now - self.last >= 5:
            self.last = now
            share = f" of {_size(self.total)}" if self.total else ""
            print(f"  {self.label}: {_size(done)}{share} hashed ({path})", file=sys.stderr)


def _context(args: argparse.Namespace) -> tuple[DatasetRegistry, DataRoots]:
    registry = DatasetRegistry(Path(args.registry) if args.registry else default_registry_dir())
    roots = load_data_roots(
        Path(args.roots_file) if args.roots_file else None, parse_root_overrides(args.root)
    )
    return registry, roots


def _selected(recipe: Recipe, ids: Sequence[str] | None) -> list[DatasetSpec]:
    if not ids:
        return list(recipe.datasets)
    out = []
    for dataset_id in ids:
        try:
            out.append(recipe.dataset(dataset_id))
        except KeyError:
            raise RegistryError(
                f"recipe {recipe.id} has no dataset {dataset_id!r}; "
                f"known: {[d.id for d in recipe.datasets]}"
            ) from None
    return out


def data_command(args: argparse.Namespace) -> int:
    """Dispatch ``lidarbench data <subcommand>``."""
    try:
        registry, roots = _context(args)
        handler = {
            "list": _list,
            "show": _show,
            "roots": _roots,
            "verify": _verify,
            "register": _register,
            "prepare": _prepare,
            "download": _download,
        }[args.data_command]
        return handler(args, registry, roots)
    except (DataRootError, ManifestError, RegistryError) as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return EXIT_USAGE


def _list(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    manifests = registry.manifests()
    recipes = registry.recipes()
    registered = {m.dataset_id for m in manifests}
    record = {
        "registry": str(registry.root),
        "datasets": [
            {**m.summary(), "root_configured": m.location.root in roots.roots} for m in manifests
        ],
        "recipes": [
            {
                **r.summary(),
                "unregistered": [d.id for d in r.datasets if d.id not in registered],
            }
            for r in recipes
        ],
    }

    def text() -> None:
        print(f"Registry: {registry.root}")
        print(f"\nRegistered datasets ({len(manifests)}):")
        for m in manifests:
            here = "" if m.location.root in roots.roots else "  [root not configured]"
            print(
                f"  {m.dataset_id:48} {m.kind:8} {m.version:20} {len(m.files):5} files "
                f"{_size(m.total_bytes):>10}  {m.content_sha256[:12]}{here}"
            )
        print(f"\nRecipes ({len(recipes)}):")
        for r in recipes:
            missing = [d.id for d in r.datasets if d.id not in registered]
            print(f"  {r.id:32} {r.kind:8} engine={r.preparation.engine:6} {r.title}")
            if missing:
                print(f"      not registered: {', '.join(missing)}")

    _emit(args.json, record, text)
    return EXIT_OK


def _show(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    manifest = registry.manifest(args.dataset)
    record = manifest.to_record() if args.files else _overview(manifest)

    def text() -> None:
        s = manifest.summary()
        print(f"{manifest.dataset_id}  ({manifest.kind}, version {manifest.version})")
        print(f"  {manifest.title}")
        print(f"  location        {manifest.location.root}:{manifest.location.path or '.'}")
        print(
            f"  files           {s['file_count']} ({s['input_count']} inputs), "
            f"{_size(s['total_bytes'])}"
        )
        print(f"  content_sha256  {s['content_sha256']}")
        print(f"  manifest_sha256 {s['manifest_sha256']}")
        print(
            f"  license         {manifest.license.name} "
            f"(redistribution: {manifest.license.redistribution})"
        )
        print(f"  source          {manifest.provenance.source_citation}")
        prep = manifest.provenance.preparation
        if prep:
            print(f"  preparation     {prep.get('method')} via {prep.get('engine')}")
        for record_ in manifest.provenance.records:
            print(f"  receipt         {record_.location.path}  {record_.sha256[:12]}")
        for note in manifest.provenance.notes:
            print(f"  note            {note}")
        if args.files:
            for f in manifest.files:
                print(f"    {f.role:9} {f.bytes:>12}  {f.sha256[:16]}  {f.path}")

    _emit(args.json, record, text)
    return EXIT_OK


def _overview(manifest: DatasetManifest) -> dict[str, Any]:
    record = manifest.to_record()
    record.pop("files")
    record["summary"] = manifest.summary()
    return record


def _roots(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    record: dict[str, Any] = {
        "default_file": str(default_roots_file()),
        "roots": {
            label: {**info, "exists": Path(info["path"]).is_dir()}
            for label, info in roots.to_record().items()
        },
    }

    def text() -> None:
        print(f"Default roots file: {default_roots_file()}")
        if not roots.roots:
            print("No data roots configured.")
        for label, info in record["roots"].items():
            state = "" if info["exists"] else "  [missing]"
            print(f"  {label:24} {info['path']}  ({info['source']}){state}")

    _emit(args.json, record, text)
    return EXIT_OK


def _verify(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    if args.all == bool(args.dataset):
        print("Error: name dataset IDs or use --all (not both)", file=sys.stderr)
        return EXIT_USAGE
    manifests = registry.manifests() if args.all else [registry.manifest(d) for d in args.dataset]
    mode = "quick" if args.quick else "full"
    reports = []
    for manifest in manifests:
        progress = _Progress(manifest.dataset_id, manifest.total_bytes, not args.json)
        report = verify_dataset(
            manifest, roots, mode=mode, provenance=args.provenance, progress=progress
        )
        reports.append(report)
        if not args.json:
            print(
                f"{report.status.upper():12} {manifest.dataset_id}  "
                f"({report.files_checked} files, {_size(report.bytes_hashed)} hashed)",
                flush=True,
            )
            for problem in report.problems[:20]:
                print(f"    {problem}")
            if len(report.problems) > 20:
                print(f"    ... {len(report.problems) - 20} more")
    if args.json:
        print(json.dumps([r.to_record() for r in reports], indent=2, sort_keys=True))
    if any(r.status == "failed" for r in reports):
        return EXIT_FAILED
    if any(r.status == "unavailable" for r in reports):
        return EXIT_UNAVAILABLE
    return EXIT_OK


def _register(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    recipe = registry.recipe(args.recipe)
    export = Path(args.legacy_export) if args.legacy_export else None
    results = []
    status = EXIT_OK
    for spec in _selected(recipe, args.dataset):
        if not args.json:
            print(f"Registering {spec.id} ...", file=sys.stderr)
        try:
            result = register_dataset(
                recipe,
                spec,
                registry,
                roots,
                legacy_export=export,
                skip_legacy_crosscheck=args.skip_legacy_crosscheck,
                progress=_Progress(spec.id, 0, not args.json),
            )
        except RegistrationError as exc:
            print(f"REFUSED {spec.id}: {exc}", file=sys.stderr)
            results.append({"dataset_id": spec.id, "status": "refused", "reason": str(exc)})
            status = EXIT_FAILED
            continue
        state = "registered" if result.created else "already-registered"
        results.append({**result.manifest.summary(), "status": state, "path": str(result.path)})
        if not args.json:
            print(
                f"{state.upper():18} {spec.id}  {result.manifest.content_sha256}  {result.path}",
                flush=True,
            )
    if args.json:
        print(json.dumps(results, indent=2, sort_keys=True))
    return status


def _prepare(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    recipe = registry.recipe(args.recipe)
    engine = recipe.preparation.engine
    if engine == "none":
        print(
            f"Error: {recipe.id} is original source data; it is never prepared "
            "(see `lidarbench data download`)",
            file=sys.stderr,
        )
        return EXIT_FAILED
    if engine == "r-cli":
        return _prepare_r(args, recipe)
    if args.plan:
        _emit(
            args.json,
            recipe.preparation.to_record(),
            lambda: print(json.dumps(recipe.preparation.to_record(), indent=2)),
        )
        return EXIT_OK
    status = EXIT_OK
    results: list[dict[str, Any]] = []
    for spec in _selected(recipe, args.dataset):
        try:
            result = prepare_python(
                recipe,
                spec,
                registry,
                roots,
                discard=args.discard_interrupted,
                register=not args.no_register,
            )
        except PreparationError as exc:
            print(f"FAILED {spec.id}: {exc}", file=sys.stderr)
            results.append({"dataset_id": spec.id, "status": "failed", "reason": str(exc)})
            status = EXIT_FAILED
            continue
        results.append(
            {
                "dataset_id": result.dataset_id,
                "status": result.status,
                "directory": str(result.directory),
                "manifest": None if result.manifest_path is None else str(result.manifest_path),
                "content_sha256": result.content_sha256,
            }
        )
        if not args.json:
            print(f"{result.status.upper():17} {result.dataset_id}  {result.directory}")
    if args.json:
        print(json.dumps(results, indent=2, sort_keys=True))
    return status


def _prepare_r(args: argparse.Namespace, recipe: Recipe) -> int:
    plan = r_cli_plan(recipe)
    if not args.run_r_stage:

        def text() -> None:
            print(f"{recipe.id}: prepared by the historical R CLI ({plan['method']}).")
            print("lidarbench does not reproduce it. Commands (from the repository root):")
            for command in plan["commands"]:
                print("  " + " ".join(command))
            print("Requires:")
            for item in plan["requires"]:
                print(f"  - {item}")
            print(f"Configuration example: {plan['config_example']}")
            print(f"Documentation: {plan['documentation']}")
            print(plan["note"])

        _emit(args.json, plan, text)
        return EXIT_OK
    if not args.r_config:
        print("Error: --run-r-stage requires --r-config", file=sys.stderr)
        return EXIT_USAGE
    repository = default_registry_dir().parent
    extra = ["--cores", str(args.cores)] if args.cores is not None else []
    try:
        return run_r_stage(
            recipe,
            args.run_r_stage,
            Path(args.r_config).resolve(),
            repository,
            rscript=args.rscript,
            extra_args=extra,
        )
    except PreparationError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return EXIT_FAILED


def _download(args: argparse.Namespace, registry: DatasetRegistry, roots: DataRoots) -> int:
    recipe = registry.recipe(args.recipe)
    last = [time.monotonic()]

    def progress(done: int, total: int) -> None:
        if time.monotonic() - last[0] >= 5:
            last[0] = time.monotonic()
            print(f"  {_size(done)} of {_size(total)}", file=sys.stderr)

    try:
        path = download_source(recipe, roots, accept_bytes=args.accept_bytes, progress=progress)
    except (DownloadError, OSError) as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return EXIT_FAILED
    print(f"Downloaded and checked {path}; register it with `lidarbench data register {recipe.id}`")
    return EXIT_OK
