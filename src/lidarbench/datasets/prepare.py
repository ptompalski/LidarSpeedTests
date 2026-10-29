"""Dataset preparation: Python generators with interruption-safe publishing,
and an unchanged pass-through to the historical R preparation CLI.

Python preparation (``engine = "python"``; synthetic fixtures only):

1. refuse if the target directory exists (unless it already verifies against
   its registered manifest, which is reported and left untouched);
2. refuse if an earlier preparation of the same target was interrupted
   (``.<name>.lidarbench-partial-<token>`` staging directory or marker), unless
   ``discard_interrupted`` is given and the leftovers are provably ours;
3. write an ownership marker, generate into a fresh staging directory beside
   the target, inventory it, compare with any registered manifest, rename the
   staging directory into place, remove the marker, then register.

A crash at any step leaves either nothing, a marked staging directory, or a
complete target -- never a partially written dataset under its final name.

R preparation (``engine = "r-cli"``) is not reimplemented. lidarbench prints
the documented commands and, only on explicit request, runs one stage of the
recipe's entry point unchanged from the repository root.
"""

from __future__ import annotations

import os
import secrets
import shutil
import socket
import stat
import subprocess
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from lidarbench.core.store import publish_json, read_json
from lidarbench.datasets.fixtures import GENERATORS
from lidarbench.datasets.inventory import PARTIAL_MARKER, InventoryError, scan_directory
from lidarbench.datasets.recipes import DatasetSpec, Recipe
from lidarbench.datasets.register import build_manifest
from lidarbench.datasets.registry import DatasetRegistry
from lidarbench.datasets.roots import DataRoots
from lidarbench.datasets.verify import verify_dataset

MARKER_SCHEMA = "lidarbench.dataset-preparation/1"


class PreparationError(RuntimeError):
    """Preparation is unsupported, refused or failed."""


class InterruptedPreparationError(PreparationError):
    """Leftovers of an interrupted preparation must be inspected or discarded first."""


@dataclass(frozen=True)
class PreparationResult:
    """Outcome of a Python preparation."""

    dataset_id: str
    directory: Path
    status: str
    """``prepared`` or ``already-prepared``."""
    manifest_path: Path | None
    content_sha256: str


def _leftovers(target: Path) -> list[Path]:
    prefix = f".{target.name}{PARTIAL_MARKER}"
    if not target.parent.is_dir():
        return []
    return sorted(p for p in target.parent.iterdir() if p.name.startswith(prefix))


def _owned_tree(path: Path) -> bool:
    for dirpath, dirnames, filenames in os.walk(path, followlinks=False):
        for name in dirnames + filenames:
            mode = os.lstat(Path(dirpath) / name).st_mode
            if stat.S_ISLNK(mode) or not (stat.S_ISREG(mode) or stat.S_ISDIR(mode)):
                return False
    return True


def discard_interrupted(target: Path, dataset_id: str) -> list[Path]:
    """Remove leftovers of an interrupted preparation of ``target``.

    Only staging directories whose marker names ``dataset_id`` and that contain
    regular files only are removed; anything else is refused.

    Raises:
        InterruptedPreparationError: A leftover cannot be proven to be ours.
    """
    removed = []
    markers = {p for p in _leftovers(target) if p.suffix == ".json" and p.is_file()}
    dirs = [p for p in _leftovers(target) if p not in markers]
    for marker in markers:
        try:
            record = read_json(marker)
        except (OSError, ValueError):
            record = None
        if not isinstance(record, dict) or record.get("dataset_id") != dataset_id:
            raise InterruptedPreparationError(f"{marker}: not a marker for {dataset_id}")
    for staging in dirs:
        marker = staging.with_name(staging.name + ".json")
        if marker not in markers or staging.is_symlink() or not staging.is_dir():
            raise InterruptedPreparationError(
                f"{staging}: no matching ownership marker; inspect and remove it manually"
            )
        if not _owned_tree(staging):
            raise InterruptedPreparationError(f"{staging}: contains links or special files")
        shutil.rmtree(staging)
        removed.append(staging)
    for marker in markers:
        marker.unlink()
        removed.append(marker)
    return removed


def prepare_python(
    recipe: Recipe,
    spec: DatasetSpec,
    registry: DatasetRegistry,
    roots: DataRoots,
    *,
    discard: bool = False,
    register: bool = True,
) -> PreparationResult:
    """Generate one dataset of a ``python`` recipe (see module docstring).

    Raises:
        PreparationError: Unsupported recipe, existing target, failure.
        InterruptedPreparationError: Leftovers exist and ``discard`` is False.
    """
    prep = recipe.preparation
    if prep.engine != "python" or prep.generator not in GENERATORS:
        raise PreparationError(
            f"recipe {recipe.id} is not prepared by lidarbench (engine {prep.engine!r}); "
            "see `lidarbench data prepare --plan`"
        )
    if not roots.path(recipe.root).is_dir():
        raise PreparationError(
            f"data root {recipe.root!r} does not exist: {roots.path(recipe.root)}"
        )
    target = roots.resolve(recipe.root, spec.path)
    leftovers = _leftovers(target)
    if leftovers and not discard:
        raise InterruptedPreparationError(
            "an earlier preparation was interrupted; inspect, then rerun with "
            "--discard-interrupted: " + ", ".join(str(p) for p in leftovers)
        )
    if leftovers:
        discard_interrupted(target, spec.id)
    registered = registry.manifest(spec.id) if registry.has_manifest(spec.id) else None
    if target.exists() or target.is_symlink():
        if registered is not None:
            report = verify_dataset(registered, roots, directory=target)
            if report.status == "verified":
                return PreparationResult(
                    spec.id,
                    target,
                    "already-prepared",
                    registry.manifest_path(spec.id),
                    registered.content_sha256,
                )
            raise PreparationError(
                f"{target} exists but does not verify against its manifest: "
                + "; ".join(report.problems[:5])
            )
        raise PreparationError(f"{target} already exists; refusing to overwrite it")
    target.parent.mkdir(parents=True, exist_ok=True)
    token = secrets.token_hex(6)
    staging = target.with_name(f".{target.name}{PARTIAL_MARKER}{token}")
    marker = staging.with_name(staging.name + ".json")
    publish_json(
        marker,
        {
            "schema": MARKER_SCHEMA,
            "dataset_id": spec.id,
            "recipe": recipe.id,
            "target": target.name,
            "host": socket.gethostname(),
            "pid": os.getpid(),
        },
    )
    staging.mkdir()
    parameters = {**prep.parameters, **spec.parameters}
    try:
        GENERATORS[prep.generator](staging, parameters)
        scan = scan_directory(staging, recipe.layout)
    except (ValueError, OSError, InventoryError) as exc:
        raise PreparationError(
            f"{spec.id}: generation failed (staging kept: {staging}): {exc}"
        ) from exc
    manifest = build_manifest(recipe, spec, scan, {}, registry)
    if registered is not None and registered.content_sha256 != manifest.content_sha256:
        raise PreparationError(
            f"{spec.id}: generated bytes differ from the registered manifest "
            f"({manifest.content_sha256[:12]}… vs {registered.content_sha256[:12]}…); "
            f"staging kept for inspection: {staging}"
        )
    if target.exists():
        raise PreparationError(f"{target} appeared during preparation; staging kept: {staging}")
    os.rename(staging, target)
    marker.unlink()
    manifest_path = None
    if registered is not None:
        manifest_path = registry.manifest_path(spec.id)
    elif register:
        manifest_path, _ = registry.publish(manifest)
    return PreparationResult(spec.id, target, "prepared", manifest_path, manifest.content_sha256)


# --------------------------------------------------------------------------- R CLI


def r_cli_plan(recipe: Recipe) -> dict[str, Any]:
    """The documented R preparation commands of an ``r-cli`` recipe (nothing is run)."""
    prep = recipe.preparation
    if prep.engine != "r-cli":
        raise PreparationError(f"recipe {recipe.id} does not use the R preparation CLI")
    return {
        "recipe": recipe.id,
        "method": prep.method,
        "working_directory": "<repository root>",
        "commands": [
            ["Rscript", "--vanilla", prep.entry_point, stage, "<trusted config.R>"]
            for stage in prep.stages
        ],
        "config_example": prep.config_example,
        "requires": list(prep.requires),
        "documentation": prep.documentation,
        "note": (
            "lidarbench does not reimplement this preparation. The R CLI owns its "
            "identities, locks, resume rules and receipts; register its verified output "
            "afterwards with `lidarbench data register`."
        ),
    }


def run_r_stage(
    recipe: Recipe,
    stage: str,
    r_config: Path,
    repository: Path,
    *,
    rscript: str = "Rscript",
    extra_args: Sequence[str] = (),
) -> int:
    """Run one stage of the R preparation CLI unchanged; returns its exit status.

    Raises:
        PreparationError: Unknown stage, missing files or Rscript.
    """
    plan = r_cli_plan(recipe)
    if stage not in recipe.preparation.stages:
        raise PreparationError(f"stage {stage!r} is not one of {list(recipe.preparation.stages)}")
    entry = repository / str(recipe.preparation.entry_point)
    if not entry.is_file():
        raise PreparationError(f"{entry} not found; run from a source checkout")
    if not r_config.is_file():
        raise PreparationError(f"R configuration not found: {r_config}")
    executable = shutil.which(rscript)
    if executable is None:
        raise PreparationError(f"{rscript} not found; R is required for {plan['method']}")
    argv = [executable, "--vanilla", str(recipe.preparation.entry_point), stage, str(r_config)]
    argv += list(extra_args)
    return subprocess.run(argv, cwd=repository, check=False).returncode
