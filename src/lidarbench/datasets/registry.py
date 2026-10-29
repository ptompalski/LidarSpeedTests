"""The dataset registry: recipes and write-once manifests in a directory.

Layout (default: ``datasets/`` of the source checkout, or
``$LIDARBENCH_DATASET_REGISTRY``)::

    datasets/
      recipes/<recipe-id>.toml                 human-written, version-controlled
      manifests/<dataset-id>.json              generated once by registration

A dataset ID containing ``/`` maps to sub-directories
(``petawawa-fresh-v4/tile_1000_density_1`` ->
``manifests/petawawa-fresh-v4/tile_1000_density_1.json``). Manifests are
published with the write-once protocol of :mod:`lidarbench.core.store` and
never rewritten: data with different bytes needs a new dataset version.
"""

from __future__ import annotations

import os
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path

from lidarbench.core.store import RecordExistsError, publish_json, read_json
from lidarbench.datasets.manifest import (
    DatasetManifest,
    ManifestError,
    check_dataset_id,
    manifest_sha256,
    parse_manifest,
)
from lidarbench.datasets.recipes import Recipe, load_recipe

REGISTRY_ENV = "LIDARBENCH_DATASET_REGISTRY"


class RegistryError(LookupError):
    """A recipe or manifest is missing or conflicts with the registry."""


def default_registry_dir(environ: Mapping[str, str] | None = None) -> Path:
    """``$LIDARBENCH_DATASET_REGISTRY`` or ``datasets/`` of the source checkout.

    Like the workload suites, the registry lives beside the package in the
    repository, not in the installed wheel.
    """
    env = os.environ if environ is None else environ
    if env.get(REGISTRY_ENV):
        return Path(env[REGISTRY_ENV])
    package = Path(__file__).resolve().parents[1]
    return package.parents[1] / "datasets"


@dataclass(frozen=True)
class DatasetRegistry:
    """Recipes and manifests below ``root``."""

    root: Path

    @property
    def recipes_dir(self) -> Path:
        """``<root>/recipes``."""
        return self.root / "recipes"

    @property
    def manifests_dir(self) -> Path:
        """``<root>/manifests``."""
        return self.root / "manifests"

    def recipes(self) -> list[Recipe]:
        """All recipes, sorted by ID.

        Raises:
            ManifestError: A recipe file is invalid.
        """
        if not self.recipes_dir.is_dir():
            return []
        return [load_recipe(p) for p in sorted(self.recipes_dir.glob("*.toml"))]

    def recipe(self, recipe_id: str) -> Recipe:
        """Recipe by ID.

        Raises:
            RegistryError: No such recipe.
        """
        path = self.recipes_dir / f"{recipe_id}.toml"
        if "/" in recipe_id or "\\" in recipe_id or not path.is_file():
            known = [r.id for r in self.recipes()]
            raise RegistryError(f"unknown recipe {recipe_id!r} in {self.root}; known: {known}")
        return load_recipe(path)

    def recipe_for_dataset(self, dataset_id: str) -> Recipe:
        """The recipe that declares ``dataset_id``."""
        for recipe in self.recipes():
            if any(spec.id == dataset_id for spec in recipe.datasets):
                return recipe
        raise RegistryError(f"no recipe in {self.recipes_dir} declares dataset {dataset_id!r}")

    def manifest_path(self, dataset_id: str) -> Path:
        """Where the manifest of ``dataset_id`` lives."""
        check_dataset_id(dataset_id)
        return self.manifests_dir.joinpath(*dataset_id.split("/")).with_suffix(".json")

    def has_manifest(self, dataset_id: str) -> bool:
        """Whether ``dataset_id`` is registered."""
        return self.manifest_path(dataset_id).is_file()

    def manifest(self, dataset_id: str) -> DatasetManifest:
        """Load and validate a registered manifest.

        Raises:
            RegistryError: Not registered.
            ManifestError: Invalid manifest (message names the file and key).
        """
        try:
            path = self.manifest_path(dataset_id)
        except ManifestError as exc:
            raise RegistryError(str(exc)) from None
        if not path.is_file():
            raise RegistryError(f"dataset {dataset_id!r} is not registered in {self.root}")
        return load_manifest(path, dataset_id)

    def manifests(self) -> list[DatasetManifest]:
        """All registered manifests, sorted by dataset ID."""
        if not self.manifests_dir.is_dir():
            return []
        out = []
        for path in sorted(self.manifests_dir.rglob("*.json")):
            rel = path.relative_to(self.manifests_dir).with_suffix("").as_posix()
            out.append(load_manifest(path, rel))
        return sorted(out, key=lambda m: m.dataset_id)

    def publish(self, manifest: DatasetManifest) -> tuple[Path, bool]:
        """Publish a manifest once.

        Returns:
            (path, created). ``created`` is False if an identical-content manifest
            already exists (it is left untouched).

        Raises:
            RegistryError: A manifest with different content exists for the ID.
        """
        path = self.manifest_path(manifest.dataset_id)
        if path.is_file():
            existing = load_manifest(path, manifest.dataset_id)
            if existing.content_sha256 == manifest.content_sha256:
                return path, False
            raise RegistryError(
                f"dataset {manifest.dataset_id!r} is already registered with different content "
                f"({existing.content_sha256[:12]}… vs {manifest.content_sha256[:12]}…); manifests "
                "are never rewritten -- register the changed data under a new dataset version"
            )
        path.parent.mkdir(parents=True, exist_ok=True)
        try:
            publish_json(path, manifest.to_record())
        except RecordExistsError as exc:
            raise RegistryError(str(exc)) from None
        return path, True


def load_manifest(path: Path, expected_id: str | None = None) -> DatasetManifest:
    """Read and validate a manifest file.

    Raises:
        ManifestError: Unreadable or invalid, or its ID does not match its file name.
    """
    try:
        data = read_json(path)
    except (OSError, ValueError) as exc:
        raise ManifestError(f"{path}: cannot read manifest: {exc}") from exc
    try:
        manifest = parse_manifest(data)
    except ManifestError as exc:
        raise ManifestError(f"{path}: {exc}") from None
    if expected_id is not None and manifest.dataset_id != expected_id:
        raise ManifestError(
            f"{path}: dataset_id {manifest.dataset_id!r} does not match its location "
            f"({expected_id!r})"
        )
    return manifest


__all__ = [
    "REGISTRY_ENV",
    "DatasetRegistry",
    "RegistryError",
    "default_registry_dir",
    "load_manifest",
    "manifest_sha256",
]
