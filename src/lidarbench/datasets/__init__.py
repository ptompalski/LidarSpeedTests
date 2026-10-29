"""Dataset manifests, recipes, registry, verification and preparation.

See ``design/DATASETS.md``. Importing this package stays stdlib-only; the
command layer (:mod:`lidarbench.datasets.commands`) is imported lazily by the CLI.
"""

from lidarbench.datasets.manifest import (
    MANIFEST_SCHEMA,
    DatasetManifest,
    FileEntry,
    ManifestError,
    manifest_sha256,
    parse_manifest,
)
from lidarbench.datasets.recipes import RECIPE_SCHEMA, Recipe, load_recipe
from lidarbench.datasets.registry import (
    DatasetRegistry,
    RegistryError,
    default_registry_dir,
    load_manifest,
)
from lidarbench.datasets.roots import DataRootError, DataRoots, load_data_roots
from lidarbench.datasets.verify import VerificationReport, verify_dataset

__all__ = [
    "MANIFEST_SCHEMA",
    "RECIPE_SCHEMA",
    "DataRootError",
    "DataRoots",
    "DatasetManifest",
    "DatasetRegistry",
    "FileEntry",
    "ManifestError",
    "Recipe",
    "RegistryError",
    "VerificationReport",
    "default_registry_dir",
    "load_data_roots",
    "load_manifest",
    "load_recipe",
    "manifest_sha256",
    "parse_manifest",
    "verify_dataset",
]
