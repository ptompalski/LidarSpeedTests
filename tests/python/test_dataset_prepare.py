"""Tests for preparation (fixtures, interruption safety, R pass-through) and downloads."""

from __future__ import annotations

import hashlib
import io
import os
import sys
import tomllib
from pathlib import Path

import pytest
from dataset_helpers import DERIVED_RECIPE, copy_repo_recipes, make_registry

from lidarbench.datasets import prepare as prep_mod
from lidarbench.datasets.download import DownloadError, download_source, partial_path
from lidarbench.datasets.prepare import (
    InterruptedPreparationError,
    PreparationError,
    discard_interrupted,
    prepare_python,
    r_cli_plan,
    run_r_stage,
)
from lidarbench.datasets.recipes import parse_recipe
from lidarbench.datasets.roots import DataRoots
from lidarbench.datasets.verify import verify_dataset


def _fixture(tmp_path: Path):
    registry = copy_repo_recipes(tmp_path)
    recipe = registry.recipe("synthetic-tiles-v1")
    root = tmp_path / "fixtures"
    root.mkdir()
    return (
        registry,
        recipe,
        recipe.dataset("synthetic-tiles-v1/pair"),
        DataRoots({"fixtures": root}),
    )


def test_prepare_generates_registers_and_is_idempotent(tmp_path):
    registry, recipe, spec, roots = _fixture(tmp_path)
    result = prepare_python(recipe, spec, registry, roots)
    assert result.status == "prepared"
    manifest = registry.manifest(spec.id)
    assert manifest.kind == "fixture" and len(manifest.files) == 2
    assert verify_dataset(manifest, roots).status == "verified"
    assert sorted(p.name for p in result.directory.parent.iterdir()) == ["pair"]
    again = prepare_python(recipe, spec, registry, roots)
    assert again.status == "already-prepared"


def test_prepare_matches_the_committed_manifest_digest(tmp_path):
    from dataset_helpers import REPO_REGISTRY

    from lidarbench.datasets.registry import DatasetRegistry

    registry, recipe, spec, roots = _fixture(tmp_path)
    result = prepare_python(recipe, spec, registry, roots)
    assert result.content_sha256 == DatasetRegistry(REPO_REGISTRY).manifest(spec.id).content_sha256


def test_prepare_against_existing_manifest_without_republishing(tmp_path):
    registry, recipe, spec, roots = _fixture(tmp_path)
    prepare_python(recipe, spec, registry, roots)
    manifest_bytes = registry.manifest_path(spec.id).read_bytes()
    target = roots.resolve("fixtures", spec.path)
    for child in target.iterdir():
        child.unlink()
    target.rmdir()
    result = prepare_python(recipe, spec, registry, roots)
    assert result.status == "prepared"
    assert registry.manifest_path(spec.id).read_bytes() == manifest_bytes


def test_prepare_refuses_existing_unregistered_or_changed_target(tmp_path):
    registry, recipe, spec, roots = _fixture(tmp_path)
    target = roots.resolve("fixtures", spec.path)
    target.mkdir(parents=True)
    with pytest.raises(PreparationError, match="already exists"):
        prepare_python(recipe, spec, registry, roots)
    target.rmdir()
    prepare_python(recipe, spec, registry, roots)
    (target / "500000_5000000.las").write_bytes(b"changed")
    with pytest.raises(PreparationError, match="does not verify"):
        prepare_python(recipe, spec, registry, roots)


def test_prepare_refuses_generated_bytes_that_differ_from_manifest(tmp_path, monkeypatch):
    registry, recipe, spec, roots = _fixture(tmp_path)
    prepare_python(recipe, spec, registry, roots)
    target = roots.resolve("fixtures", spec.path)
    for child in target.iterdir():
        child.unlink()
    target.rmdir()

    from lidarbench.datasets import fixtures

    altered = dict(fixtures.GENERATORS)
    altered["synthetic-tiles-v1"] = lambda folder, params: fixtures.generate_synthetic_tiles(
        folder, {**params, "seed": 1}
    )
    monkeypatch.setattr(prep_mod, "GENERATORS", altered)
    with pytest.raises(PreparationError, match="differ from the registered manifest"):
        prepare_python(recipe, spec, registry, roots)
    assert not target.exists()
    assert len(list(target.parent.iterdir())) == 2  # staging + marker kept for inspection


def test_interrupted_preparation_is_refused_then_discarded(tmp_path, monkeypatch):
    registry, recipe, spec, roots = _fixture(tmp_path)

    def crash(folder, params):
        (folder / "500000_5000000.las").write_bytes(b"half")
        raise OSError("simulated crash")

    monkeypatch.setattr(prep_mod, "GENERATORS", {"synthetic-tiles-v1": crash})
    with pytest.raises(PreparationError, match="staging kept"):
        prepare_python(recipe, spec, registry, roots)
    target = roots.resolve("fixtures", spec.path)
    assert not target.exists()
    assert not registry.has_manifest(spec.id)
    monkeypatch.undo()
    with pytest.raises(InterruptedPreparationError, match="--discard-interrupted"):
        prepare_python(recipe, spec, registry, roots)
    result = prepare_python(recipe, spec, registry, roots, discard=True)
    assert result.status == "prepared"
    assert sorted(p.name for p in target.parent.iterdir()) == ["pair"]


def test_discard_refuses_unowned_leftovers(tmp_path):
    target = tmp_path / "pair"
    staging = tmp_path / ".pair.lidarbench-partial-abc"
    staging.mkdir()
    with pytest.raises(InterruptedPreparationError, match="no matching ownership marker"):
        discard_interrupted(target, "x/pair")
    marker = tmp_path / ".pair.lidarbench-partial-abc.json"
    marker.write_text('{"dataset_id": "other/pair"}', encoding="utf-8")
    with pytest.raises(InterruptedPreparationError, match="not a marker"):
        discard_interrupted(target, "x/pair")
    marker.write_text('{"dataset_id": "x/pair"}', encoding="utf-8")
    try:
        (staging / "link").symlink_to(tmp_path)
    except (OSError, NotImplementedError):
        pytest.skip("symlinks not permitted")
    with pytest.raises(InterruptedPreparationError, match="links"):
        discard_interrupted(target, "x/pair")
    (staging / "link").unlink()
    assert len(discard_interrupted(target, "x/pair")) == 2
    assert list(tmp_path.iterdir()) == []


def test_prepare_requires_existing_root_and_python_engine(tmp_path):
    registry, recipe, spec, _ = _fixture(tmp_path)
    with pytest.raises(PreparationError, match="does not exist"):
        prepare_python(recipe, spec, registry, DataRoots({"fixtures": tmp_path / "missing"}))
    registry2 = make_registry(tmp_path / "b", {"demo": DERIVED_RECIPE})
    demo = registry2.recipe("demo")
    with pytest.raises(PreparationError, match="not prepared by lidarbench"):
        prepare_python(demo, demo.datasets[0], registry2, DataRoots({"data": tmp_path}))


# --------------------------------------------------------------------------- R CLI


def test_r_cli_plan_lists_documented_commands(tmp_path):
    registry = copy_repo_recipes(tmp_path)
    plan = r_cli_plan(registry.recipe("petawawa-fresh-v4"))
    assert plan["commands"][0] == [
        "Rscript",
        "--vanilla",
        "R/prepare_fresh_data.R",
        "--download",
        "<trusted config.R>",
    ]
    assert any("LAStools" in r for r in plan["requires"])
    with pytest.raises(PreparationError):
        r_cli_plan(registry.recipe("synthetic-tiles-v1"))


@pytest.mark.skipif(sys.platform == "win32", reason="POSIX shebang fake Rscript")
def test_run_r_stage_passes_arguments_through_unchanged(tmp_path):
    registry = copy_repo_recipes(tmp_path)
    recipe = registry.recipe("petawawa-fresh-v4")
    repo = tmp_path / "repo"
    (repo / "R").mkdir(parents=True)
    (repo / "R" / "prepare_fresh_data.R").write_text("# fake", encoding="utf-8")
    config = tmp_path / "my config.R"
    config.write_text("list()", encoding="utf-8")
    log = tmp_path / "argv.txt"
    fake = tmp_path / "bin" / "Rscript"
    fake.parent.mkdir()
    fake.write_text(
        f"#!{sys.executable}\nimport os, sys\n"
        f"open({str(log)!r}, 'w').write(os.getcwd() + '\\n' + '\\n'.join(sys.argv[1:]))\n"
        "sys.exit(4)\n",
        encoding="utf-8",
    )
    fake.chmod(0o755)
    status = run_r_stage(
        recipe, "--verify-full", config, repo, rscript=str(fake), extra_args=["--cores", "4"]
    )
    assert status == 4
    cwd, *argv = log.read_text(encoding="utf-8").splitlines()
    assert Path(cwd).resolve() == repo.resolve()
    assert argv == [
        "--vanilla",
        "R/prepare_fresh_data.R",
        "--verify-full",
        str(config),
        "--cores",
        "4",
    ]
    with pytest.raises(PreparationError, match="stage"):
        run_r_stage(recipe, "--rebuild", config, repo, rscript=str(fake))
    with pytest.raises(PreparationError, match="not found"):
        run_r_stage(recipe, "--full", tmp_path / "none.R", repo, rscript=str(fake))
    with pytest.raises(PreparationError, match="R is required"):
        run_r_stage(recipe, "--full", config, repo, rscript="no-such-rscript-xyz")
    with pytest.raises(PreparationError, match="source checkout"):
        run_r_stage(recipe, "--full", config, tmp_path, rscript=str(fake))


# --------------------------------------------------------------------------- download

PAYLOAD = b"original archive bytes"


def _download_recipe(**download):
    data = tomllib.loads(DERIVED_RECIPE)
    data.update(kind="source")
    data["preparation"] = {"engine": "none", "method": "original"}
    data["download"] = {
        "url": "https://example.org/a.zip",
        "file": "a.zip",
        "bytes": len(PAYLOAD),
        "sha256": hashlib.sha256(PAYLOAD).hexdigest(),
        "md5": hashlib.md5(PAYLOAD).hexdigest(),
        "checksum_origin": "test",
        "terms_url": "https://example.org/terms",
        **download,
    }
    return parse_recipe(data)


class _Response(io.BytesIO):
    def __init__(self, data: bytes, status: int = 200, url: str = "https://example.org/a.zip"):
        super().__init__(data)
        self.status, self._url = status, url

    def geturl(self) -> str:
        return self._url


def test_download_requires_size_acknowledgement(tmp_path):
    recipe = _download_recipe()
    roots = DataRoots({"data": tmp_path})
    with pytest.raises(DownloadError, match="--accept-bytes"):
        download_source(recipe, roots, accept_bytes=None, opener=lambda r: _Response(PAYLOAD))


def test_download_publishes_only_verified_file(tmp_path):
    recipe = _download_recipe()
    roots = DataRoots({"data": tmp_path})
    requests = []

    def opener(request):
        requests.append(request)
        return _Response(PAYLOAD)

    path = download_source(recipe, roots, accept_bytes=len(PAYLOAD), opener=opener)
    assert path.read_bytes() == PAYLOAD and path == tmp_path / "stage" / "tiles" / "a.zip"
    assert requests[0].full_url.startswith("https://") and not requests[0].has_header("Range")
    with pytest.raises(DownloadError, match="already exists"):
        download_source(recipe, roots, accept_bytes=len(PAYLOAD), opener=opener)


def test_download_resumes_partial_transfer(tmp_path):
    recipe = _download_recipe()
    roots = DataRoots({"data": tmp_path})
    final = tmp_path / "stage" / "tiles" / "a.zip"
    final.parent.mkdir(parents=True)
    partial_path(final).write_bytes(PAYLOAD[:5])
    seen = []

    def opener(request):
        seen.append(request.get_header("Range"))
        return _Response(PAYLOAD[5:], status=206)

    download_source(recipe, roots, accept_bytes=len(PAYLOAD), opener=opener)
    assert seen == ["bytes=5-"] and final.read_bytes() == PAYLOAD
    assert not partial_path(final).exists()


def test_download_keeps_partial_on_checksum_mismatch(tmp_path):
    recipe = _download_recipe(md5="0" * 32)
    roots = DataRoots({"data": tmp_path})
    with pytest.raises(DownloadError, match="checksum mismatch"):
        download_source(
            recipe, roots, accept_bytes=len(PAYLOAD), opener=lambda r: _Response(PAYLOAD)
        )
    final = tmp_path / "stage" / "tiles" / "a.zip"
    assert not final.exists() and partial_path(final).read_bytes() == PAYLOAD


@pytest.mark.parametrize(
    "response, message",
    [
        (_Response(PAYLOAD, url="http://example.org/a.zip"), "non-HTTPS"),
        (_Response(PAYLOAD, status=404), "HTTP status"),
        (_Response(PAYLOAD + b"more"), "more bytes"),
        (_Response(PAYLOAD[:3]), "size"),
    ],
)
def test_download_failures(tmp_path, response, message):
    recipe = _download_recipe()
    with pytest.raises(DownloadError, match=message):
        download_source(
            recipe,
            DataRoots({"data": tmp_path}),
            accept_bytes=len(PAYLOAD),
            opener=lambda r: response,
        )


def test_download_refuses_recipes_without_download(tmp_path):
    registry = copy_repo_recipes(tmp_path)
    with pytest.raises(DownloadError, match="declares no download"):
        download_source(registry.recipe("petawawa-als-2012"), DataRoots(), accept_bytes=1)


def test_oversized_partial_is_refused(tmp_path):
    recipe = _download_recipe()
    final = tmp_path / "stage" / "tiles" / "a.zip"
    final.parent.mkdir(parents=True)
    partial_path(final).write_bytes(PAYLOAD + b"x")
    with pytest.raises(DownloadError, match="larger"):
        download_source(
            recipe,
            DataRoots({"data": tmp_path}),
            accept_bytes=len(PAYLOAD),
            opener=lambda r: _Response(b""),
        )
    assert os.path.exists(partial_path(final))
