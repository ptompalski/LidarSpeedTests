"""Tests for ``lidarbench data`` and for campaigns that use registered dataset manifests."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from campaign_helpers import make_runner, write_layout
from dataset_helpers import copy_repo_recipes

from lidarbench import cli
from lidarbench.campaign.identity import IdentityError
from lidarbench.config.campaign import ConfigError, load_config
from lidarbench.datasets.manifest import manifest_sha256


def _args(registry: Path, fixtures: Path) -> list[str]:
    return ["--registry", str(registry), "--root", f"fixtures={fixtures}"]


@pytest.fixture
def prepared(tmp_path):
    """A registry copy with both fixture datasets prepared and registered."""
    registry = copy_repo_recipes(tmp_path).root
    fixtures = tmp_path / "fixtures"
    fixtures.mkdir()
    assert cli.main(["data", "prepare", "synthetic-tiles-v1", *_args(registry, fixtures)]) == 0
    return registry, fixtures


def test_cli_list_show_roots(prepared, capsys):
    registry, fixtures = prepared
    assert cli.main(["data", "list", "--json", *_args(registry, fixtures)]) == 0
    listing = json.loads(capsys.readouterr().out)
    ids = [d["dataset_id"] for d in listing["datasets"]]
    assert ids == ["synthetic-tiles-v1/grid", "synthetic-tiles-v1/pair"]
    assert all(d["root_configured"] for d in listing["datasets"])
    petawawa = next(r for r in listing["recipes"] if r["id"] == "petawawa-fresh-v4")
    assert len(petawawa["unregistered"]) == 7
    assert cli.main(["data", "list", *_args(registry, fixtures)]) == 0
    assert "synthetic-tiles-v1/pair" in capsys.readouterr().out
    assert (
        cli.main(["data", "show", "synthetic-tiles-v1/pair", "--json", *_args(registry, fixtures)])
        == 0
    )
    shown = json.loads(capsys.readouterr().out)
    assert "files" not in shown and shown["summary"]["input_count"] == 2
    assert (
        cli.main(["data", "show", "synthetic-tiles-v1/pair", "--files", *_args(registry, fixtures)])
        == 0
    )
    assert "500100_5000000.las" in capsys.readouterr().out
    assert cli.main(["data", "roots", "--json", *_args(registry, fixtures)]) == 0
    assert json.loads(capsys.readouterr().out)["roots"]["fixtures"]["exists"] is True


def test_cli_verify_exit_codes(prepared, capsys):
    registry, fixtures = prepared
    assert cli.main(["data", "verify", "--all", *_args(registry, fixtures)]) == 0
    assert "VERIFIED" in capsys.readouterr().out
    assert cli.main(["data", "verify", "--all", "--registry", str(registry)]) in (0, 3)
    capsys.readouterr()
    roots_file = registry.parent / "empty-roots.toml"
    roots_file.write_text('schema = "lidarbench.data-roots/1"\n', encoding="utf-8")
    code = cli.main(
        [
            "data",
            "verify",
            "synthetic-tiles-v1/pair",
            "--registry",
            str(registry),
            "--roots-file",
            str(roots_file),
        ]
    )
    assert code == 3 and "UNAVAILABLE" in capsys.readouterr().out
    (fixtures / "synthetic-tiles-v1" / "pair" / "500000_5000000.las").write_bytes(b"x")
    code = cli.main(
        ["data", "verify", "synthetic-tiles-v1/pair", "--json", *_args(registry, fixtures)]
    )
    assert code == 1
    assert json.loads(capsys.readouterr().out)[0]["status"] == "failed"
    assert cli.main(["data", "verify", *_args(registry, fixtures)]) == 2
    assert cli.main(["data", "verify", "nope/x", *_args(registry, fixtures)]) == 2


def test_cli_prepare_variants(prepared, capsys):
    registry, fixtures = prepared
    capsys.readouterr()
    assert (
        cli.main(["data", "prepare", "synthetic-tiles-v1", "--json", *_args(registry, fixtures)])
        == 0
    )
    assert {r["status"] for r in json.loads(capsys.readouterr().out)} == {"already-prepared"}
    assert (
        cli.main(["data", "prepare", "synthetic-tiles-v1", "--plan", *_args(registry, fixtures)])
        == 0
    )
    assert "synthetic-tiles-v1" in capsys.readouterr().out
    assert cli.main(["data", "prepare", "petawawa-fresh-v4", *_args(registry, fixtures)]) == 0
    out = capsys.readouterr().out
    assert "does not reproduce it" in out and "--verify-full" in out
    code = cli.main(
        ["data", "prepare", "petawawa-fresh-v4", "--run-r-stage=--full", *_args(registry, fixtures)]
    )
    assert code == 2
    assert cli.main(["data", "prepare", "petawawa-als-2012", *_args(registry, fixtures)]) == 1
    assert (
        cli.main(
            [
                "data",
                "prepare",
                "synthetic-tiles-v1",
                "--dataset",
                "x/y",
                *_args(registry, fixtures),
            ]
        )
        == 2
    )


def test_cli_register_and_download_refusals(prepared, capsys):
    registry, fixtures = prepared
    code = cli.main(
        [
            "data",
            "register",
            "petawawa-fresh-v4",
            "--dataset",
            "petawawa-fresh-v4/tile_1000_density_1",
            "--root",
            f"petawawa-fresh={fixtures}",
            "--registry",
            str(registry),
        ]
    )
    assert code == 1
    assert "REFUSED" in capsys.readouterr().err
    code = cli.main(
        ["data", "register", "synthetic-tiles-v1", "--json", *_args(registry, fixtures)]
    )
    assert code == 0
    assert {r["status"] for r in json.loads(capsys.readouterr().out)} == {"already-registered"}
    assert cli.main(["data", "download", "petawawa-als-2012", *_args(registry, fixtures)]) == 1
    assert "declares no download" in capsys.readouterr().err


# --------------------------------------------------------------------------- campaigns


def _campaign(base: Path, registry: Path, fixtures: Path, dataset: str) -> Path:
    path = base / "campaign.toml"
    path.write_text(
        "\n".join(
            [
                'schema = "lidarbench.campaign-config/1"',
                'campaign_id = "manifest-campaign"',
                'workspace = "workspace"',
                "cpu_counts = [1]",
                'validation_mode = "performance-only"',
                "[scheduling]",
                "repetitions = 1\nwarmups = 0\ncooldown_s = 0",
                "[monitoring]",
                "enabled = false",
                "[scratch]",
                'roots = { local = "scratch" }',
                "[data]",
                f"registry = {json.dumps(str(registry))}",
                f"roots = {{ fixtures = {json.dumps(str(fixtures))} }}",
                "[[datasets]]",
                'id = "pair"',
                dataset,
                "[[workloads]]",
                'id = "synthetic.checksum"',
                "[[tools]]",
                'id = "synthetic"',
                "",
            ]
        ),
        encoding="utf-8",
    )
    return path


def test_campaign_uses_verified_manifest_inputs(prepared, tmp_path):
    registry, fixtures = prepared
    base = write_layout(tmp_path)
    config = load_config(
        _campaign(base, registry, fixtures, 'manifest = "synthetic-tiles-v1/pair"')
    )
    dataset = config.datasets[0]
    assert dataset.manifest is not None
    assert [p.name for p in dataset.input_files()] == ["500000_5000000.las", "500100_5000000.las"]
    record = dataset.to_record()
    assert record["manifest"]["dataset_id"] == "synthetic-tiles-v1/pair"
    assert "include" not in record

    summary = make_runner(base / "campaign.toml", base).run()
    assert summary.exit_code == 0
    frozen = summary.campaign_path / "identity" / "datasets.json"
    ds = json.loads(frozen.read_text(encoding="utf-8"))["datasets"][0]
    manifest = dataset.manifest
    assert ds["manifest"]["content_sha256"] == manifest.content_sha256
    assert ds["manifest"]["manifest_sha256"] == manifest_sha256(manifest)
    assert ds["inputs"] == ["500000_5000000.las", "500100_5000000.las"]
    assert [(f["path"], f["sha256"]) for f in ds["files"]] == [
        (f.path, f.sha256) for f in manifest.files
    ]


def test_campaign_refuses_bytes_that_differ_from_manifest(prepared, tmp_path):
    registry, fixtures = prepared
    base = write_layout(tmp_path)
    _campaign(base, registry, fixtures, 'manifest = "synthetic-tiles-v1/pair"')
    tile = fixtures / "synthetic-tiles-v1" / "pair" / "500100_5000000.las"
    data = bytearray(tile.read_bytes())
    data[-1] ^= 1
    tile.write_bytes(bytes(data))
    with pytest.raises(IdentityError, match="does not match manifest"):
        make_runner(base / "campaign.toml", base).dry_run()


@pytest.mark.parametrize(
    "dataset, message",
    [
        (
            'manifest = "synthetic-tiles-v1/pair"\nmanifest_sha256 = "' + "0" * 64 + '"',
            "manifest_sha256 does not match",
        ),
        ('manifest = "synthetic-tiles-v1/pair"\nmanifest_sha256 = "abc"', "hex digest"),
        ('manifest = "synthetic-tiles-v1/nope"', "not registered"),
        ('manifest = "synthetic-tiles-v1/pair"\npath = "data"', "exactly one"),
        ('manifest = "synthetic-tiles-v1/pair"\ninclude = ["*.las"]', "include is not allowed"),
        ('path = "data"\nmanifest_sha256 = "' + "0" * 64 + '"', "requires 'manifest'"),
        ("manifest = 3", "dataset ID string"),
    ],
)
def test_campaign_manifest_config_errors(prepared, tmp_path, dataset, message):
    registry, fixtures = prepared
    base = write_layout(tmp_path)
    with pytest.raises(ConfigError, match=message):
        load_config(_campaign(base, registry, fixtures, dataset))


def test_campaign_manifest_pin_and_unconfigured_root(prepared, tmp_path):
    registry, fixtures = prepared
    base = write_layout(tmp_path)
    from lidarbench.datasets.registry import DatasetRegistry

    digest = manifest_sha256(DatasetRegistry(registry).manifest("synthetic-tiles-v1/pair"))
    pinned = f'manifest = "synthetic-tiles-v1/pair"\nmanifest_sha256 = "{digest}"'
    assert load_config(_campaign(base, registry, fixtures, pinned)).datasets[0].manifest
    path = _campaign(base, registry, fixtures, 'manifest = "synthetic-tiles-v1/pair"')
    text = path.read_text(encoding="utf-8").replace("fixtures = ", "other = ")
    path.write_text(text, encoding="utf-8")
    roots_file = tmp_path / "roots.toml"
    roots_file.write_text('schema = "lidarbench.data-roots/1"\n', encoding="utf-8")
    text = text.replace("[data]\n", f"[data]\nroots_file = {json.dumps(str(roots_file))}\n")
    path.write_text(text, encoding="utf-8")
    with pytest.raises(ConfigError, match="not configured"):
        load_config(path)


def test_path_datasets_keep_their_frozen_record(tmp_path):
    """Existing path-based campaigns keep an unchanged dataset record (resume safety)."""
    from campaign_helpers import write_config

    base = write_layout(tmp_path)
    config = load_config(write_config(base))
    assert set(config.datasets[0].to_record()) == {"id", "path", "include", "scratch"}
