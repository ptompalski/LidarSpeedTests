"""Campaign configuration parsing and validation."""

from __future__ import annotations

from pathlib import Path

import pytest
from campaign_helpers import write_config, write_layout

from lidarbench.config.campaign import CACHE_POLICY_DESCRIPTIONS, ConfigError, load_config


@pytest.fixture
def base(tmp_path):
    return write_layout(tmp_path)


def test_defaults_are_resolved_and_recorded(base):
    config = load_config(write_config(base))
    assert config.campaign_id == "test-campaign"
    assert config.workspace == base / "workspace"
    assert config.scratch.roots == {"local": base / "scratch"}
    assert config.datasets[0].scratch == "local"
    assert [p.name for p in config.datasets[0].input_files()] == ["tile 1.laz", "tile2.laz"]
    record = config.to_record()
    assert record["scheduling"]["seed"] == 20261020
    assert record["scheduling"]["tool_order"] == "balanced"
    assert record["scheduling"]["cache_policy_description"] == (
        CACHE_POLICY_DESCRIPTIONS["uncontrolled-warm"]
    )
    assert record["monitoring"] == {
        "enabled": False,
        "backend": "psutil",
        "interval_s": 0.5,
        "required": True,
    }
    assert config.source_sha256 and config.source_path


@pytest.mark.parametrize(
    ("kwargs", "message"),
    [
        ({"extra": "bogus = 1"}, "unknown key"),
        ({"scheduling": 'cache_policy = "cold"'}, "cold-cache control is not implemented"),
        ({"scheduling": 'cache_policy = "drop-caches"'}, "cache_policy"),
        ({"scheduling": "seed = -1"}, "seed"),
        ({"scheduling": "repetitions = 0"}, "repetitions"),
        ({"scheduling": "cooldown_s = -1"}, "cooldown_s"),
        ({"scheduling": 'tool_order = "fixed"'}, "tool_order"),
        ({"cpu_counts": "[1, 1]"}, "duplicates"),
        ({"cpu_counts": "[0]"}, "cpu_counts"),
        ({"campaign_id": "../escape"}, "campaign_id"),
        ({"campaign_id": "CON"}, "portable"),
        ({"tools": '[[tools]]\nid = "a"\n[[tools]]\nid = "A"\nadapter = "synthetic"'}, "duplicate"),
        ({"workloads": '[[workloads]]\nid = "x"\ndatasets = ["nope"]'}, "datasets"),
        ({"workloads": '[[workloads]]\nid = "x"\nparameters = { a = [1] }'}, "parameters"),
        ({"monitoring": "interval_s = 0"}, "interval_s"),
    ],
)
def test_invalid_configs_fail_with_clear_messages(base, kwargs, message):
    path = write_config(base, **kwargs)
    with pytest.raises(ConfigError, match=message):
        load_config(path)


def test_scratch_label_required_with_several_roots(base):
    path = write_config(base)
    text = path.read_text(encoding="utf-8").replace(
        'roots = { local = "scratch" }', 'roots = { a = "scratch", b = "scratch" }'
    )
    path.write_text(text, encoding="utf-8")
    with pytest.raises(ConfigError, match="scratch must name"):
        load_config(path)


def test_dataset_without_matching_inputs(base):
    for f in (base / "data").glob("*.laz"):
        f.unlink()
    config = load_config(write_config(base))
    with pytest.raises(ConfigError, match="no files"):
        config.datasets[0].input_files()


def test_invalid_validation_mode(base):
    path = write_config(base)
    text = path.read_text(encoding="utf-8").replace('"performance-only"', '"fast"')
    path.write_text(text, encoding="utf-8")
    with pytest.raises(ConfigError, match="validation_mode"):
        load_config(path)


def test_invalid_toml(base):
    path = base / "bad.toml"
    path.write_text("schema = ", encoding="utf-8")
    with pytest.raises(ConfigError, match="invalid TOML"):
        load_config(path)


def test_example_config_is_valid():
    example = Path(__file__).parents[2] / "configs" / "examples" / "synthetic-campaign.toml"
    config = load_config(example)
    assert config.campaign_id == "synthetic-example-001"
    assert [w.id for w in config.workloads] == ["synthetic.hashchain", "synthetic.checksum"]
