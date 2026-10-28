"""Tests for adapter discovery and registration."""

from __future__ import annotations

from importlib.metadata import EntryPoint

import pytest
from adapter_fakes import FakePdalAdapter, FakeRscriptAdapter

from lidarbench.execution.adapters import ENTRY_POINT_GROUP, AdapterRegistry, ToolAdapter
from lidarbench.execution.adapters.synthetic import SyntheticAdapter


def ep(name: str, value: str) -> EntryPoint:
    return EntryPoint(name=name, value=value, group=ENTRY_POINT_GROUP)


class TestDiscovery:
    def test_installed_entry_points_include_synthetic(self):
        registry = AdapterRegistry.discover()
        assert "synthetic" in registry.ids()
        assert registry.adapter_class("synthetic") is SyntheticAdapter
        assert isinstance(registry.create("synthetic"), SyntheticAdapter)
        assert registry.failures == ()

    def test_third_party_adapter_via_entry_point(self):
        registry = AdapterRegistry.discover(
            [
                ep("fake-pdal", "adapter_fakes:FakePdalAdapter"),
                ep("synthetic", "lidarbench.execution.adapters.synthetic:SyntheticAdapter"),
            ]
        )
        assert registry.ids() == ["fake-pdal", "synthetic"]
        assert registry.adapter_class("fake-pdal") is FakePdalAdapter

    def test_broken_plugins_are_recorded_not_raised(self):
        registry = AdapterRegistry.discover(
            [
                ep("missing", "lidarbench_no_such_module:Adapter"),
                ep("notadapter", "pathlib:Path"),
                ep("wrongname", "adapter_fakes:FakePdalAdapter"),
                ep("abstract", "lidarbench.execution.adapters.base:ToolAdapter"),
                ep("synthetic", "lidarbench.execution.adapters.synthetic:SyntheticAdapter"),
            ]
        )
        assert registry.ids() == ["synthetic"]
        failures = {f.name: f.error for f in registry.failures}
        assert set(failures) == {"missing", "notadapter", "wrongname", "abstract"}
        assert "ModuleNotFoundError" in failures["missing"]
        assert "not a ToolAdapter" in failures["notadapter"]
        assert "must equal adapter_id" in failures["wrongname"]


class TestRegister:
    def test_register_and_create(self):
        registry = AdapterRegistry()
        registry.register(FakeRscriptAdapter)
        registry.register(FakeRscriptAdapter)  # idempotent for the same class
        assert "fake-rscript" in registry
        assert isinstance(registry.create("fake-rscript"), FakeRscriptAdapter)

    def test_duplicate_id_from_other_class_rejected(self):
        class Impostor(FakePdalAdapter):
            pass

        registry = AdapterRegistry()
        registry.register(FakePdalAdapter)
        with pytest.raises(ValueError, match="already registered"):
            registry.register(Impostor)

    def test_invalid_id_and_abstract_rejected(self):
        class BadId(FakePdalAdapter):
            adapter_id = "Bad ID"

        registry = AdapterRegistry()
        with pytest.raises(ValueError, match="invalid adapter_id"):
            registry.register(BadId)
        with pytest.raises(ValueError):
            registry.register(ToolAdapter)  # type: ignore[type-abstract]

    def test_unknown_adapter(self):
        with pytest.raises(KeyError, match="unknown adapter"):
            AdapterRegistry().create("nope")
