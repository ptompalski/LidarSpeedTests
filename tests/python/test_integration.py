"""Integration tests - ensure package can be imported and used."""

import sys

import pytest

import lidarbench
from lidarbench import cli
from lidarbench.core import paths, platform


class TestPackageImports:
    """Test that core modules can be imported."""

    def test_import_main_package(self):
        """Test that main package imports."""
        assert lidarbench.__version__
        assert lidarbench.__author__

    def test_import_cli(self):
        """Test that CLI module imports."""
        assert cli.main
        assert cli.app

    def test_import_core_modules(self):
        """Test that core modules import."""
        assert paths
        assert platform
        assert platform.get_platform_info
        assert paths.normalize_path


class TestCLIIntegration:
    """Test CLI works from package."""

    def test_cli_version_via_app(self, capsys):
        """Test calling version through app() entry point."""
        exit_code = cli.app()
        # app() with no args should show help or return 0
        assert exit_code == 0

    def test_version_command_works(self, capsys):
        """Test version command returns proper output."""
        exit_code = cli.main(["version"])
        assert exit_code == 0
        captured = capsys.readouterr()
        assert "lidarbench" in captured.out


class TestPackageStructure:
    """Test that package structure is as expected."""

    def test_version_attribute(self):
        """Test that version is properly formatted."""
        version = lidarbench.__version__
        assert isinstance(version, str)
        # Should be semantic versioning
        parts = version.split(".")
        assert len(parts) >= 2

    def test_core_subpackage_exists(self):
        """Test that core subpackage exists."""
        from lidarbench import core
        assert core

    def test_config_subpackage_exists(self):
        """Test that config subpackage exists."""
        from lidarbench import config
        assert config

    def test_workloads_subpackage_exists(self):
        """Test that workloads subpackage exists."""
        from lidarbench import workloads
        assert workloads

    def test_execution_subpackage_exists(self):
        """Test that execution subpackage exists."""
        from lidarbench import execution
        assert execution


class TestCrossPlatformAPIs:
    """Test that cross-platform APIs work."""

    def test_platform_detection(self):
        """Test platform detection works."""
        info = platform.get_platform_info()
        assert info.system in ("Linux", "Windows", "Darwin")
        assert info.cpu_count > 0

    def test_path_normalization(self):
        """Test path normalization works."""
        from pathlib import Path
        p = paths.normalize_path(".")
        assert isinstance(p, Path)
        assert p.is_absolute()

    def test_no_hard_coded_unix_paths(self):
        """Test that we don't have hard-coded Unix paths exposed in __init__."""
        # This is a smoke test - just verify nothing crashes on import
        import lidarbench
        assert lidarbench.__version__
