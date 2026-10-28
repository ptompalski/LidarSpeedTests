"""Tests for platform detection and utilities."""

import platform
from pathlib import Path

import pytest

from lidarbench.core import platform as platform_module


class TestPlatformInfo:
    """Tests for platform information."""

    def test_get_platform_info(self):
        """Test that platform info is retrieved correctly."""
        info = platform_module.get_platform_info()
        assert info.system in ("Linux", "Windows", "Darwin")
        assert info.architecture
        assert info.python_version
        assert info.cpu_count > 0
        assert info.total_memory_bytes > 0

    def test_platform_system_match(self):
        """Test that detected system matches platform.system()."""
        info = platform_module.get_platform_info()
        assert info.system == platform.system()


class TestPlatformChecks:
    """Tests for platform check functions."""

    def test_platform_checks_mutually_exclusive(self):
        """Test that only one platform check returns True."""
        checks = [
            platform_module.is_linux(),
            platform_module.is_windows(),
            platform_module.is_macos(),
        ]
        assert sum(checks) == 1

    @pytest.mark.skipif(
        platform.system() != "Linux", reason="Linux-specific test"
    )
    def test_is_linux_on_linux(self):
        """Test is_linux returns True on Linux."""
        assert platform_module.is_linux()

    @pytest.mark.skipif(
        platform.system() != "Windows", reason="Windows-specific test"
    )
    def test_is_windows_on_windows(self):
        """Test is_windows returns True on Windows."""
        assert platform_module.is_windows()

    @pytest.mark.skipif(
        platform.system() != "Darwin", reason="macOS-specific test"
    )
    def test_is_macos_on_macos(self):
        """Test is_macos returns True on macOS."""
        assert platform_module.is_macos()


class TestExecutableExtension:
    """Tests for executable extension."""

    @pytest.mark.skipif(platform.system() != "Windows", reason="Windows-specific")
    def test_exe_extension_on_windows(self):
        """Test .exe extension on Windows."""
        assert platform_module.get_executable_extension() == ".exe"

    @pytest.mark.skipif(platform.system() == "Windows", reason="Unix-specific")
    def test_no_extension_on_unix(self):
        """Test no extension on Unix-like systems."""
        assert platform_module.get_executable_extension() == ""


class TestPathSeparator:
    """Tests for path separator."""

    @pytest.mark.skipif(platform.system() != "Windows", reason="Windows-specific")
    def test_semicolon_on_windows(self):
        """Test ; separator on Windows."""
        assert platform_module.get_path_separator() == ";"

    @pytest.mark.skipif(platform.system() == "Windows", reason="Unix-specific")
    def test_colon_on_unix(self):
        """Test : separator on Unix-like systems."""
        assert platform_module.get_path_separator() == ":"


class TestWhich:
    """Tests for the which() function."""

    def test_which_finds_python(self):
        """Test that which finds Python."""
        result = platform_module.which("python")
        assert result is not None or platform_module.which("python3") is not None

    def test_which_returns_path_object(self):
        """Test that which returns a Path object."""
        result = platform_module.which("python3")
        if result is not None:
            assert isinstance(result, Path)

    def test_which_not_found(self):
        """Test that which returns None for nonexistent executable."""
        result = platform_module.which("nonexistent_executable_12345")
        assert result is None

    def test_which_with_explicit_paths(self):
        """Test which with explicit path list."""
        import sys
        paths = [str(Path(sys.executable).parent)]
        result = platform_module.which("python", paths)
        # Should find python or python3
        assert result is not None or platform_module.which("python3", paths) is not None
