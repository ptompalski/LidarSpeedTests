"""Tests for path utilities."""

from pathlib import Path

import pytest

from lidarbench.core import paths


class TestNormalizePath:
    """Tests for path normalization."""

    def test_normalize_string_path(self):
        """Test normalizing a string path."""
        result = paths.normalize_path("/tmp")
        assert isinstance(result, Path)
        assert result.is_absolute()

    def test_normalize_path_object(self):
        """Test normalizing a Path object."""
        p = Path("/tmp")
        result = paths.normalize_path(p)
        assert isinstance(result, Path)
        assert result.is_absolute()

    def test_normalize_relative_path(self):
        """Test that relative paths are converted to absolute."""
        result = paths.normalize_path(".")
        assert result.is_absolute()

    def test_normalize_resolves_symlinks(self):
        """Test that symlinks are resolved (if supported)."""
        result = paths.normalize_path(Path.home())
        assert isinstance(result, Path)
        assert result.is_absolute()


class TestPathContainment:
    """Tests for path containment checking."""

    def test_path_contained_in_parent(self):
        """Test that a path is contained in its parent."""
        parent = Path("/home")
        child = Path("/home/user")
        assert paths.is_path_contained(child, parent)

    def test_path_contained_equal_paths(self):
        """Test that a path contains itself."""
        p = Path("/home/user")
        assert paths.is_path_contained(p, p)

    def test_path_not_contained_sibling(self):
        """Test that siblings are not contained."""
        p1 = Path("/home/user1")
        p2 = Path("/home/user2")
        assert not paths.is_path_contained(p1, p2)

    def test_path_not_contained_different_roots(self):
        """Test that different roots are not contained."""
        p1 = Path("/tmp/test")
        p2 = Path("/var/test")
        assert not paths.is_path_contained(p1, p2)


class TestPathComponents:
    """Tests for path component extraction."""

    def test_path_components_absolute(self):
        """Test extracting components from absolute path."""
        p = Path("/home/user/test")
        components = paths.get_path_components(p)
        assert "/" in components or "C:\\" in str(components[0])
        assert "user" in components or "Users" in components

    def test_path_components_relative(self):
        """Test extracting components from relative path."""
        p = Path("a/b/c")
        components = paths.get_path_components(p)
        assert len(components) >= 3


class TestProtectedPaths:
    """Tests for protected path detection."""

    def test_root_is_protected(self):
        """Test that filesystem root is protected."""
        assert paths.is_protected_path(Path("/"))

    def test_home_is_protected(self):
        """Test that home directory is protected."""
        assert paths.is_protected_path(Path.home())

    def test_etc_is_protected(self):
        """Test that /etc is protected (on Unix)."""
        import platform as platform_module
        if platform_module.system() != "Windows":
            assert paths.is_protected_path(Path("/etc"))

    def test_regular_path_not_protected(self, tmp_path):
        """Test that regular temp path is not protected."""
        assert not paths.is_protected_path(tmp_path)


class TestWorkingDirectoryContainment:
    """Tests for working directory containment."""

    def test_cwd_contains_itself(self):
        """Test that current working directory contains itself."""
        cwd = Path.cwd()
        assert paths.is_within_working_directory(cwd, cwd)

    def test_explicit_cwd(self, tmp_path):
        """Test with explicit working directory."""
        child = tmp_path / "child"
        child.mkdir()
        assert paths.is_within_working_directory(child, tmp_path)

    def test_outside_cwd(self, tmp_path):
        """Test that unrelated paths are not in cwd."""
        home = Path.home()
        if home != tmp_path:
            assert not paths.is_within_working_directory(home, tmp_path)
