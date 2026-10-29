"""Tests for portable paths and machine-local data roots."""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

from lidarbench.datasets.roots import (
    ROOTS_ENV,
    DataRootError,
    DataRoots,
    check_relative_path,
    default_roots_file,
    load_data_roots,
    parse_root_overrides,
)


@pytest.mark.parametrize(
    "value",
    [
        "a/b.laz",
        "full/prepared/1_org/tile_1000_density_1",
        "tile 1.laz",
        "ümlaut/ok.las",
    ],
)
def test_portable_relative_paths_accepted(value):
    assert check_relative_path(value, "where") == value


@pytest.mark.parametrize(
    "value",
    [
        "",
        "/abs/path",
        "C:/data",
        "c:relative",
        "a\\b",
        "a/../b",
        "./a",
        "a//b",
        "con/x",
        "a/NUL.txt",
        "a/LPT1",
        "trailing./x",
        "space /x",
        "a:b",
        "q?",
        "tab\tname",
        3,
    ],
)
def test_non_portable_paths_rejected(value):
    with pytest.raises(DataRootError):
        check_relative_path(value, "where")


def test_empty_path_allowed_only_when_requested():
    assert check_relative_path("", "w", allow_empty=True) == ""


def test_roots_file_parsing_and_relative_paths(tmp_path):
    file = tmp_path / "roots.toml"
    file.write_text(
        'schema = "lidarbench.data-roots/1"\n[roots]\nlocal = "data"\n'
        f'abs = "{(tmp_path / "x").as_posix()}"\n',
        encoding="utf-8",
    )
    roots = load_data_roots(file, environ={})
    assert roots.path("local") == tmp_path / "data"
    assert roots.path("abs") == tmp_path / "x"
    assert roots.sources["local"] == os.fspath(file)


@pytest.mark.parametrize(
    "text, message",
    [
        ('schema = "other"\n', "schema"),
        ('schema = "lidarbench.data-roots/1"\nextra = 1\n', "unknown key"),
        ('schema = "lidarbench.data-roots/1"\n[roots]\nBad_Label = "x"\n', "label"),
        ('schema = "lidarbench.data-roots/1"\n[roots]\nok = 3\n', "roots.ok"),
        ("not toml [", "cannot read"),
    ],
)
def test_roots_file_errors(tmp_path, text, message):
    file = tmp_path / "roots.toml"
    file.write_text(text, encoding="utf-8")
    with pytest.raises(DataRootError, match=message):
        load_data_roots(file, environ={})


def test_explicit_missing_file_is_an_error_default_is_not(tmp_path, monkeypatch):
    with pytest.raises(DataRootError):
        load_data_roots(tmp_path / "missing.toml", environ={})
    with pytest.raises(DataRootError):
        load_data_roots(environ={ROOTS_ENV: os.fspath(tmp_path / "missing.toml")})
    monkeypatch.setenv("XDG_CONFIG_HOME", os.fspath(tmp_path / "cfg"))
    monkeypatch.setenv("APPDATA", os.fspath(tmp_path / "cfg"))
    monkeypatch.setenv("HOME", os.fspath(tmp_path / "home"))
    monkeypatch.setenv("USERPROFILE", os.fspath(tmp_path / "home"))
    assert load_data_roots(environ={}).roots == {}


def test_env_file_and_overrides_take_precedence(tmp_path):
    file = tmp_path / "roots.toml"
    file.write_text(
        'schema = "lidarbench.data-roots/1"\n[roots]\na = "one"\nb = "two"\n', encoding="utf-8"
    )
    roots = load_data_roots(
        overrides={"b": tmp_path / "override"}, environ={ROOTS_ENV: os.fspath(file)}
    )
    assert roots.path("a") == tmp_path / "one"
    assert roots.path("b") == tmp_path / "override"
    assert roots.sources["b"] == "override"
    assert roots.to_record()["b"]["source"] == "override"


@pytest.mark.skipif(sys.platform == "win32", reason="XDG layout is POSIX-only")
def test_default_roots_file_location(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_CONFIG_HOME", os.fspath(tmp_path))
    expected = (
        Path.home() / "Library/Application Support/lidarbench/data-roots.toml"
        if sys.platform == "darwin"
        else tmp_path / "lidarbench" / "data-roots.toml"
    )
    assert default_roots_file() == expected


def test_overrides_parsing():
    parsed = parse_root_overrides(["data=/x/y", "b=rel"])
    assert parsed["data"] == Path(os.path.abspath("/x/y"))
    for bad in ("nolabel", "=path", "Bad=x", "x="):
        with pytest.raises(DataRootError):
            parse_root_overrides([bad])


def test_unknown_label_names_the_fix():
    with pytest.raises(DataRootError, match="--root missing=PATH"):
        DataRoots().path("missing")


def test_resolve_joins_portable_path(tmp_path):
    roots = DataRoots({"r": tmp_path})
    assert roots.resolve("r", "a/b") == tmp_path / "a" / "b"
    assert roots.resolve("r", "") == tmp_path
    with pytest.raises(DataRootError):
        roots.resolve("r", "../escape")


def test_resolve_refuses_link_escaping_root(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    try:
        (root / "link").symlink_to(outside, target_is_directory=True)
    except (OSError, NotImplementedError):
        pytest.skip("symlinks not permitted")
    with pytest.raises(DataRootError, match="outside data root"):
        DataRoots({"r": root}).resolve("r", "link")
