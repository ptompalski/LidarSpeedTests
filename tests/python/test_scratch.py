"""Run-owned scratch directories: root validation, creation and guarded cleanup."""

from __future__ import annotations

import json
import os
import stat
import sys
from dataclasses import replace
from pathlib import Path

import pytest

from lidarbench.core.scratch import (
    MARKER_NAME,
    SCRATCH_PREFIX,
    ScratchError,
    cleanup_orphan,
    cleanup_scratch,
    create_scratch,
    find_owned_scratch,
    is_within,
    paths_overlap,
    release_ownership,
    validate_scratch_root,
)


@pytest.fixture
def root(tmp_path):
    path = tmp_path / "scratch root"
    path.mkdir()
    return path


def _symlink(link: Path, target: Path) -> None:
    try:
        link.symlink_to(target, target_is_directory=target.is_dir())
    except OSError:
        pytest.skip("symlinks not permitted")


class TestRootValidation:
    def test_valid_root_is_resolved(self, root):
        assert validate_scratch_root(root) == Path(os.path.realpath(root))

    def test_refuses_filesystem_root_home_and_cwd(self, root, monkeypatch):
        anchor = Path(Path.cwd().anchor)
        with pytest.raises(ScratchError, match="root"):
            validate_scratch_root(anchor)
        with pytest.raises(ScratchError, match="home"):
            validate_scratch_root(root, home=root)
        monkeypatch.chdir(root)
        with pytest.raises(ScratchError, match="working"):
            validate_scratch_root(root)

    def test_refuses_missing_root(self, tmp_path):
        with pytest.raises(ScratchError, match="existing directory"):
            validate_scratch_root(tmp_path / "missing")

    def test_refuses_overlap_in_both_directions(self, tmp_path, root):
        inside = root / "inputs"
        inside.mkdir()
        with pytest.raises(ScratchError, match="overlaps"):
            validate_scratch_root(root, [inside])
        with pytest.raises(ScratchError, match="overlaps"):
            validate_scratch_root(root, [tmp_path])

    def test_overlap_through_symlink(self, tmp_path, root):
        link = tmp_path / "alias"
        _symlink(link, root)
        assert is_within(link, root) and paths_overlap(link, root)
        with pytest.raises(ScratchError, match="overlaps"):
            validate_scratch_root(link, [root])


class TestLifecycle:
    def test_create_and_cleanup(self, root):
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        assert scratch.path.parent == Path(os.path.realpath(root))
        assert scratch.path.name.startswith(SCRATCH_PREFIX)
        assert scratch.output_dir.is_dir() and scratch.work_dir.is_dir()
        marker = json.loads((scratch.path / MARKER_NAME).read_text(encoding="utf-8"))
        assert marker["token"] == scratch.token and marker["attempt_id"] == "a"
        if sys.platform != "win32":
            assert stat.S_IMODE(os.stat(scratch.path).st_mode) == 0o700
        (scratch.output_dir / "sub").mkdir()
        readonly = scratch.output_dir / "sub" / "ro.txt"
        readonly.write_text("x")
        os.chmod(readonly, stat.S_IREAD)
        result = cleanup_scratch(scratch)
        assert result.complete, result.reason
        assert not scratch.path.exists()

    def test_refuses_without_in_memory_ownership(self, root):
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        forged = replace(scratch, token="forged")
        result = cleanup_scratch(forged)
        assert not result.complete and "not owned" in result.reason
        assert scratch.path.exists()
        release_ownership(scratch)
        assert not cleanup_scratch(scratch).complete
        assert scratch.path.exists()

    def test_refuses_replaced_directory(self, root):
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        moved = root / "moved"
        scratch.path.rename(moved)
        scratch.path.mkdir()
        (moved / MARKER_NAME).rename(scratch.path / MARKER_NAME)
        result = cleanup_scratch(scratch)
        assert not result.complete and "identity" in result.reason
        assert scratch.path.exists()

    def test_refuses_link_inside_tree(self, tmp_path, root):
        precious = tmp_path / "precious"
        precious.mkdir()
        (precious / "keep.txt").write_text("keep")
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        _symlink(scratch.output_dir / "escape", precious)
        result = cleanup_scratch(scratch)
        assert not result.complete and "link" in result.reason
        assert (precious / "keep.txt").read_text() == "keep"

    def test_refuses_when_root_became_protected(self, root):
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        scratch = replace(scratch, protected=(root,))
        assert "overlaps" in (cleanup_scratch(scratch).reason or "")
        assert scratch.path.exists()


class TestOrphans:
    def test_find_and_clean_orphans_by_marker(self, root):
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        other = create_scratch(root, campaign_id="other", attempt_id="b")
        release_ownership(scratch)
        (root / f"{SCRATCH_PREFIX}fake").mkdir()
        found = find_owned_scratch(root, "c")
        assert [f["attempt_id"] for f in found] == ["a"]
        assert not cleanup_orphan(other.path, root, "c").complete
        assert cleanup_orphan(scratch.path, root, "c").complete
        assert not scratch.path.exists() and other.path.exists()
        assert not cleanup_orphan(root / f"{SCRATCH_PREFIX}fake", root, "c").complete
        cleanup_scratch(other)

    def test_orphan_with_tampered_marker_is_kept(self, root):
        scratch = create_scratch(root, campaign_id="c", attempt_id="a")
        release_ownership(scratch)
        marker = scratch.path / MARKER_NAME
        data = json.loads(marker.read_text(encoding="utf-8"))
        data["inode"] = -1
        os.chmod(marker, 0o644)
        marker.write_text(json.dumps(data), encoding="utf-8")
        assert not cleanup_orphan(scratch.path, root, "c").complete
        assert scratch.path.exists()
