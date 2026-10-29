"""Run-owned scratch directories: guarded creation and cleanup.

Ports the legacy R guards (``benchmark_validate_scratch_root``,
``benchmark_scratch_create``, ``benchmark_scratch_cleanup``) and extends them:

- **Root validation.** A scratch root must be an existing writable directory,
  not a filesystem/drive/UNC-share root, the home directory or the working
  directory, and must not overlap any protected path (inputs, workspace,
  repository). Overlap is decided on resolved paths with ``os.path.samefile``
  on every ancestor, so case-insensitive filesystems (Windows, default macOS)
  and differently spelled paths are handled.
- **Creation.** ``tempfile.mkdtemp`` (mode ``0700`` on POSIX; on Windows the
  directory inherits the ACL of the root, which should be private to the
  benchmark user). An ownership marker with a random token is written with
  exclusive create, and the device/inode pair is recorded.
- **Cleanup** requires all of: the in-memory ownership token, the marker token,
  the recorded device/inode, the validated parent root and name prefix, and a
  walk without following links that finds no symlink, junction or mount point.
  Read-only files inside the tree are made writable before deletion. Cleanup
  never raises: an incomplete or refused cleanup is returned as a result so the
  campaign can log a warning and continue.
- **Orphans** left by a crash are found by :func:`find_owned_scratch` through
  their markers; they are reported, never deleted automatically.
"""

from __future__ import annotations

import os
import secrets
import shutil
import socket
import stat
import sys
import tempfile
from collections.abc import Callable, Iterable
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from lidarbench.core.clock import iso_utc, now
from lidarbench.core.store import publish_json, read_json

SCRATCH_PREFIX = "lidarbench-run-"
MARKER_NAME = ".lidarbench-scratch.json"
MARKER_SCHEMA = "lidarbench.scratch-marker/1"
OUTPUT_SUBDIR = "out"
WORK_SUBDIR = "work"

_OWNERS: dict[str, str] = {}


class ScratchError(RuntimeError):
    """A scratch root or directory failed a safety check."""


def _real(path: str | os.PathLike[str]) -> Path:
    return Path(os.path.realpath(path))


def _same(a: Path, b: Path) -> bool:
    try:
        return os.path.samefile(a, b)
    except OSError:
        return os.path.normcase(os.fspath(a)) == os.path.normcase(os.fspath(b))


def is_within(path: str | os.PathLike[str], parent: str | os.PathLike[str]) -> bool:
    """Whether ``path`` equals or is below ``parent`` (resolved, filesystem-aware)."""
    real, container = _real(path), _real(parent)
    return any(_same(candidate, container) for candidate in (real, *real.parents))


def paths_overlap(a: str | os.PathLike[str], b: str | os.PathLike[str]) -> bool:
    """Whether either path contains the other."""
    return is_within(a, b) or is_within(b, a)


def validate_scratch_root(
    root: str | os.PathLike[str],
    protected: Iterable[str | os.PathLike[str]] = (),
    *,
    cwd: Path | None = None,
    home: Path | None = None,
) -> Path:
    """Validate a scratch root and return its resolved path.

    Raises:
        ScratchError: The root is unsafe or unusable.
    """
    given = Path(root)
    if not given.is_dir():
        raise ScratchError(f"scratch root must be an existing directory: {given}")
    real = _real(given)
    if real.parent == real or real == Path(real.anchor):
        raise ScratchError(f"scratch root cannot be a filesystem, drive or share root: {real}")
    for label, other in (("home", home or Path.home()), ("working", cwd or Path.cwd())):
        if other.exists() and _same(real, _real(other)):
            raise ScratchError(f"scratch root cannot be the {label} directory: {real}")
    if not os.access(real, os.W_OK | os.X_OK):
        raise ScratchError(f"scratch root is not writable: {real}")
    for path in protected:
        if Path(path).exists() and paths_overlap(real, path):
            raise ScratchError(f"scratch root {real} overlaps protected path {_real(path)}")
    return real


@dataclass(frozen=True)
class ScratchDir:
    """A created, run-owned scratch directory."""

    path: Path
    root: Path
    token: str
    device: int
    inode: int
    campaign_id: str
    attempt_id: str
    protected: tuple[Path, ...] = ()

    @property
    def output_dir(self) -> Path:
        """Directory for tool outputs."""
        return self.path / OUTPUT_SUBDIR

    @property
    def work_dir(self) -> Path:
        """Working directory and adapter config files."""
        return self.path / WORK_SUBDIR

    def to_record(self) -> dict[str, Any]:
        """JSON-ready description (no secret token)."""
        return {
            "path": os.fspath(self.path),
            "root": os.fspath(self.root),
            "device": self.device,
            "inode": self.inode,
        }


def create_scratch(
    root: str | os.PathLike[str],
    *,
    campaign_id: str,
    attempt_id: str,
    protected: Iterable[str | os.PathLike[str]] = (),
) -> ScratchDir:
    """Create a run-owned scratch directory with ``out/`` and ``work/`` inside."""
    protected_paths = tuple(Path(p) for p in protected)
    real_root = validate_scratch_root(root, protected_paths)
    path = Path(tempfile.mkdtemp(prefix=SCRATCH_PREFIX, dir=real_root))
    st = os.lstat(path)
    token = secrets.token_hex(16)
    publish_json(
        path / MARKER_NAME,
        {
            "schema": MARKER_SCHEMA,
            "campaign_id": campaign_id,
            "attempt_id": attempt_id,
            "token": token,
            "created_at": iso_utc(now().wall_ns),
            "host": socket.gethostname(),
            "pid": os.getpid(),
            "device": st.st_dev,
            "inode": st.st_ino,
        },
    )
    (path / OUTPUT_SUBDIR).mkdir()
    (path / WORK_SUBDIR).mkdir()
    _OWNERS[os.fspath(path)] = token
    return ScratchDir(
        path, real_root, token, st.st_dev, st.st_ino, campaign_id, attempt_id, protected_paths
    )


@dataclass(frozen=True)
class CleanupResult:
    """Outcome of :func:`cleanup_scratch`."""

    path: Path
    complete: bool
    reason: str | None = None

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {"path": os.fspath(self.path), "complete": self.complete, "reason": self.reason}


def _is_link(path: Path) -> bool:
    if path.is_symlink():
        return True
    isjunction = getattr(os.path, "isjunction", None)
    if isjunction is not None and isjunction(path):
        return True
    attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    return bool(attributes & getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0))


def _tree_problem(path: Path) -> str | None:
    errors: list[OSError] = []
    for dirpath, dirnames, filenames in os.walk(path, followlinks=False, onerror=errors.append):
        for name in (*dirnames, *filenames):
            entry = Path(dirpath) / name
            if _is_link(entry):
                return f"link or junction inside scratch: {entry}"
            if entry.is_dir() and os.path.ismount(entry):
                return f"mount point inside scratch: {entry}"
    if errors:
        return f"cannot inspect scratch tree: {errors[0]}"
    return None


def _ownership_problem(scratch: ScratchDir) -> str | None:
    path = scratch.path
    if _OWNERS.get(os.fspath(path)) != scratch.token:
        return "not owned by this run (no in-memory ownership record)"
    try:
        root = validate_scratch_root(scratch.root, scratch.protected)
    except ScratchError as exc:
        return f"scratch root no longer valid: {exc}"
    if not path.name.startswith(SCRATCH_PREFIX) or not _same(path.parent, root):
        return "path is not a run directory directly inside the validated root"
    try:
        st = os.lstat(path)
    except OSError as exc:
        return f"cannot stat scratch directory: {exc}"
    if stat.S_ISLNK(st.st_mode) or _is_link(path) or not stat.S_ISDIR(st.st_mode):
        return "scratch path is no longer a plain directory"
    if (st.st_dev, st.st_ino) != (scratch.device, scratch.inode):
        return "scratch directory identity (device/inode) changed"
    try:
        marker = read_json(path / MARKER_NAME)
    except (OSError, ValueError) as exc:
        return f"ownership marker unreadable: {exc}"
    if not isinstance(marker, dict) or marker.get("token") != scratch.token:
        return "ownership marker does not match this run"
    return _tree_problem(path)


def _rmtree(path: Path) -> list[str]:
    failures: list[str] = []

    def retry(function: Callable[..., Any], target: str, exc: BaseException) -> None:
        if isinstance(exc, PermissionError) and is_within(target, path):
            try:
                os.chmod(target, stat.S_IWRITE | stat.S_IREAD | stat.S_IEXEC)
                function(target)
                return
            except OSError as again:
                exc = again
        failures.append(f"{target}: {exc}")

    if sys.version_info >= (3, 12):
        shutil.rmtree(path, onexc=retry)
    else:
        shutil.rmtree(path, onerror=lambda f, t, info: retry(f, t, info[1]))
    return failures


def cleanup_scratch(scratch: ScratchDir) -> CleanupResult:
    """Delete a run-owned scratch directory after all ownership checks pass.

    Never raises; refusals and partial deletions are returned with a reason.
    """
    path = scratch.path
    if not path.exists() and not path.is_symlink():
        _OWNERS.pop(os.fspath(path), None)
        return CleanupResult(path, True, "already absent")
    problem = _ownership_problem(scratch)
    if problem is not None:
        return CleanupResult(path, False, f"cleanup refused: {problem}")
    failures = _rmtree(path)
    if path.exists():
        detail = "; ".join(failures[:3]) or "directory still exists"
        return CleanupResult(path, False, f"cleanup incomplete: {detail}")
    _OWNERS.pop(os.fspath(path), None)
    return CleanupResult(path, True)


def release_ownership(scratch: ScratchDir) -> None:
    """Forget ownership without deleting (retained scratch)."""
    _OWNERS.pop(os.fspath(scratch.path), None)


def find_owned_scratch(
    root: str | os.PathLike[str], campaign_id: str | None = None
) -> list[dict[str, Any]]:
    """List marker-proven run directories in ``root`` (for orphan reports)."""
    found: list[dict[str, Any]] = []
    root_path = Path(root)
    if not root_path.is_dir():
        return found
    for entry in sorted(root_path.iterdir()):
        if not entry.name.startswith(SCRATCH_PREFIX) or entry.is_symlink() or not entry.is_dir():
            continue
        try:
            marker = read_json(entry / MARKER_NAME)
        except (OSError, ValueError):
            continue
        if not isinstance(marker, dict) or marker.get("schema") != MARKER_SCHEMA:
            continue
        if campaign_id is not None and marker.get("campaign_id") != campaign_id:
            continue
        found.append(
            {
                "path": os.fspath(entry),
                "campaign_id": marker.get("campaign_id"),
                "attempt_id": marker.get("attempt_id"),
                "created_at": marker.get("created_at"),
                "owned_by_this_process": os.fspath(entry) in _OWNERS,
            }
        )
    return found


def cleanup_orphan(
    path: str | os.PathLike[str],
    root: str | os.PathLike[str],
    campaign_id: str,
    protected: Iterable[str | os.PathLike[str]] = (),
) -> CleanupResult:
    """Delete an orphaned run directory whose ownership is proven by its marker.

    Used after a crash, when the in-memory ownership record is gone. Requires a
    valid root, a direct child with the run prefix, no link at the path, a
    marker for ``campaign_id`` whose recorded device/inode match the directory,
    and a link-free, mount-free tree. Never raises.
    """
    target = Path(path)
    try:
        real_root = validate_scratch_root(root, protected)
    except ScratchError as exc:
        return CleanupResult(target, False, f"cleanup refused: {exc}")
    if not target.name.startswith(SCRATCH_PREFIX) or not _same(target.parent, real_root):
        return CleanupResult(target, False, "cleanup refused: not a run directory of the root")
    try:
        st = os.lstat(target)
        marker = read_json(target / MARKER_NAME)
    except (OSError, ValueError) as exc:
        return CleanupResult(target, False, f"cleanup refused: {exc}")
    if stat.S_ISLNK(st.st_mode) or _is_link(target) or not stat.S_ISDIR(st.st_mode):
        return CleanupResult(target, False, "cleanup refused: not a plain directory")
    if (
        not isinstance(marker, dict)
        or marker.get("schema") != MARKER_SCHEMA
        or marker.get("campaign_id") != campaign_id
        or (marker.get("device"), marker.get("inode")) != (st.st_dev, st.st_ino)
    ):
        return CleanupResult(target, False, "cleanup refused: marker does not prove ownership")
    problem = _tree_problem(target)
    if problem is not None:
        return CleanupResult(target, False, f"cleanup refused: {problem}")
    failures = _rmtree(target)
    if target.exists():
        detail = "; ".join(failures[:3]) or "directory still exists"
        return CleanupResult(target, False, f"cleanup incomplete: {detail}")
    return CleanupResult(target, True)
