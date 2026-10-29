"""Write-once records, append-only logs and sortable identifiers.

Publishing protocol for a write-once file ``name``:

1. write ``name.pending-<random>`` with exclusive create, ``fsync`` it;
2. publish it under ``name`` with ``os.link`` (fails if ``name`` exists, so an
   existing record is never replaced). On filesystems without hard links the
   pending file is renamed after an existence check; the campaign lock makes
   that check race-free in practice;
3. remove the pending name and ``fsync`` the directory (POSIX).

A crash leaves at most a ``*.pending-*`` file, never a truncated record.
"""

from __future__ import annotations

import contextlib
import json
import os
import secrets
import stat
import sys
import threading
import time
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import pretty_bytes, sha256_bytes

PENDING_MARKER = ".pending-"
_CROCKFORD = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"


class RecordExistsError(FileExistsError):
    """A write-once record already exists and was not modified."""


def fsync_directory(path: Path) -> None:
    """Flush directory entries to disk (POSIX; no-op on Windows)."""
    if sys.platform == "win32":
        return
    fd = os.open(path, os.O_RDONLY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def publish_bytes(path: Path, data: bytes) -> str:
    """Publish ``data`` at ``path`` exactly once.

    Returns:
        Hex SHA-256 of ``data``.

    Raises:
        RecordExistsError: ``path`` already exists (it is left untouched).
    """
    path = Path(path)
    if path.exists() or path.is_symlink():
        raise RecordExistsError(f"refusing to overwrite write-once record: {path}")
    pending = path.with_name(f"{path.name}{PENDING_MARKER}{secrets.token_hex(6)}")
    with open(pending, "xb") as handle:
        handle.write(data)
        handle.flush()
        os.fsync(handle.fileno())
    try:
        try:
            os.link(pending, path)
        except FileExistsError:
            raise RecordExistsError(f"refusing to overwrite write-once record: {path}") from None
        except OSError:
            if path.exists():
                raise RecordExistsError(
                    f"refusing to overwrite write-once record: {path}"
                ) from None
            os.replace(pending, path)
    finally:
        with contextlib.suppress(FileNotFoundError):
            os.unlink(pending)
    fsync_directory(path.parent)
    return sha256_bytes(data)


def publish_json(path: Path, record: Any) -> str:
    """Publish a JSON record once (deterministic pretty form). Returns its SHA-256."""
    return publish_bytes(path, pretty_bytes(record))


def read_json(path: Path) -> Any:
    """Read a UTF-8 JSON file."""
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def make_read_only(path: Path) -> None:
    """Remove write permission from a published file (defence in depth only)."""
    mode = os.stat(path).st_mode
    os.chmod(path, mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH))


@dataclass(frozen=True)
class JsonlRead:
    """Lines of an append-only JSON Lines file."""

    records: tuple[dict[str, Any], ...]
    torn_tail: bool
    """The last line is incomplete (a crash during append); it is ignored."""
    damaged_lines: tuple[int, ...] = ()
    """1-based numbers of complete lines that are not JSON objects (for example an
    earlier torn line that a later append terminated); skipped, never repaired."""


def read_jsonl(path: Path) -> JsonlRead:
    """Read a JSON Lines file; damaged lines are skipped and reported, not fatal."""
    if not path.exists():
        return JsonlRead((), False)
    data = path.read_bytes()
    lines = data.split(b"\n")
    tail = lines.pop()
    records = []
    damaged = []
    for number, line in enumerate(lines, start=1):
        if not line.strip():
            continue
        try:
            item = json.loads(line.decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError):
            item = None
        if not isinstance(item, dict):
            damaged.append(number)
            continue
        records.append(item)
    return JsonlRead(tuple(records), bool(tail.strip()), tuple(damaged))


class AppendOnlyLog:
    """Append-only JSON Lines log; each line is flushed and fsynced.

    An incomplete last line left by a crash is preserved and terminated before
    the next append, so earlier bytes are never rewritten.
    """

    def __init__(self, path: Path) -> None:
        self.path = Path(path)
        existing = read_jsonl(self.path)
        self._lock = threading.Lock()
        self._torn_tail = existing.torn_tail
        self._next_seq = 1 + max((int(r.get("seq", 0)) for r in existing.records), default=0)

    @property
    def next_seq(self) -> int:
        """Sequence number the next appended record will receive."""
        return self._next_seq

    def append(self, record: dict[str, Any]) -> dict[str, Any]:
        """Append ``record`` with the next ``seq`` number and return the stored line."""
        with self._lock:
            line = {"seq": self._next_seq, **record}
            data = json.dumps(line, sort_keys=True, ensure_ascii=False, allow_nan=False)
            payload = data.encode("utf-8") + b"\n"
            if self._torn_tail:
                payload = b"\n" + payload
            with open(self.path, "ab") as handle:
                handle.write(payload)
                handle.flush()
                os.fsync(handle.fileno())
            self._torn_tail = False
            self._next_seq += 1
            return line


_id_lock = threading.Lock()
_last_id: tuple[int, int] = (0, 0)


def _b32(value: int, length: int) -> str:
    chars = []
    for _ in range(length):
        value, rem = divmod(value, 32)
        chars.append(_CROCKFORD[rem])
    return "".join(reversed(chars))


def new_sortable_id() -> str:
    """26-character ID: 48-bit millisecond time + 80 random bits (Crockford base32).

    IDs sort by creation time; within one millisecond in one process they
    increase monotonically.
    """
    global _last_id
    with _id_lock:
        ms = time.time_ns() // 1_000_000
        rand = secrets.randbits(80)
        last_ms, last_rand = _last_id
        if ms <= last_ms:
            ms, rand = last_ms, (last_rand + 1) % (1 << 80)
        _last_id = (ms, rand)
    return _b32(ms, 10) + _b32(rand, 16)


def iter_files(root: Path) -> Iterator[Path]:
    """Regular files below ``root`` in sorted order (links are not followed)."""
    for dirpath, dirnames, filenames in os.walk(root, followlinks=False):
        dirnames.sort()
        for name in sorted(filenames):
            yield Path(dirpath) / name
