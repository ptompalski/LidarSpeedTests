"""Canonical JSON serialization and SHA-256 digests.

Canonical form: UTF-8, keys sorted, no insignificant whitespace, no NaN or
infinity, tuples as arrays, a trailing newline. Floats use Python's shortest
round-trip ``repr``, which is stable across Python 3 versions and platforms.
Values without one unambiguous JSON form (paths, sets, bytes, arbitrary
objects) are rejected, so a digest never depends on an implicit conversion.
"""

from __future__ import annotations

import hashlib
import json
import math
import os
from collections.abc import Mapping
from typing import Any

HASH_CHUNK_BYTES = 1 << 20


class CanonicalError(TypeError):
    """A value cannot be represented canonically."""


def _check(value: Any, where: str) -> Any:
    if value is None or isinstance(value, bool | int):
        return value
    if isinstance(value, str):
        return str(value)
    if isinstance(value, float):
        if not math.isfinite(value):
            raise CanonicalError(f"{where}: non-finite float {value!r}")
        return value
    if isinstance(value, Mapping):
        out = {}
        for key, item in value.items():
            if not isinstance(key, str):
                raise CanonicalError(f"{where}: non-string key {key!r}")
            out[str(key)] = _check(item, f"{where}.{key}")
        return out
    if isinstance(value, list | tuple):
        return [_check(item, f"{where}[{i}]") for i, item in enumerate(value)]
    raise CanonicalError(f"{where}: {type(value).__name__} has no canonical JSON form")


def canonical_bytes(value: Any) -> bytes:
    """Serialize ``value`` canonically (UTF-8, sorted keys, trailing newline)."""
    text = json.dumps(
        _check(value, "$"),
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    )
    return (text + "\n").encode("utf-8")


def pretty_bytes(value: Any) -> bytes:
    """Readable, deterministic JSON (sorted keys, 2-space indent, trailing newline)."""
    text = json.dumps(_check(value, "$"), sort_keys=True, indent=2, ensure_ascii=False)
    return (text + "\n").encode("utf-8")


def sha256_bytes(data: bytes) -> str:
    """Hex SHA-256 of ``data``."""
    return hashlib.sha256(data).hexdigest()


def digest(value: Any) -> str:
    """Hex SHA-256 of the canonical serialization of ``value``."""
    return sha256_bytes(canonical_bytes(value))


def sha256_file(path: str | os.PathLike[str]) -> str:
    """Hex SHA-256 of a file's contents, read in 1 MiB chunks."""
    h = hashlib.sha256()
    with open(path, "rb") as handle:
        while chunk := handle.read(HASH_CHUNK_BYTES):
            h.update(chunk)
    return h.hexdigest()
