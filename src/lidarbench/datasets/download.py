"""Optional download of original source data with established public terms.

Only ``source`` recipes may declare ``[download]``, and only when their
license permits redistribution (enforced when the recipe is parsed). A
download must be acknowledged with its exact size (``--accept-bytes``), uses
HTTPS only, resumes an interrupted transfer into a marked partial file, and
publishes the file under its final name only after its size and checksum
match the recipe. Nothing is ever downloaded implicitly.
"""

from __future__ import annotations

import os
import urllib.request
from collections.abc import Callable
from pathlib import Path
from typing import IO, Any

from lidarbench.datasets.inventory import PARTIAL_MARKER, hash_file
from lidarbench.datasets.recipes import Recipe
from lidarbench.datasets.roots import DataRoots

CHUNK = 1 << 20

Opener = Callable[[urllib.request.Request], Any]


class DownloadError(RuntimeError):
    """The download is not permitted, not acknowledged or failed verification."""


def partial_path(final: Path) -> Path:
    """Partial file of an interrupted download (blocks registration until resolved)."""
    return final.with_name(f".{final.name}{PARTIAL_MARKER}download")


def _open(opener: Opener, url: str, offset: int) -> tuple[Any, bool]:
    request = urllib.request.Request(url, headers={"User-Agent": "lidarbench"})
    if offset:
        request.add_header("Range", f"bytes={offset}-")
    response = opener(request)
    final_url = getattr(response, "geturl", lambda: url)()
    if not str(final_url).startswith("https://"):
        response.close()
        raise DownloadError(f"refusing non-HTTPS redirect to {final_url}")
    status = getattr(response, "status", 200)
    if status not in (200, 206):
        response.close()
        raise DownloadError(f"HTTP status {status}")
    return response, status == 206


def download_source(
    recipe: Recipe,
    roots: DataRoots,
    *,
    accept_bytes: int | None,
    opener: Opener = urllib.request.urlopen,
    progress: Callable[[int, int], None] | None = None,
) -> Path:
    """Download the recipe's original file into its dataset directory.

    Returns:
        The published file.

    Raises:
        DownloadError: Not permitted, size not acknowledged, file exists, or the
            transfer does not match the declared size/checksum (partial kept).
    """
    spec = recipe.download
    if spec is None:
        raise DownloadError(f"recipe {recipe.id} declares no download")
    if accept_bytes != spec.bytes:
        raise DownloadError(
            f"{spec.url} is {spec.bytes} bytes ({spec.bytes / 1e9:.2f} GB); acknowledge the "
            f"size with --accept-bytes {spec.bytes} to download it"
        )
    folder = roots.resolve(recipe.root, recipe.datasets[0].path)
    folder.mkdir(parents=True, exist_ok=True)
    final = folder / spec.file
    if final.exists():
        raise DownloadError(f"{final} already exists; verify or register it instead")
    partial = partial_path(final)
    offset = partial.stat().st_size if partial.is_file() else 0
    if offset > spec.bytes:
        raise DownloadError(f"{partial} is larger than the declared size; remove it manually")
    if offset < spec.bytes:
        response, resumed = _open(opener, spec.url, offset)
        mode = "ab" if resumed else "wb"
        done = offset if resumed else 0
        try:
            with open(partial, mode) as handle:
                done = _copy(response, handle, done, spec.bytes, progress)
                handle.flush()
                os.fsync(handle.fileno())
        finally:
            response.close()
    sha, md5, size = hash_file(partial, md5=spec.md5 is not None)
    if size != spec.bytes:
        raise DownloadError(f"size {size} != declared {spec.bytes}; partial kept: {partial}")
    if (spec.sha256 is not None and sha != spec.sha256) or (
        spec.md5 is not None and md5 != spec.md5
    ):
        raise DownloadError(f"checksum mismatch ({spec.checksum_origin}); partial kept: {partial}")
    if final.exists():
        raise DownloadError(f"{final} appeared during the download; partial kept: {partial}")
    os.rename(partial, final)
    return final


def _copy(
    response: Any,
    handle: IO[bytes],
    done: int,
    total: int,
    progress: Callable[[int, int], None] | None,
) -> int:
    while chunk := response.read(CHUNK):
        handle.write(chunk)
        done += len(chunk)
        if done > total:
            raise DownloadError("server sent more bytes than declared")
        if progress is not None:
            progress(done, total)
    return done
