"""PDAL adapter: native ``pdal pipeline`` JSON pipelines under Python orchestration.

PDAL is an independent engine, not a port of the legacy R calls. Each workload
is expressed with PDAL's own stages, written as one JSON pipeline per input
tile (``ConfigFile`` in the attempt's work directory)::

    readers.las (tile) [+ readers.las (each neighbour) -> filters.merge]
        -> writers.gdal(binmode=true, output_type=mean|max, resolution=r,
                        origin_x, origin_y, width, height, nodata=-9999,
                        data_type=double, gdaldriver=GTiff)

The benchmarked process is :mod:`~lidarbench.execution.adapters.pdal_worker`,
which runs ``pdal pipeline <file> --metadata <file> [--stream|--nostream]``
once per tile (at most ``threads`` at a time) and records exit codes and launch
instants. The generic engine times, monitors and terminates it like any other
command.

Semantics (from the PDAL source, ``io/GDALWriter.cpp``,
``io/private/GDALGrid.cpp``, ``pdal/private/Raster.hpp``; unchanged 2.5.0-2.10):

* ``binmode=true`` (PDAL >= 2.5.0) bins each point into exactly one cell. Without
  it, ``writers.gdal`` adds every point to all cells whose centre lies within
  ``radius`` (default ``resolution * sqrt(2)``): a neighbourhood statistic, not a
  cell statistic. Only binmode is offered.
* Cell of a point: ``floor((X - origin_x) / r)``, ``floor((Y - origin_y) / r)``
  in binary64, rows counted upwards from ``origin_y``. On exact edges that is
  rightward X and **upward** Y (contract: downward Y); a point on the grid's
  north edge lies outside the tile's raster. Contract convention
  ``pdal_binmode_floor`` mirrors this arithmetic.
* The grid is fixed with ``origin_x/origin_y/width/height`` (the header bounds
  snapped outward to the zero-origin grid); without it PDAL sizes the grid from
  the points with ``floor(extent / r) + 1`` cells.
* ``mean`` is a running (Welford) mean in double, ``max`` the maximum. Empty
  cells are NaN internally and written as ``nodata`` (PDAL's double default is
  -9999; it is set explicitly). Band descriptions are PDAL's ``mean``/``max``.
* The 20 m buffer is honoured by reading every input whose header bounds
  intersect the tile bounds grown by the buffer (the contract population).
  Points outside the fixed grid never contribute in binmode.

Discovery (never installs anything): the ``executable`` option, else
``$LIDARBENCH_PDAL``, else ``pdal`` on ``PATH`` (``pdal.exe`` through
``PATHEXT`` on Windows). ``pdal --version`` gives the version;
``pdal --drivers --showjson`` and ``pdal --options writers.gdal --showjson``
confirm the stages and options the pipelines use.
"""

from __future__ import annotations

import json
import math
import os
import re
import subprocess
import sys
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path
from types import MappingProxyType
from typing import TYPE_CHECKING, Any

from lidarbench.core.canonical import sha256_bytes
from lidarbench.core.platform import PlatformInfo
from lidarbench.datasets.lasheader import LasHeaderError, read_las_header
from lidarbench.execution.adapters import pdal_worker as worker
from lidarbench.execution.adapters.base import (
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ConfigFile,
    ImplementationStatus,
    Interpretation,
    Invocation,
    MetadataValue,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    ToolTiming,
    UnavailableReason,
    UnsupportedCombinationError,
    WorkloadSupport,
    thread_cap_env,
)
from lidarbench.execution.adapters.launchers import python_module_argv, resolve_executable

if TYPE_CHECKING:
    from lidarbench.execution.process import ProcessResult

EXECUTABLE_ENV = "LIDARBENCH_PDAL"
"""Environment variable naming the ``pdal`` executable when no option is given."""

SUPPORTED_SYSTEMS = ("Linux", "Windows", "Darwin")
MINIMUM_VERSION = (2, 5, 0)
"""First release with ``writers.gdal`` ``binmode`` (PDAL commit e243a67, #3840)."""
REQUIRED_STAGES = ("readers.las", "filters.merge", "writers.gdal")
TIN_STAGES = (
    "filters.crop",
    "filters.range",
    "filters.delaunay",
    "filters.faceraster",
    "writers.raster",
)
"""Extra stages of TIN workloads; their absence makes only those workloads unrunnable."""
READER_THREADS = 1
"""``readers.las`` ``threads`` per pipeline; concurrency comes from concurrent pipelines."""
REQUIRED_GDAL_OPTIONS = (
    "binmode",
    "data_type",
    "gdaldriver",
    "height",
    "nodata",
    "origin_x",
    "origin_y",
    "output_type",
    "override_srs",
    "resolution",
    "width",
)
EXECUTION_MODES = tuple(worker.MODE_FLAGS)
NODATA = -9999.0
DATA_TYPE = "double"
GDAL_DRIVER = "GTiff"
SUPPORTED_PARAMETERS = ("epsg",)
LAS_SUFFIXES = (".las", ".laz")
WORKER_MODULE = "lidarbench.execution.adapters.pdal_worker"
JOB_FILE = "pdal-job.json"
RUN_FILE = "pdal-run.json"
PDAL_THREAD_VARIABLES = {"GDAL_NUM_THREADS": "1"}
"""Each ``pdal pipeline`` is single-threaded; parallelism is concurrent files."""

_VERSION_RE = re.compile(r"^\s*pdal\s+([0-9]+)\.([0-9]+)\.([0-9]+)(\S*)(?:\s+(.*))?$")
_EXPECTED_RE = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
_ERROR_RE = re.compile(r"(?i)(^\s*PDAL:|\berror\b|\bexception\b|\bfatal\b)")
_WARNING_RE = re.compile(r"(?i)\bwarning\b")
_LINE_LIMIT = 20


@dataclass(frozen=True)
class PdalWorkload:
    """A workload expressed as a ``writers.gdal`` binmode cell statistic or a TIN raster.

    ``method = "binmode"``: ``writers.gdal`` cell statistic ``output_type``.
    ``method = "tin"``: ``filters.range`` (``classes``) -> ``filters.delaunay`` ->
    ``filters.faceraster`` (sampled at cell centres) -> ``writers.raster``; the merged
    points are first cropped to the tile bounds grown by ``buffer``. Not streamable.
    """

    workload_id: str
    output_type: str
    resolution: float
    artifact_pattern: str = "{stem}.tif"
    buffer: float = 20.0
    method: str = "binmode"
    classes: tuple[int, ...] = ()
    max_triangle_edge_length: float | None = None

    def __post_init__(self) -> None:
        if self.method not in ("binmode", "tin"):
            raise ValueError(f"unknown PDAL workload method {self.method!r}")
        if self.method == "tin" and (self.output_type or not self.classes):
            raise ValueError("TIN workloads take classes and no output_type")

    @property
    def streamable(self) -> bool:
        """Whether every stage of the pipeline can stream (filters.delaunay cannot)."""
        return self.method == "binmode"

    @property
    def stages(self) -> tuple[str, ...]:
        """PDAL stages the pipeline needs."""
        if self.method == "tin":
            return ("readers.las", "filters.merge", *TIN_STAGES)
        return REQUIRED_STAGES


WORKLOADS: Mapping[str, PdalWorkload] = MappingProxyType(
    {
        "pixel_metrics_1a": PdalWorkload("pixel_metrics_1a", "mean", 20.0),
        "generate_DSM1": PdalWorkload("generate_DSM1", "max", 1.0),
        "generate_DEM": PdalWorkload(
            "generate_DEM", "", 1.0, method="tin", classes=(2, 9), max_triangle_edge_length=100.0
        ),
    }
)
"""Implemented workloads (workloads-v2 and later contracts, ``[implementations.pdal]``)."""

_NO_CONTRACT = "not implemented for PDAL: no workloads-v2 contract for this workload yet"
OTHER_WORKLOADS: tuple[WorkloadSupport, ...] = (
    *(
        WorkloadSupport(w, ImplementationStatus.UNSUPPORTED, reason=f"{_NO_CONTRACT} ({why})")
        for w, why in (
            (
                "normalization",
                "candidate filters.hag_delaunay/hag_nn not yet evaluated against the legacy "
                "TIN normalization",
            ),
            ("generate_DSM2", "the legacy subcircle/pit-free DSM has no evaluated PDAL pipeline"),
            (
                "pixel_metrics_2",
                "the nine-band metric set exceeds writers.gdal statistics; not evaluated",
            ),
            ("tree_detection", "no evaluated PDAL equivalent of the legacy local-maximum filter"),
        )
    ),
    *(
        WorkloadSupport(
            w,
            ImplementationStatus.UNSUPPORTED,
            reason="lasR-only sequential pipeline in the legacy harness",
        )
        for w in ("complex_task1", "complex_task2")
    ),
)

# -- discovery and version ------------------------------------------------------------


class ProbeError(RuntimeError):
    """A PDAL query failed (message safe to show)."""


def parse_version(text: str) -> tuple[tuple[int, int, int], str]:
    """``((major, minor, patch), full)`` from ``pdal --version`` output.

    ``full`` is the version string as printed after ``pdal`` (for example
    ``2.6.3 (git-version: Release)``).

    Raises:
        ProbeError: No or more than one ``pdal <x.y.z>`` line.
    """
    found = [m for m in map(_VERSION_RE.match, text.splitlines()) if m]
    if len(found) != 1:
        raise ProbeError(
            f"expected one 'pdal <x.y.z>' line in --version output, found {len(found)}"
        )
    m = found[0]
    numbers = (int(m.group(1)), int(m.group(2)), int(m.group(3)))
    full = f"{m.group(1)}.{m.group(2)}.{m.group(3)}{m.group(4)}"
    if m.group(5):
        full += f" {m.group(5).strip()}"
    return numbers, full


def parse_drivers(text: str) -> set[str]:
    """Stage names from ``pdal --drivers --showjson``."""
    try:
        data = json.loads(text)
    except ValueError as exc:
        raise ProbeError(f"--drivers --showjson is not JSON: {exc}") from exc
    if not isinstance(data, list):
        raise ProbeError("--drivers --showjson is not a list")
    return {d["name"] for d in data if isinstance(d, dict) and isinstance(d.get("name"), str)}


def parse_stage_options(text: str, stage: str) -> set[str]:
    """Option names from ``pdal --options <stage> --showjson``.

    PDAL prints ``["<stage>", [{"name": ...}, ...]]`` (some builds an object
    ``{"<stage>": [...]}``); both are accepted.
    """
    try:
        data = json.loads(text)
    except ValueError as exc:
        raise ProbeError(f"--options {stage} --showjson is not JSON: {exc}") from exc
    options: Any = None
    if isinstance(data, list) and len(data) == 2 and data[0] == stage:
        options = data[1]
    elif isinstance(data, dict) and stage in data:
        options = data[stage]
    if not isinstance(options, list):
        raise ProbeError(f"--options {stage} --showjson has no option list")
    return {o["name"] for o in options if isinstance(o, dict) and isinstance(o.get("name"), str)}


def run_query(executable: Path, args: Sequence[str], timeout_s: float) -> str:
    """Run ``pdal <args>`` (argv, no shell) and return stdout; raise on failure."""
    try:
        done = subprocess.run(
            [os.fspath(executable), *args],
            capture_output=True,
            timeout=timeout_s,
            check=False,
            shell=False,
            stdin=subprocess.DEVNULL,
        )
    except subprocess.TimeoutExpired as exc:
        raise ProbeError(f"'pdal {' '.join(args)}' timed out after {timeout_s:g} s") from exc
    except OSError as exc:
        raise ProbeError(f"cannot start {executable}: {exc.strerror or exc}") from exc
    if done.returncode != 0:
        raise ProbeError(f"'pdal {' '.join(args)}' exited with status {done.returncode}")
    return done.stdout.decode("utf-8", errors="replace")


def locate_executable(
    option: str, environ: Mapping[str, str] | None = None
) -> tuple[Path | None, str]:
    """``(path, source)``: option, then ``$LIDARBENCH_PDAL``, then ``pdal`` on PATH.

    Configured paths are returned (absolute) even if they do not exist.
    """
    env = os.environ if environ is None else environ
    if option:
        return Path(option).absolute(), "option"
    if env.get(EXECUTABLE_ENV):
        return Path(env[EXECUTABLE_ENV]).absolute(), "environment"
    return resolve_executable("pdal"), "PATH"


# -- grid and pipeline generation -----------------------------------------------------


@dataclass(frozen=True)
class TileGrid:
    """A tile's header bounds snapped outward to the zero-origin grid."""

    resolution: float
    col0: int
    row0: int
    width: int
    height: int

    @property
    def origin_x(self) -> float:
        """West edge (``col0 * r``, the same float the validator uses)."""
        return self.col0 * self.resolution

    @property
    def origin_y(self) -> float:
        """South edge (``row0 * r``)."""
        return self.row0 * self.resolution


def snapped_grid(bounds: tuple[float, float, float, float], resolution: float) -> TileGrid:
    """Header bounds ``(min_x, min_y, max_x, max_y)`` snapped outward (exact rationals).

    Raises:
        ValueError: Non-finite or zero-area bounds.
    """
    min_x, min_y, max_x, max_y = bounds
    if not all(math.isfinite(v) for v in bounds) or not (min_x < max_x and min_y < max_y):
        raise ValueError(f"unusable tile bounds {bounds}")
    r = Fraction(resolution)
    col0 = math.floor(Fraction(min_x) / r)
    col1 = math.ceil(Fraction(max_x) / r)
    row0 = math.floor(Fraction(min_y) / r)
    row1 = math.ceil(Fraction(max_y) / r)
    return TileGrid(resolution, col0, row0, col1 - col0, row1 - row0)


def _intersects(a: tuple[float, ...], b: tuple[float, ...]) -> bool:
    return a[0] <= b[2] and b[0] <= a[2] and a[1] <= b[3] and b[1] <= a[3]


def buffered_neighbours(
    bounds: Mapping[Path, tuple[float, float, float, float]], tile: Path, buffer: float
) -> list[Path]:
    """Inputs whose header bounds intersect ``tile``'s bounds grown by ``buffer`` (closed).

    The same rule the validator uses for the reference population; input order.
    """
    min_x, min_y, max_x, max_y = bounds[tile]
    grown = (min_x - buffer, min_y - buffer, max_x + buffer, max_y + buffer)
    return [p for p, b in bounds.items() if p != tile and _intersects(b, grown)]


def build_pipeline(
    tile: Path,
    neighbours: Sequence[Path],
    output: Path,
    grid: TileGrid,
    output_type: str,
    epsg: int | None = None,
    reader_threads: int | None = READER_THREADS,
) -> dict[str, Any]:
    """The PDAL pipeline document for one tile.

    ``reader_threads`` sets ``readers.las`` ``threads`` (its decompression pool,
    default 7 in PDAL) so a pipeline stays within its share of the thread budget;
    ``None`` omits the option (PDAL builds without it).
    """
    reader: dict[str, Any] = {} if reader_threads is None else {"threads": reader_threads}
    stages: list[dict[str, Any]] = [
        {"type": "readers.las", "filename": os.fspath(tile), "tag": "tile", **reader}
    ]
    for i, path in enumerate(neighbours, start=1):
        stages.append(
            {
                "type": "readers.las",
                "filename": os.fspath(path),
                "tag": f"neighbour_{i}",
                **reader,
            }
        )
    writer: dict[str, Any] = {
        "type": "writers.gdal",
        "filename": os.fspath(output),
        "binmode": True,
        "output_type": output_type,
        "resolution": grid.resolution,
        "origin_x": grid.origin_x,
        "origin_y": grid.origin_y,
        "width": grid.width,
        "height": grid.height,
        "nodata": NODATA,
        "data_type": DATA_TYPE,
        "gdaldriver": GDAL_DRIVER,
    }
    if epsg is not None:
        writer["override_srs"] = f"EPSG:{epsg}"
    if neighbours:
        stages.append(
            {
                "type": "filters.merge",
                "inputs": ["tile", *(f"neighbour_{i}" for i in range(1, len(neighbours) + 1))],
                "tag": "merged",
            }
        )
        writer["inputs"] = ["merged"]
    else:
        writer["inputs"] = ["tile"]
    stages.append(writer)
    return {"pipeline": stages}


def _range_limits(classes: Sequence[int]) -> str:
    return ",".join(f"Classification[{c}:{c}]" for c in classes)


def build_tin_pipeline(
    tile: Path,
    neighbours: Sequence[Path],
    output: Path,
    grid: TileGrid,
    tile_bounds: tuple[float, float, float, float],
    workload: PdalWorkload,
    epsg: int | None = None,
    reader_threads: int | None = READER_THREADS,
) -> dict[str, Any]:
    """The PDAL TIN pipeline document for one tile (see :class:`PdalWorkload`).

    Points are cropped to the tile's header bounds grown by the buffer, so the TIN
    uses the contract's 20 m neighbourhood rather than whole neighbouring tiles.
    """
    reader: dict[str, Any] = {} if reader_threads is None else {"threads": reader_threads}
    if epsg is not None:
        reader["override_srs"] = f"EPSG:{epsg}"
    paths = [tile, *neighbours]
    stages: list[dict[str, Any]] = [
        {
            "type": "readers.las",
            "filename": os.fspath(path),
            "tag": "tile" if i == 0 else f"neighbour_{i}",
            **reader,
        }
        for i, path in enumerate(paths)
    ]
    if neighbours:
        stages.append(
            {
                "type": "filters.merge",
                "inputs": ["tile", *(f"neighbour_{i}" for i in range(1, len(paths)))],
                "tag": "merged",
            }
        )
    min_x, min_y, max_x, max_y = tile_bounds
    b = workload.buffer
    raster: dict[str, Any] = {
        "type": "filters.faceraster",
        "resolution": grid.resolution,
        "origin_x": grid.origin_x,
        "origin_y": grid.origin_y,
        "width": grid.width,
        "height": grid.height,
        "nodata": NODATA,
    }
    if workload.max_triangle_edge_length is not None:
        raster["max_triangle_edge_length"] = workload.max_triangle_edge_length
    stages += [
        {
            "type": "filters.crop",
            "bounds": f"([{min_x - b!r}, {max_x + b!r}], [{min_y - b!r}, {max_y + b!r}])",
        },
        {"type": "filters.range", "limits": _range_limits(workload.classes)},
        {"type": "filters.delaunay"},
        raster,
        {
            "type": "writers.raster",
            "filename": os.fspath(output),
            "gdaldriver": GDAL_DRIVER,
            "data_type": DATA_TYPE,
            "nodata": NODATA,
        },
    ]
    return {"pipeline": stages}


def render_json(document: Mapping[str, Any]) -> str:
    """Stable UTF-8 JSON text (paths unescaped, floats round-trip)."""
    return json.dumps(document, indent=2, ensure_ascii=False, allow_nan=False) + "\n"


def _quote(lines: Sequence[str]) -> str:
    shown = [line.strip() for line in lines[:_LINE_LIMIT]]
    extra = len(lines) - _LINE_LIMIT
    return "; ".join(shown) + (f"; … ({extra} more)" if extra > 0 else "")


def _metadata_facts(path: Path) -> dict[str, Any]:
    """Compact facts from a ``pdal pipeline --metadata`` document."""
    try:
        raw = path.read_bytes()
    except OSError as exc:
        return {"present": False, "error": exc.strerror or str(exc)}
    facts: dict[str, Any] = {"present": True, "bytes": len(raw), "sha256": sha256_bytes(raw)}
    try:
        data = json.loads(raw.decode("utf-8"))
    except ValueError as exc:
        facts["error"] = f"not JSON: {exc}"
        return facts
    stages = data.get("stages") if isinstance(data, dict) else None
    if not isinstance(stages, dict):
        return facts
    readers = []
    for name, node in sorted(stages.items()):
        if not isinstance(node, dict):
            continue
        if name.startswith("readers.las"):
            readers.append(
                {
                    k: node.get(k)
                    for k in ("count", "minx", "maxx", "miny", "maxy", "minz", "maxz")
                    if isinstance(node.get(k), int | float | str)
                }
            )
        elif name.startswith(("writers.gdal", "writers.raster")):
            writer = "writers.gdal" if name.startswith("writers.gdal") else "writers.raster"
            facts[writer] = json.loads(json.dumps(node, allow_nan=False, default=str))
    facts["readers.las"] = readers
    facts["points_read"] = sum(
        r.get("count", 0) for r in readers if isinstance(r.get("count"), int)
    )
    return facts


# -- adapter --------------------------------------------------------------------------


class PdalAdapter(ToolAdapter):
    """PDAL ``pdal pipeline`` with ``writers.gdal`` binmode cell statistics.

    Constructor options (campaign ``[[tools]] options``):

    * ``executable``: path of ``pdal`` (``pdal.exe``). Default: ``$LIDARBENCH_PDAL``,
      else ``pdal`` on ``PATH``.
    * ``execution_mode``: ``"stream"`` (default, ``--stream``: fail if PDAL cannot
      stream), ``"standard"`` (``--nostream``, whole tiles in memory) or
      ``"prefer"`` (PDAL's own choice; the mode actually used is then not recorded).
      Part of the campaign identity.
    * ``expected_version``: refuse every other version (``"2.6.3"``).
    * ``python``: interpreter that runs the launcher (default: the orchestrator's).
    * ``version_timeout_s``: timeout of each probe query (default 60).
    """

    adapter_id = "pdal"
    display_name = "PDAL (pdal pipeline, writers.gdal binmode)"
    contract_tool = "pdal"
    workloads: Mapping[str, PdalWorkload] = WORKLOADS

    def __init__(
        self,
        executable: str = "",
        execution_mode: str = "stream",
        expected_version: str = "",
        python: str = "",
        version_timeout_s: float = 60.0,
    ) -> None:
        for name, value in (
            ("executable", executable),
            ("execution_mode", execution_mode),
            ("expected_version", expected_version),
            ("python", python),
        ):
            if not isinstance(value, str):
                raise TypeError(f"option {name!r} must be str, got {value!r}")
        if execution_mode not in EXECUTION_MODES:
            raise TypeError(f"option 'execution_mode' must be one of {list(EXECUTION_MODES)}")
        if expected_version and not _EXPECTED_RE.match(expected_version):
            raise TypeError(f"option 'expected_version' must look like 2.6.3: {expected_version!r}")
        if isinstance(version_timeout_s, bool) or not isinstance(version_timeout_s, int | float):
            raise TypeError(
                f"option 'version_timeout_s' must be a number, got {version_timeout_s!r}"
            )
        if version_timeout_s <= 0:
            raise TypeError("option 'version_timeout_s' must be > 0")
        self.executable_option = executable
        self.execution_mode = execution_mode
        self.expected_version = expected_version
        self.python_option = python or sys.executable
        self.version_timeout_s = float(version_timeout_s)
        self._pdal: Path | None = None
        self._source: str | None = None
        self._version: str | None = None
        self._full_version: str | None = None
        self._python: Path | None = None
        self._reader_threads: int | None = READER_THREADS
        self._stages: set[str] = set()
        self._headers: dict[tuple[Path, ...], dict[Path, tuple[float, float, float, float]]] = {}

    # -- probe and identity -------------------------------------------------------

    def probe(self, platform_info: PlatformInfo) -> Availability:
        """Locate ``pdal``, check version >= 2.5.0, required stages and options."""
        self._pdal = None
        if platform_info.system not in SUPPORTED_SYSTEMS:
            return Availability.unavailable(
                UnavailableReason.PLATFORM_UNSUPPORTED,
                f"PDAL adapter supports {', '.join(SUPPORTED_SYSTEMS)}, not {platform_info.system}",
            )
        python = resolve_executable(self.python_option)
        if python is None:
            return Availability.unavailable(
                UnavailableReason.MISCONFIGURED,
                f"Python interpreter for the PDAL launcher not found: {self.python_option}",
            )
        path, source = locate_executable(self.executable_option)
        hint = (
            f"set the 'executable' option or ${EXECUTABLE_ENV}, or put pdal on PATH (for "
            "example by activating its conda environment). PDAL is not installed automatically."
        )
        if path is None:
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED,
                f"pdal not found on PATH on {platform_info.system}; {hint}",
            )
        if not path.is_file():
            return Availability.unavailable(
                UnavailableReason.MISCONFIGURED, f"pdal from {source} is not a file: {path}"
            )
        try:
            numbers, full = parse_version(run_query(path, ["--version"], self.version_timeout_s))
        except ProbeError as exc:
            return Availability.unavailable(UnavailableReason.PROBE_FAILED, f"{path}: {exc}")
        version = ".".join(map(str, numbers))
        if numbers < MINIMUM_VERSION:
            return Availability.unavailable(
                UnavailableReason.VERSION_UNSUPPORTED,
                f"PDAL {version} is older than 2.5.0, which added writers.gdal binmode "
                "(required for cell statistics)",
            )
        if self.expected_version and version != self.expected_version:
            return Availability.unavailable(
                UnavailableReason.VERSION_UNSUPPORTED,
                f"PDAL {version}, but expected_version is {self.expected_version}",
            )
        try:
            stages = parse_drivers(
                run_query(path, ["--drivers", "--showjson"], self.version_timeout_s)
            )
            missing = [s for s in REQUIRED_STAGES if s not in stages]
            if missing:
                return Availability.unavailable(
                    UnavailableReason.MISCONFIGURED,
                    f"PDAL {version} at {path} lacks stage(s) {missing}",
                )
            options = parse_stage_options(
                run_query(
                    path, ["--options", "writers.gdal", "--showjson"], self.version_timeout_s
                ),
                "writers.gdal",
            )
            reader_options = parse_stage_options(
                run_query(path, ["--options", "readers.las", "--showjson"], self.version_timeout_s),
                "readers.las",
            )
        except ProbeError as exc:
            return Availability.unavailable(UnavailableReason.PROBE_FAILED, f"{path}: {exc}")
        self._reader_threads = READER_THREADS if "threads" in reader_options else None
        self._stages = stages
        missing = [o for o in REQUIRED_GDAL_OPTIONS if o not in options]
        if missing:
            return Availability.unavailable(
                UnavailableReason.VERSION_UNSUPPORTED,
                f"PDAL {version} writers.gdal lacks option(s) {missing}",
            )
        self._pdal, self._source, self._python = path, source, python
        self._version, self._full_version = version, full
        return Availability.ok({"pdal": path, "python": python})

    def _require_probe(self) -> Path:
        if self._pdal is None:
            from lidarbench.core.platform import get_platform_info

            availability = self.probe(get_platform_info())
            if not availability.available:
                raise RuntimeError(f"{self.adapter_id} is unavailable: {availability.detail}")
        assert self._pdal is not None
        return self._pdal

    def identify(self) -> ToolIdentity:
        """Version, executables (hashed by the campaign) and pipeline settings."""
        pdal = self._require_probe()
        assert self._python is not None
        return ToolIdentity(
            tool="PDAL",
            version=self._version,
            executables={"pdal": pdal, "python": self._python},
            details={
                "contract_tool": self.contract_tool,
                "full_version": self._full_version,
                "discovery": self._source,
                "execution_mode": self.execution_mode,
                "expected_version": self.expected_version or None,
                "launcher": WORKER_MODULE,
                "job_schema": worker.JOB_SCHEMA,
                "run_schema": worker.RUN_SCHEMA,
                "writer": "writers.gdal binmode=true; TIN: filters.faceraster + writers.raster",
                "nodata": NODATA,
                "data_type": DATA_TYPE,
                "required_stages": ",".join(REQUIRED_STAGES),
                "reader_threads": (
                    self._reader_threads
                    if self._reader_threads is not None
                    else "not set (readers.las has no threads option)"
                ),
            },
        )

    # -- workloads and commands -------------------------------------------------------

    def workload_support(self) -> list[WorkloadSupport]:
        """Two binmode cell statistics; legacy workloads without a contract are unsupported."""
        threading = ThreadingSupport(ThreadingModel.CONCURRENT_FILES, 1, None)
        return [
            *(
                WorkloadSupport(w, ImplementationStatus.RUNNABLE, threading=threading)
                for w in self.workloads
            ),
            *OTHER_WORKLOADS,
        ]

    def _workload(self, invocation: Invocation) -> PdalWorkload:
        try:
            return self.workloads[invocation.workload_id]
        except KeyError:
            raise UnsupportedCombinationError(
                f"{self.adapter_id} has no pipeline for {invocation.workload_id!r}"
            ) from None

    def _epsg(self, invocation: Invocation) -> int | None:
        unknown = sorted(set(invocation.parameters) - set(SUPPORTED_PARAMETERS))
        if unknown:
            raise UnsupportedCombinationError(
                f"{self.adapter_id} does not accept parameter(s) {unknown} for "
                f"{invocation.workload_id}; supported: {list(SUPPORTED_PARAMETERS)}"
            )
        epsg = invocation.parameters.get("epsg")
        if epsg is None:
            return None
        if isinstance(epsg, bool) or not isinstance(epsg, int) or epsg <= 0:
            raise UnsupportedCombinationError(
                f"parameter epsg must be a positive integer: {epsg!r}"
            )
        return epsg

    def _check_paths(self, invocation: Invocation) -> None:
        stems: dict[str, Path] = {}
        for path in invocation.inputs:
            if path.suffix.lower() not in LAS_SUFFIXES:
                raise UnsupportedCombinationError(
                    f"{self.adapter_id} reads LAS/LAZ point clouds only, got {path.name}"
                )
            key = path.stem.casefold()
            if key in stems:
                raise UnsupportedCombinationError(
                    f"inputs {stems[key].name} and {path.name} would write the same output"
                )
            stems[key] = path
            if "://" in os.fspath(path):
                raise UnsupportedCombinationError(
                    f"PDAL would treat '://' in {path} as a remote URL"
                )
        for artifact in self.expected_outputs(invocation):
            text = os.fspath(artifact.path)
            if "#" in text:
                raise UnsupportedCombinationError(
                    f"PDAL writers expand '#' in output paths into per-view file names: {text}"
                )
            if "://" in text:
                raise UnsupportedCombinationError(f"PDAL would treat '://' in {text} as a URL")

    def header_bounds(
        self, invocation: Invocation
    ) -> dict[Path, tuple[float, float, float, float]]:
        """Header XY bounds of every input (cached per input set).

        Raises:
            ValueError: A header is unreadable (planning then fails, not skips).
        """
        key = tuple(invocation.inputs)
        if key not in self._headers:
            bounds = {}
            for path in key:
                try:
                    bounds[path] = read_las_header(path).bounds_xy
                except (LasHeaderError, OSError) as exc:
                    raise ValueError(f"cannot read the LAS header of {path}: {exc}") from exc
            self._headers[key] = bounds
        return self._headers[key]

    def pipelines(self, invocation: Invocation) -> list[tuple[Path, str, dict[str, Any]]]:
        """``(tile, config file name, pipeline document)`` for every input, in order."""
        workload = self._workload(invocation)
        epsg = self._epsg(invocation)
        bounds = self.header_bounds(invocation)
        out = []
        for index, tile in enumerate(invocation.inputs, start=1):
            grid = snapped_grid(bounds[tile], workload.resolution)
            neighbours = buffered_neighbours(bounds, tile, workload.buffer)
            output = self._artifact(invocation, workload, tile)
            if workload.method == "tin":
                document = build_tin_pipeline(
                    tile,
                    neighbours,
                    output,
                    grid,
                    bounds[tile],
                    workload,
                    epsg,
                    self._reader_threads,
                )
            else:
                document = build_pipeline(
                    tile,
                    neighbours,
                    output,
                    grid,
                    workload.output_type,
                    epsg,
                    self._reader_threads,
                )
            out.append((tile, f"pipeline-{index:04d}.json", document))
        return out

    def effective_mode(self, workload: PdalWorkload) -> str:
        """``execution_mode`` actually used: non-streamable pipelines always run standard."""
        return self.execution_mode if workload.streamable else "standard"

    @staticmethod
    def _artifact(invocation: Invocation, workload: PdalWorkload, tile: Path) -> Path:
        return invocation.output_dir / workload.artifact_pattern.replace("{stem}", tile.stem)

    def build_command(self, invocation: Invocation) -> CommandSpec:
        """``python -m pdal_worker --job pdal-job.json`` plus one pipeline file per tile."""
        workload = self._workload(invocation)
        self._epsg(invocation)
        self._check_paths(invocation)
        try:
            pdal = self._require_probe()
        except RuntimeError as exc:
            raise UnsupportedCombinationError(str(exc)) from exc
        missing = [s for s in workload.stages if s not in self._stages]
        if missing:
            raise UnsupportedCombinationError(
                f"PDAL {self._version} lacks stage(s) {missing} needed by {workload.workload_id}"
            )
        python = self._python
        assert python is not None
        files = []
        tiles = []
        for tile, name, document in self.pipelines(invocation):
            files.append(ConfigFile(name, render_json(document)))
            base = name.removesuffix(".json")
            tiles.append(
                {
                    "stem": tile.stem,
                    "pipeline": name,
                    "metadata": f"{base}.metadata.json",
                    "log": f"{base}.log",
                }
            )
        job = {
            "schema": worker.JOB_SCHEMA,
            "pdal": os.fspath(pdal),
            "execution_mode": self.effective_mode(workload),
            "concurrency": invocation.threads,
            "tiles": tiles,
            "result": RUN_FILE,
        }
        files.append(ConfigFile(JOB_FILE, render_json(job)))
        env = {**thread_cap_env(1), **PDAL_THREAD_VARIABLES}
        argv = python_module_argv(python, WORKER_MODULE, "--job", invocation.config_path(JOB_FILE))
        return CommandSpec(argv=argv, cwd=invocation.work_dir, env=env, config_files=tuple(files))

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        """One single-band GeoTIFF per input tile, named ``{stem}.tif``."""
        workload = self._workload(invocation)
        return [
            ArtifactSpec(
                self._artifact(invocation, workload, path),
                ArtifactKind.RASTER,
                required=True,
                description=(
                    f"{workload.workload_id} writers.gdal {workload.output_type} for {path.name}"
                    if workload.method == "binmode"
                    else f"{workload.workload_id} TIN (faceraster) for {path.name}"
                ),
            )
            for path in invocation.inputs
        ]

    def invocation_metadata(self, invocation: Invocation) -> Mapping[str, MetadataValue]:
        """Pipeline settings, neighbour reads and the pipeline digests."""
        workload = self._workload(invocation)
        pipelines = self.pipelines(invocation)
        neighbour_reads = sum(
            sum(1 for s in doc["pipeline"] if s["type"] == "readers.las") - 1
            for _, _, doc in pipelines
        )
        digest = sha256_bytes(
            "\x00".join(render_json(doc) for _, _, doc in pipelines).encode("utf-8")
        )
        tin = workload.method == "tin"
        return {
            "pdal_version": self._version,
            "writer": "writers.raster" if tin else "writers.gdal",
            "binmode": None if tin else True,
            "output_type": "tin_faceraster" if tin else workload.output_type,
            "classes": ",".join(map(str, workload.classes)) or None,
            "max_triangle_edge_length": workload.max_triangle_edge_length,
            "resolution": workload.resolution,
            "buffer": workload.buffer,
            "nodata": NODATA,
            "data_type": DATA_TYPE,
            "epsg": self._epsg(invocation),
            "execution_mode": self.effective_mode(workload),
            "configured_execution_mode": self.execution_mode,
            "concurrency": invocation.threads,
            "reader_threads": self._reader_threads,
            "threading_model": str(ThreadingModel.CONCURRENT_FILES),
            "pipelines": len(pipelines),
            "neighbour_reads": neighbour_reads,
            "pipelines_sha256": digest,
            "inner_timing_boundary": "first pdal pipeline launch to last pdal pipeline exit",
        }

    # -- interpretation ---------------------------------------------------------------

    def interpret(self, invocation: Invocation, process: ProcessResult) -> Interpretation:
        """Read the run record, per-tile logs, metadata and outputs; fail closed."""
        workload = self._workload(invocation)
        work = invocation.work_dir
        details: dict[str, Any] = {
            "workload": workload.workload_id,
            "raw_exit_code": process.exit_code,
            "execution_mode": self.effective_mode(workload),
            "pdal_version": self._version,
            "pipelines": {},
            "tiles": [],
        }
        for _, name, _ in self.pipelines(invocation):
            try:
                details["pipelines"][name] = json.loads((work / name).read_text(encoding="utf-8"))
            except (OSError, ValueError) as exc:
                details["pipelines"][name] = {"error": str(exc)}

        try:
            run = json.loads((work / RUN_FILE).read_text(encoding="utf-8"))
            if not isinstance(run, dict) or run.get("schema") != worker.RUN_SCHEMA:
                raise ValueError(f"not a {worker.RUN_SCHEMA} record")
            records = run["tiles"]
            if not isinstance(records, list):
                raise ValueError("tiles is not a list")
        except (OSError, ValueError, KeyError) as exc:
            reason = f"PDAL launcher produced no usable run record ({RUN_FILE}): {exc}"
            if process.exit_code != 0:
                reason = f"PDAL launcher exited with status {process.exit_code}; {reason}"
            return Interpretation(failure_reason=reason, details=details)

        failures: list[str] = []
        warnings: list[str] = []
        errors: list[str] = []
        starts, ends = [], []
        for record in records:
            stem = record.get("stem")
            log_lines: list[str] = []
            if record.get("started"):
                try:
                    text = (work / record["log"]).read_text(encoding="utf-8", errors="replace")
                    log_lines = [ln for ln in text.splitlines() if ln.strip()]
                except OSError:
                    pass
            tile_errors = [ln for ln in log_lines if _ERROR_RE.search(ln)]
            tile_warnings = [
                ln for ln in log_lines if _WARNING_RE.search(ln) and ln not in tile_errors
            ]
            code = record.get("exit_code")
            if not record.get("started"):
                failures.append(
                    f"{stem}: not run ({record.get('error') or 'stopped after an earlier failure'})"
                )
            elif code != 0:
                shown = (
                    f": {_quote(tile_errors or log_lines)}" if (tile_errors or log_lines) else ""
                )
                failures.append(f"{stem}: pdal pipeline exited with status {code}{shown}")
            errors += [f"{stem}: {ln.strip()}" for ln in tile_errors]
            warnings += [f"{stem}: {ln.strip()}" for ln in tile_warnings]
            if isinstance(record.get("start_wall_ns"), int) and isinstance(
                record.get("end_wall_ns"), int
            ):
                starts.append(record["start_wall_ns"])
                ends.append(record["end_wall_ns"])
            metadata = (
                _metadata_facts(work / record["metadata"])
                if code == 0 and isinstance(record.get("metadata"), str)
                else {"present": False}
            )
            details["tiles"].append(
                {
                    "stem": stem,
                    "pipeline": record.get("pipeline"),
                    "exit_code": code,
                    "start_wall_ns": record.get("start_wall_ns"),
                    "end_wall_ns": record.get("end_wall_ns"),
                    "terminated": record.get("terminated"),
                    "log_lines": len(log_lines),
                    "log_sha256": record.get("log_sha256"),
                    "metadata": metadata,
                }
            )

        expected = {p.stem for p in invocation.inputs}
        recorded = [r.get("stem") for r in records]
        outputs = self._output_problems(invocation)
        details.update(
            error_lines=len(errors),
            warning_lines=len(warnings),
            output_problems=outputs,
            note=(
                "Execution checks only. writers.gdal binmode owns edge points rightward in X "
                "and upward in Y (pdal_binmode_floor); numerical agreement and comparability "
                "are decided by validation and the workload contracts."
                if workload.method == "binmode"
                else "Execution checks only. TIN rasters are sampled at cell centres by "
                "filters.faceraster; validation and the workload contract decide the rest."
            ),
        )
        failure = None
        if sorted(recorded) != sorted(expected):
            failure = (
                f"run record lists tiles {sorted(map(str, recorded))}, expected {sorted(expected)}"
            )
        elif failures:
            failure = f"PDAL failed: {_quote(failures)}"
        elif process.exit_code != 0:
            failure = f"PDAL launcher exited with status {process.exit_code}"
        elif errors:
            failure = f"PDAL reported errors despite exit status 0: {_quote(errors)}"
        elif warnings:
            failure = f"PDAL reported warnings (not accepted): {_quote(warnings)}"
        elif outputs:
            failure = f"incomplete PDAL outputs: {_quote(outputs)}"
        elif any(not t["metadata"].get("present") for t in details["tiles"]):
            failure = "a pdal pipeline exited 0 without writing its --metadata record"

        timing = None
        if starts and len(starts) == len(records):
            timing = ToolTiming(
                start_wall_ns=min(starts),
                end_wall_ns=max(ends),
                clock=str(run.get("clock", worker.CLOCK)),
                boundary=(
                    "first pdal pipeline launch to last pdal pipeline exit (excludes the "
                    "Python launcher start-up)"
                ),
            )
        return Interpretation(
            failure_reason=failure,
            warnings=tuple(warnings),
            tool_timing=timing,
            details=details,
        )

    def _output_problems(self, invocation: Invocation) -> list[str]:
        problems = []
        for artifact in self.expected_outputs(invocation):
            path = Path(artifact.path)
            try:
                if not path.is_file():
                    problems.append(f"missing {path.name}")
                elif path.stat().st_size == 0:
                    problems.append(f"empty {path.name}")
            except OSError as exc:
                problems.append(f"unreadable {path.name}: {exc.strerror or exc}")
        return problems
