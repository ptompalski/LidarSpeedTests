"""LAStools adapter: native LAStools command-line tools under Python orchestration.

Each attempt is one fresh native process (``lascanopy64``, ``lasgrid64``, …),
launched by the generic execution engine from an argument vector, never a
shell string. The adapter reuses the legacy definitions of ``R/0_setup.R``
(``task_call_lastools``) argument for argument, plus the legacy ``-epsg``
metadata assignment and an explicit ``-fail`` licensing request.

Discovery (no download, installation or licence handling):

1. the ``bin_dir`` tool option;
2. the ``LIDARBENCH_LASTOOLS_BIN`` environment variable;
3. the first ``PATH`` directory that contains a LAStools workload command.

Every command is then looked up in that single directory, so that one
installation (one build) is used. Windows uses the ``.exe`` binaries and other
systems the vendor's native binaries without a suffix. Commands are never
assumed to exist: a workload whose command is missing, not executable or has
no identifiable version is ``unsupported`` with the reason.

Identity records the normalized build number (``version 260914``) and the
executables (their SHA-256 digests are recorded by the campaign identity). The
licensing part of the version banner is never recorded: it can contain licensee
details and varies between identical queries (LASTOOLS_260914_ISSUE_REPORT.md).

Interpretation is conservative (WORKLOADS.md): nonzero exit statuses,
``ERROR``/``FATAL`` lines, unlicensed-run notices, unrecognized ``WARNING``
lines and missing or empty primary outputs all fail the attempt, because a
multi-core LAStools parent can exit 0 after a child failed. Three documented
warnings are the only exceptions, each disabled by default and recorded as a
named exception with the raw exit status:

* the ``lascanopy`` all-values-above-the-height-cutoff warning (tool option
  ``accept_known_cutoff_warning``; the pixel metrics only);
* the ``blast2dem`` triangulator's ``WARNING: set nverts N but v_count M`` with
  ``M < N``: points with identical XY were merged into one TIN vertex, and its
  rasterization's ``WARNING: there were N duplicate rasters`` (tool option
  ``accept_known_duplicate_xy_warning``; the ``blast2dem`` workloads
  ``generate_DSM2``, where first returns often share XY, and ``generate_DEM``);
* ``WARNING: on-the-fly merged LAS 1.2 files contain too many points (N) for
  single LAS 1.2 file.`` with N > 2**32 - 1 from a ``-buffered`` call, whose
  neighbour reader merges every input tile (tool option
  ``accept_known_merged_point_count_warning``; every buffered workload). When
  several apply, the attempt records the first of cutoff, duplicate-XY and
  merged-count as its exception; the details count each kind.
"""

from __future__ import annotations

import math
import os
import re
import subprocess
import sys
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass
from decimal import Decimal
from pathlib import Path
from types import MappingProxyType
from typing import TYPE_CHECKING

from lidarbench.core.canonical import sha256_bytes
from lidarbench.core.platform import PlatformInfo
from lidarbench.datasets.lasheader import LasHeaderError, read_las_header
from lidarbench.execution.adapters.base import (
    AcceptedException,
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ImplementationStatus,
    Interpretation,
    Invocation,
    MetadataValue,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    UnavailableReason,
    UnsupportedCombinationError,
    WorkloadSupport,
)
from lidarbench.execution.adapters.launchers import native_argv
from lidarbench.execution.adapters.rscript import LEGACY_THREAD_VARIABLES

if TYPE_CHECKING:
    from lidarbench.execution.process import ProcessResult

BIN_ENV = "LIDARBENCH_LASTOOLS_BIN"
"""Environment variable naming the LAStools ``bin`` directory when no option is given."""

CUTOFF_EXCEPTION = "lascanopy_all_values_above_height_cutoff"
CUTOFF_POLICY = "lastools.accept_known_cutoff_warning"
CUTOFF_COMMANDS = frozenset({"lascanopy", "lascanopy64"})
CUTOFF_SUGGESTION = "maybe run 'lasheight -i raw.las -replace_z -o normalized.laz'."
DUPLICATE_XY_EXCEPTION = "blast2dem_duplicate_xy_points_merged"
DUPLICATE_XY_POLICY = "lastools.accept_known_duplicate_xy_warning"
MERGED_COUNT_EXCEPTION = "lastools_merged_point_count_overflow"
MERGED_COUNT_POLICY = "lastools.accept_known_merged_point_count_warning"
LAS12_MAX_POINTS = 2**32 - 1
"""LAS 1.2 stores the point count in 32 bits."""

PLACEHOLDERS = ("{inputs}", "{output}", "{cores}", "{height_cutoff}")
SUPPORTED_PARAMETERS = ("epsg",)
LAS_SUFFIXES = (".las", ".laz")
NATIVE_THREAD_VARIABLES = ("OMP_NUM_THREADS", "OMP_THREAD_LIMIT")
"""Legacy ``benchmark_with_resources()`` gives LAStools the budget here, 1 elsewhere."""
WINDOWS_COMMAND_LINE_LIMIT = 32766
"""``CreateProcess`` accepts at most 32767 characters including the terminator."""
LICENCE_FLAG = "-fail"
"""Fail instead of running in unlicensed (demo) mode (WORKLOADS.md)."""

_NUMBER = r"[-+]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][-+]?[0-9]+)?"
_NUMBER_RE = re.compile(rf"^{_NUMBER}$")
_VERSION_RE = re.compile(r"^\s*LAStools\b.*?\bversion\s+([0-9][0-9A-Za-z._-]*)")
_BUILD_RE = re.compile(r"^[0-9][0-9A-Za-z._-]{0,63}$")
_ERROR_RE = re.compile(r"^\s*(ERROR|FATAL)(:|\s)", re.IGNORECASE)
_WARNING_RE = re.compile(r"^\s*WARNING:", re.IGNORECASE)
_UNLICENSED_RE = re.compile(r"\bunlicensed\b", re.IGNORECASE)
_LICENCE_TEXT_RE = re.compile(r"licen[cs]", re.IGNORECASE)
_CUTOFF_HEADER_RE = re.compile(
    rf"^WARNING: z coordinates range from ({_NUMBER}) to ({_NUMBER}) for '(.*)'$"
)
_CUTOFF_CONTINUATION_RE = re.compile(rf"^all values are above the height_cutoff ({_NUMBER})\.$")
_DUPLICATE_XY_RE = re.compile(r"^WARNING: set nverts ([0-9]+) but v_count ([0-9]+)$")
_DUPLICATE_RASTERS_RE = re.compile(r"^WARNING: there were ([0-9]+) duplicate rasters$")
_MERGED_COUNT_BODY = (
    r"on-the-fly merged LAS 1\.2 files contain too many points \(([0-9]+)\) "
    r"for single LAS 1\.2 file\."
)
_MERGED_COUNT_RE = re.compile(rf"^((?:WARNING: )*){_MERGED_COUNT_BODY}$")
_LINE_LIMIT = 20


@dataclass(frozen=True)
class LastoolsWorkload:
    """A workload implemented by one pinned LAStools call.

    ``args`` is the legacy ``task_call_lastools`` argument template of
    ``R/0_setup.R`` (or, with ``legacy_call = False``, a template that has no
    legacy counterpart and is defined by the workload contract) with the closed
    placeholder set :data:`PLACEHOLDERS`. ``{inputs}`` expands to one argument
    per input file. A changed template needs a new workload version.
    ``artifact_patterns`` are the primary outputs per input tile, in contract order.
    """

    workload_id: str
    command: str
    args: tuple[str, ...]
    artifact_patterns: tuple[str, ...]
    buffer: float = 20.0
    kind: ArtifactKind = ArtifactKind.RASTER
    cutoff_warning_applies: bool = False
    """Whether the documented lascanopy cutoff-warning exception may apply."""
    duplicate_xy_warning_applies: bool = False
    """Whether the blast2dem duplicate-XY (``set nverts N but v_count M``) exception may apply."""
    legacy_deviation: tuple[tuple[str, str, str], ...] = ()
    """``(flag, legacy value, pinned value)`` of a deliberately changed legacy template
    (declared in the contract note); empty = exactly the legacy call."""
    legacy_call: bool = True
    """Whether ``R/0_setup.R`` defines a LAStools call for this workload."""

    def __post_init__(self) -> None:
        for arg in self.args:
            if ("{" in arg or "}" in arg) and arg not in PLACEHOLDERS:
                raise ValueError(f"unknown LAStools argument placeholder: {arg}")
        if (
            not isinstance(self.artifact_patterns, tuple)
            or not self.artifact_patterns
            or len(set(self.artifact_patterns)) != len(self.artifact_patterns)
            or not all("{stem}" in p for p in self.artifact_patterns)
        ):
            raise ValueError("artifact_patterns must be distinct names containing {stem}")
        if self.legacy_deviation and not self.legacy_call:
            raise ValueError("legacy_deviation needs a legacy call")
        for flag, _, pinned in self.legacy_deviation:
            i = self.args.index(flag)
            if self.args[i + 1] != pinned:
                raise ValueError(f"legacy_deviation {flag} {pinned} is not in the arguments")

    def legacy_args(self) -> tuple[str, ...]:
        """The ``R/0_setup.R`` template this call derives from.

        Raises:
            ValueError: The workload has no legacy LAStools call.
        """
        if not self.legacy_call:
            raise ValueError(f"{self.workload_id} has no legacy LAStools call")
        args = list(self.args)
        for flag, legacy, _ in self.legacy_deviation:
            args[args.index(flag) + 1] = legacy
        return tuple(args)

    @property
    def template_sha256(self) -> str:
        """SHA-256 of the command and its argument template."""
        return sha256_bytes("\x00".join((self.command, *self.args)).encode("utf-8"))


# fmt: off
WORKLOADS: Mapping[str, LastoolsWorkload] = MappingProxyType(
    {
        # lasheight uses class 2 only as ground (the legacy call has no -classification 2 9).
        # Every input point is written (points outside the ground TIN get extrapolated
        # heights); -buffered points are used for the TIN but not written.
        "normalization": LastoolsWorkload(
            "normalization",
            command="lasheight64",
            args=(
                "-i", "{inputs}", "-odir", "{output}", "-olaz", "-cores", "{cores}",
                "-replace_z", "-buffered", "20",
            ),
            artifact_patterns=("{stem}.laz",),
            kind=ArtifactKind.POINT_CLOUD,
        ),
        "pixel_metrics_1a": LastoolsWorkload(
            "pixel_metrics_1a",
            command="lascanopy64",
            args=(
                "-i", "{inputs}", "-step", "20", "-avg", "-odir", "{output}",
                "-otif", "-cores", "{cores}", "-buffered", "20", "-use_orig_bb",
                "-height_cutoff", "{height_cutoff}", "-grid_ll", "0", "0",
                "-no_data", "-9999", "-no_kml",
            ),
            artifact_patterns=("{stem}_avg.tif",),
            cutoff_warning_applies=True,
        ),
        # Nine single-band rasters per tile, in the contract's band order (workloads-v8).
        "pixel_metrics_2": LastoolsWorkload(
            "pixel_metrics_2",
            command="lascanopy64",
            args=(
                "-i", "{inputs}", "-step", "20", "-max", "-avg", "-std", "-dns", "-p",
                "5", "25", "50", "75", "95", "-odir", "{output}", "-otif",
                "-cores", "{cores}", "-buffered", "20", "-use_orig_bb",
                "-height_cutoff", "{height_cutoff}", "-cover_cutoff", "2", "-fractions",
                "-grid_ll", "0", "0", "-no_data", "-9999", "-no_kml",
            ),
            artifact_patterns=tuple(
                f"{{stem}}_{m}.tif"
                for m in ("max", "avg", "std", "dns", "p05", "p25", "p50", "p75", "p95")
            ),
            cutoff_warning_applies=True,
        ),
        "generate_DSM1": LastoolsWorkload(
            "generate_DSM1",
            command="lasgrid64",
            args=(
                "-i", "{inputs}", "-highest", "-otif", "-odir", "{output}", "-step", "1",
                "-cores", "{cores}", "-buffered", "20", "-use_orig_bb",
                "-no_data", "-9999", "-no_kml",
            ),
            artifact_patterns=("{stem}.tif",),
        ),
        "generate_DEM": LastoolsWorkload(
            "generate_DEM",
            command="blast2dem64",
            args=(
                "-i", "{inputs}", "-otif", "-odir", "{output}", "-step", "1",
                "-cores", "{cores}", "-keep_class", "2", "9", "-elevation",
                "-kill", "100000", "-buffered", "20", "-use_orig_bb",
                "-no_data", "-9999", "-no_kml",
            ),
            artifact_patterns=("{stem}.tif",),
            # The same triangulator as generate_DSM2: one Petawawa d5 tile of 100 printed
            # "there were 1 duplicate rasters" in every run of the full-v9 campaign.
            duplicate_xy_warning_applies=True,
            # workloads-v6: no long-triangle trimming (like lasR) instead of the default 100 m.
            legacy_deviation=(("-kill", "100", "100000"),),
        ),
        # workloads-v8: the generate_DEM call with -keep_return 1 instead of -keep_class 2 9
        # (no legacy LAStools call exists for this workload). -first_only would also keep
        # points with the invalid return number 0.
        "generate_DSM2": LastoolsWorkload(
            "generate_DSM2",
            command="blast2dem64",
            args=(
                "-i", "{inputs}", "-otif", "-odir", "{output}", "-step", "1",
                "-cores", "{cores}", "-keep_return", "1", "-elevation",
                "-kill", "100000", "-buffered", "20", "-use_orig_bb",
                "-no_data", "-9999", "-no_kml",
            ),
            artifact_patterns=("{stem}.tif",),
            duplicate_xy_warning_applies=True,
            legacy_call=False,
        ),
    }
)
# fmt: on
"""Pinned calls; a parity test compares them with ``R/0_setup.R`` (after ``legacy_deviation``)."""

OTHER_WORKLOADS: tuple[WorkloadSupport, ...] = (
    WorkloadSupport(
        "tree_detection",
        ImplementationStatus.UNSUPPORTED,
        reason="no LAStools implementation in the legacy harness",
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


def command_filename(command: str, windows: bool) -> str:
    """File name of a LAStools command: ``<command>.exe`` on Windows, else ``<command>``."""
    return f"{command}.exe" if windows else command


def find_command(command: str, bin_dir: Path, windows: bool) -> Path | None:
    """Absolute path of ``command`` in ``bin_dir`` if it is a usable executable file."""
    candidate = bin_dir / command_filename(command, windows)
    try:
        if not candidate.is_file():
            return None
    except OSError:
        return None
    if not windows and not os.access(candidate, os.X_OK):
        return None
    return candidate.absolute()


def locate_bin_dir(
    option: str,
    commands: Sequence[str],
    windows: bool,
    environ: Mapping[str, str] | None = None,
) -> tuple[Path | None, str]:
    """Find the LAStools ``bin`` directory.

    Returns:
        ``(directory, source)`` with source ``option``, ``environment`` or ``PATH``.
        The directory is ``None`` when ``PATH`` holds none of ``commands`` (source
        ``PATH``). Configured directories are returned even if they do not exist.
    """
    env = os.environ if environ is None else environ
    if option:
        return Path(option).absolute(), "option"
    if env.get(BIN_ENV):
        return Path(env[BIN_ENV]).absolute(), "environment"
    for entry in env.get("PATH", "").split(os.pathsep):
        if not entry:
            continue
        directory = Path(entry.strip('"'))
        if any(find_command(c, directory, windows) for c in commands):
            return directory.absolute(), "PATH"
    return None, "PATH"


class VersionError(RuntimeError):
    """A LAStools version banner could not be identified."""


def parse_version(text: str) -> str:
    """Normalized build from ``-version`` output, for example ``"260914"``.

    Only the build number after ``version`` is returned. The trailing licensing
    note (``(non-profit)``, ``(unlicensed)``, a licensee) is discarded.

    Raises:
        VersionError: No or more than one ``LAStools … version …`` line.
    """
    builds = [m.group(1) for m in map(_VERSION_RE.match, text.splitlines()) if m]
    if not builds:
        raise VersionError("no 'LAStools ... version <build>' line in the -version output")
    if len(builds) > 1:
        raise VersionError(f"{len(builds)} version lines in the -version output")
    if not _BUILD_RE.match(builds[0]):
        raise VersionError("unrecognized build number in the -version output")
    return builds[0]


def query_version(executable: Path, timeout_s: float) -> str:
    """Run ``<executable> -version`` (argv, no shell) and return the normalized build.

    Raises:
        VersionError: The query could not start, timed out, failed or printed no
            single version line. Messages never quote the tool output.
    """
    try:
        done = subprocess.run(
            [os.fspath(executable), "-version"],
            capture_output=True,
            timeout=timeout_s,
            check=False,
            shell=False,
            stdin=subprocess.DEVNULL,
        )
    except subprocess.TimeoutExpired as exc:
        raise VersionError(f"-version timed out after {timeout_s:g} s") from exc
    except OSError as exc:
        raise VersionError(f"cannot start: {exc.strerror or exc}") from exc
    if done.returncode != 0:
        raise VersionError(f"-version exited with status {done.returncode}")
    text = (done.stdout + done.stderr).decode("utf-8", errors="replace")
    return parse_version(text)


@dataclass(frozen=True)
class CommandStatus:
    """Discovery result of one LAStools command."""

    command: str
    path: Path | None
    version: str | None = None
    problem: str | None = None

    @property
    def usable(self) -> bool:
        """Found, executable and identified."""
        return self.path is not None and self.problem is None


# -- height cutoff (port of benchmark_height_cutoff()) --------------------------------


def format_r17(value: float) -> str:
    """Format like R ``format(x, digits = 17, scientific = FALSE, trim = TRUE)``.

    Uses the fewest significant digits (at most 17) that give the same decimal
    value as 17 significant digits, written without an exponent.
    """
    if not math.isfinite(value):
        raise ValueError(f"cannot format a non-finite value: {value!r}")
    full = Decimal(f"{value:.16e}")
    for digits in range(1, 18):
        candidate = Decimal(f"{value:.{digits - 1}e}")
        if candidate == full:
            break
    text = format(candidate, "f")
    if "." in text:
        text = text.rstrip("0").rstrip(".")
    return "0" if text in ("-0", "") else text


def height_cutoff(min_z: Iterable[float]) -> str:
    """All-point ``-height_cutoff``: strictly below every header minimum Z.

    Port of ``benchmark_height_cutoff()`` (``R/1_functions.R``): subtract at least
    one coordinate unit from the smallest minimum Z and format with 17 digits.

    Raises:
        ValueError: No values, a non-finite value, or no representable cutoff.
    """
    values = list(min_z)
    if not values or not all(isinstance(v, float | int) and math.isfinite(v) for v in values):
        raise ValueError("cannot define an all-point height cutoff without finite header Min Z")
    minimum = float(min(values))
    cutoff = minimum - max(1.0, abs(minimum) * sys.float_info.epsilon * 4)
    if not math.isfinite(cutoff) or cutoff >= minimum:
        raise ValueError("cannot represent a height cutoff below the input Z")
    text = format_r17(cutoff)
    if float(text) >= minimum:
        raise ValueError("formatted height cutoff is not below the input Z")
    return text


def inputs_height_cutoff(inputs: Iterable[Path]) -> str:
    """:func:`height_cutoff` over the LAS/LAZ header minimum Z of every input."""
    values = []
    for path in inputs:
        try:
            values.append(read_las_header(path).min_xyz[2])
        except (LasHeaderError, OSError) as exc:
            raise ValueError(f"cannot read the LAS header of {path}: {exc}") from exc
    return height_cutoff(values)


# -- log interpretation ---------------------------------------------------------------


def output_lines(stdout: str, stderr: str) -> list[str]:
    """Combined stdout and stderr lines, as the legacy harness reads them."""
    return [line.rstrip("\r") for line in f"{stdout}\n{stderr}".split("\n")]


def known_cutoff_warnings(
    lines: Sequence[str], argv: Sequence[str], inputs: Iterable[str], cutoff: str
) -> list[int]:
    """Indices of complete, recognized lascanopy cutoff-warning blocks.

    Port of ``benchmark_lastools_cutoff_warnings()``: the three-line block must
    name a supplied input, a finite ordered Z range above the cutoff actually
    passed after the single ``-height_cutoff`` argument, and the exact
    suggestion line. Policy and workload checks are the caller's.
    """
    name = Path(argv[0]).name if argv else ""
    if name.lower().endswith(".exe"):
        name = name[:-4]
    if name.lower() not in CUTOFF_COMMANDS:
        return []
    positions = [i for i, a in enumerate(argv) if a == "-height_cutoff"]
    if (
        len(positions) != 1
        or positions[0] == len(argv) - 1
        or argv[positions[0] + 1] != cutoff
        or not _NUMBER_RE.match(cutoff)
    ):
        return []
    threshold = float(cutoff)
    if not math.isfinite(threshold):
        return []
    supplied = set(inputs)
    known = []
    for i, line in enumerate(lines):
        if not _WARNING_RE.match(line) or i + 2 >= len(lines):
            continue
        header = _CUTOFF_HEADER_RE.match(line.strip())
        continuation = _CUTOFF_CONTINUATION_RE.match(lines[i + 1].strip())
        if (
            header is None
            or continuation is None
            or header.group(3) not in supplied
            or lines[i + 2].strip() != CUTOFF_SUGGESTION
        ):
            continue
        low, high, reported = (
            float(header.group(1)),
            float(header.group(2)),
            float(continuation.group(1)),
        )
        if (
            all(math.isfinite(v) for v in (low, high, reported))
            and low <= high
            and low > threshold
            and abs(reported - threshold) <= 1e-12 * max(1.0, abs(threshold))
        ):
            known.append(i)
    return known


def known_duplicate_xy_warnings(lines: Sequence[str], argv: Sequence[str]) -> list[int]:
    """Indices of ``blast2dem`` duplicate warnings.

    Two exact forms are recognized:

    * ``WARNING: set nverts N but v_count M`` with 0 < M < N: the triangulator merged
      ``N - M`` points whose XY duplicates another point's;
    * ``WARNING: there were N duplicate rasters`` with N > 0: reported by the TIN
      rasterization (seen on one Petawawa d10 tile of 100 in every generate_DSM2 run
      of the workloads-v8 campaign, the rasters passed every structural check, and
      on one d5 tile in every generate_DEM run of the full-v9 campaign).
    Only ``blast2dem``/``blast2dem64`` output is recognized; policy and workload
    checks are the caller's.
    """
    name = Path(argv[0]).name.lower() if argv else ""
    if name.endswith(".exe"):
        name = name[:-4]
    if name not in ("blast2dem", "blast2dem64"):
        return []
    known = []
    for i, line in enumerate(lines):
        text = line.strip()
        match = _DUPLICATE_XY_RE.match(text)
        if match is not None and 0 < int(match.group(2)) < int(match.group(1)):
            known.append(i)
            continue
        rasters = _DUPLICATE_RASTERS_RE.match(text)
        if rasters is not None and int(rasters.group(1)) > 0:
            known.append(i)
    return known


def known_merged_count_warnings(lines: Sequence[str], argv: Sequence[str]) -> list[int]:
    """Indices of the on-the-fly merged point-count warning lines of a ``-buffered`` run.

    With ``-buffered`` every per-tile process opens all inputs as one on-the-fly
    merged neighbour reader; above 2**32 - 1 points in total (for example 100
    Petawawa tiles at 50 pts/m2) LAStools warns that the merged header cannot
    hold the count, and the outputs are unaffected (the full-v8 campaign's
    lascanopy d50 rasters passed numerical validation). Concurrent processes
    interleave their stderr, so one line can carry several ``WARNING: `` prefixes
    while their bodies follow on unprefixed lines. The lines are recognized only
    if every prefix is matched by a body, so a stray prefix of another warning is
    never absorbed; otherwise nothing is recognized. Policy checks are the caller's.
    """
    if "-buffered" not in argv:
        return []
    prefixed: list[int] = []
    prefixes = bodies = 0
    for i, line in enumerate(lines):
        match = _MERGED_COUNT_RE.match(line.strip())
        if match is None:
            continue
        if int(match.group(2)) <= LAS12_MAX_POINTS:
            return []
        bodies += 1
        count = match.group(1).count("WARNING:")
        if count:
            prefixed.append(i)
            prefixes += count
    return prefixed if prefixes == bodies else []


def _quote(lines: Iterable[str]) -> str:
    """Lines for a failure reason; licensing messages are withheld (see the logs)."""
    shown = [
        (
            "[licensing message withheld; see the attempt logs]"
            if _LICENCE_TEXT_RE.search(line)
            else line.strip()
        )
        for line in lines
    ]
    extra = len(shown) - _LINE_LIMIT
    text = "; ".join(shown[:_LINE_LIMIT])
    return text + (f"; … ({extra} more)" if extra > 0 else "")


# -- adapter --------------------------------------------------------------------------


class LastoolsAdapter(ToolAdapter):
    """LAStools native command-line tools (``lasheight64``, ``lascanopy64``, ``lasgrid64``, …).

    Constructor options (campaign ``[[tools]] options``):

    * ``bin_dir``: the LAStools ``bin`` directory. Default:
      ``$LIDARBENCH_LASTOOLS_BIN``, else the first ``PATH`` directory holding a
      workload command.
    * ``accept_known_cutoff_warning``: accept the documented lascanopy
      all-values-above-the-height-cutoff warning (raw status 0 or 1) as the
      named exception ``lascanopy_all_values_above_height_cutoff``. Default
      ``False``; part of the campaign identity.
    * ``accept_known_duplicate_xy_warning``: accept blast2dem's duplicate-XY and
      duplicate-raster warnings (raw status 0) for ``generate_DSM2`` and ``generate_DEM``
      as the named exception
      ``blast2dem_duplicate_xy_points_merged``. Default ``False``; part of the
      campaign identity.
    * ``accept_known_merged_point_count_warning``: accept the on-the-fly merged LAS 1.2
      point-count warning of ``-buffered`` calls whose inputs hold more than 2**32 - 1
      points in total (raw status 0) as the named exception
      ``lastools_merged_point_count_overflow``. Default ``False``; part of the
      campaign identity.
    * ``expected_version``: refuse every other build (for example ``"260914"``).
    * ``version_timeout_s``: timeout of each ``-version`` query (default 30).
    """

    adapter_id = "lastools"
    display_name = "LAStools (native command-line tools)"
    contract_tool = "lastools"
    workloads: Mapping[str, LastoolsWorkload] = WORKLOADS
    other_workloads: tuple[WorkloadSupport, ...] = OTHER_WORKLOADS

    def __init__(
        self,
        bin_dir: str = "",
        accept_known_cutoff_warning: bool = False,
        expected_version: str = "",
        version_timeout_s: float = 30.0,
        accept_known_duplicate_xy_warning: bool = False,
        accept_known_merged_point_count_warning: bool = False,
    ) -> None:
        for name, value, kind in (
            ("bin_dir", bin_dir, str),
            ("accept_known_cutoff_warning", accept_known_cutoff_warning, bool),
            ("accept_known_duplicate_xy_warning", accept_known_duplicate_xy_warning, bool),
            (
                "accept_known_merged_point_count_warning",
                accept_known_merged_point_count_warning,
                bool,
            ),
            ("expected_version", expected_version, str),
        ):
            if not isinstance(value, kind):
                raise TypeError(f"option {name!r} must be {kind.__name__}, got {value!r}")
        if expected_version and not _BUILD_RE.match(expected_version):
            raise TypeError(
                f"option 'expected_version' is not a build number: {expected_version!r}"
            )
        if isinstance(version_timeout_s, bool) or not isinstance(version_timeout_s, int | float):
            raise TypeError(
                f"option 'version_timeout_s' must be a number, got {version_timeout_s!r}"
            )
        if version_timeout_s <= 0:
            raise TypeError("option 'version_timeout_s' must be > 0")
        self.bin_dir_option = bin_dir
        self.accept_known_cutoff_warning = accept_known_cutoff_warning
        self.accept_known_duplicate_xy_warning = accept_known_duplicate_xy_warning
        self.accept_known_merged_point_count_warning = accept_known_merged_point_count_warning
        self.expected_version = expected_version
        self.version_timeout_s = float(version_timeout_s)
        self.windows = sys.platform == "win32"
        self._bin_dir: Path | None = None
        self._source: str | None = None
        self._commands: dict[str, CommandStatus] | None = None
        self._cutoffs: dict[tuple[Path, ...], str] = {}

    @property
    def commands(self) -> tuple[str, ...]:
        """LAStools commands used by the implemented workloads (sorted)."""
        return tuple(sorted({w.command for w in self.workloads.values()}))

    # -- probe and identity -------------------------------------------------------

    def discover(self) -> tuple[Path | None, str, dict[str, CommandStatus]]:
        """Locate the ``bin`` directory and every workload command (no version query)."""
        bin_dir, source = locate_bin_dir(self.bin_dir_option, self.commands, self.windows)
        statuses = {}
        for command in self.commands:
            path = find_command(command, bin_dir, self.windows) if bin_dir else None
            problem = None
            if path is None:
                where = f"in {bin_dir}" if bin_dir else "on PATH"
                problem = (
                    f"{command_filename(command, self.windows)} not found or not executable {where}"
                )
            statuses[command] = CommandStatus(command, path, problem=problem)
        return bin_dir, source, statuses

    def probe(self, platform_info: PlatformInfo) -> Availability:
        """Find the installation, then identify each workload command with ``-version``."""
        self.windows = platform_info.system == "Windows"
        self._bin_dir, self._source, self._commands = None, None, None
        bin_dir, source, statuses = self.discover()
        hint = (
            f"set the 'bin_dir' option or ${BIN_ENV}, or put the LAStools bin directory on "
            "PATH. LAStools is not downloaded or installed automatically."
        )
        if bin_dir is None:
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED,
                f"no LAStools workload command ({', '.join(self._filenames())}) found on PATH "
                f"on {platform_info.system}; {hint}",
            )
        if source != "PATH" and not bin_dir.is_dir():
            return Availability.unavailable(
                UnavailableReason.MISCONFIGURED,
                f"LAStools bin directory from {source} does not exist: {bin_dir}",
            )
        if not any(s.path for s in statuses.values()):
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED,
                f"no LAStools workload command ({', '.join(self._filenames())}) in {bin_dir} "
                f"on {platform_info.system}; {hint}",
            )
        for command, status in list(statuses.items()):
            if status.path is None:
                continue
            try:
                version = query_version(status.path, self.version_timeout_s)
            except VersionError as exc:
                statuses[command] = CommandStatus(command, status.path, problem=f"{exc}")
                continue
            problem = None
            if self.expected_version and version != self.expected_version:
                problem = f"version {version}, but expected_version is {self.expected_version}"
            statuses[command] = CommandStatus(command, status.path, version, problem)

        usable = {c: s for c, s in statuses.items() if s.usable}
        problems = "; ".join(f"{c}: {s.problem}" for c, s in statuses.items() if s.problem)
        if not usable:
            reason = (
                UnavailableReason.VERSION_UNSUPPORTED
                if self.expected_version and any(s.version for s in statuses.values())
                else UnavailableReason.PROBE_FAILED
            )
            return Availability.unavailable(reason, f"no usable LAStools command: {problems}")
        builds = sorted({s.version for s in usable.values() if s.version})
        if len(builds) > 1:
            found = ", ".join(f"{c} {s.version}" for c, s in usable.items())
            return Availability.unavailable(
                UnavailableReason.MISCONFIGURED,
                f"mixed LAStools builds in {bin_dir} ({found}); use one installation",
            )
        self._bin_dir, self._source, self._commands = bin_dir, source, statuses
        return Availability.ok({c: s.path for c, s in usable.items() if s.path})

    def _filenames(self) -> list[str]:
        return [command_filename(c, self.windows) for c in self.commands]

    def _probed(self) -> dict[str, CommandStatus]:
        if self._commands is None:
            from lidarbench.core.platform import get_platform_info

            availability = self.probe(get_platform_info())
            if not availability.available:
                raise RuntimeError(f"{self.adapter_id} is unavailable: {availability.detail}")
        assert self._commands is not None
        return self._commands

    def identify(self) -> ToolIdentity:
        """Normalized build, executables (hashed by the campaign) and discovery facts."""
        statuses = self._probed()
        usable = {c: s for c, s in statuses.items() if s.usable}
        builds = sorted({s.version for s in usable.values() if s.version})
        details: dict[str, MetadataValue] = {
            "contract_tool": self.contract_tool,
            "bin_dir": os.fspath(self._bin_dir) if self._bin_dir else None,
            "discovery": self._source,
            "platform_binaries": "windows-exe" if self.windows else "native",
            "accept_known_cutoff_warning": self.accept_known_cutoff_warning,
            "accept_known_duplicate_xy_warning": self.accept_known_duplicate_xy_warning,
            "accept_known_merged_point_count_warning": (
                self.accept_known_merged_point_count_warning
            ),
            "expected_version": self.expected_version or None,
            "licence_flag": LICENCE_FLAG,
            "version_source": "-version banner; build number only, licensing text not recorded",
        }
        for command, status in sorted(statuses.items()):
            details[f"version.{command}"] = status.version
            details[f"problem.{command}"] = status.problem
        return ToolIdentity(
            tool="LAStools",
            version=builds[0] if len(builds) == 1 else None,
            executables={c: s.path for c, s in usable.items() if s.path},
            details=details,
        )

    # -- workloads and commands -------------------------------------------------------

    def workload_support(self) -> list[WorkloadSupport]:
        """Runnable when the workload's command is usable; unsupported otherwise."""
        threading = ThreadingSupport(ThreadingModel.NATIVE_THREADS, 1, None)
        declared = []
        for workload in self.workloads.values():
            status = (self._commands or {}).get(workload.command)
            if status is not None and not status.usable:
                declared.append(
                    WorkloadSupport(
                        workload.workload_id,
                        ImplementationStatus.UNSUPPORTED,
                        reason=f"LAStools command unavailable: {status.problem}",
                        threading=threading,
                    )
                )
            else:
                declared.append(
                    WorkloadSupport(
                        workload.workload_id, ImplementationStatus.RUNNABLE, threading=threading
                    )
                )
        return declared + list(self.other_workloads)

    def _workload(self, invocation: Invocation) -> LastoolsWorkload:
        try:
            return self.workloads[invocation.workload_id]
        except KeyError:
            raise UnsupportedCombinationError(
                f"{self.adapter_id} has no pinned LAStools call for {invocation.workload_id!r}"
            ) from None

    def _epsg(self, invocation: Invocation) -> int | None:
        unknown = sorted(set(invocation.parameters) - set(SUPPORTED_PARAMETERS))
        if unknown:
            raise UnsupportedCombinationError(
                f"{self.adapter_id} does not accept parameter(s) {unknown} for "
                f"{invocation.workload_id}; supported: {list(SUPPORTED_PARAMETERS)} "
                "(the height cutoff is always derived from the input headers)"
            )
        epsg = invocation.parameters.get("epsg")
        if epsg is None:
            return None
        if isinstance(epsg, bool) or not isinstance(epsg, int) or epsg <= 0:
            raise UnsupportedCombinationError(
                f"parameter epsg must be a positive integer: {epsg!r}"
            )
        return epsg

    def _check_inputs(self, invocation: Invocation) -> None:
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

    def _executable(self, command: str) -> Path:
        statuses = self._commands
        if statuses is None:
            _, _, statuses = self.discover()
        status = statuses.get(command)
        if status is None or status.path is None or (self._commands and not status.usable):
            reason = status.problem if status else f"{command} is not a workload command"
            raise UnsupportedCombinationError(f"LAStools command unavailable: {reason}")
        return status.path

    def height_cutoff(self, invocation: Invocation) -> str:
        """The all-point cutoff for ``invocation.inputs`` (cached per input set).

        Raises:
            ValueError: A header is unreadable or has no finite minimum Z (the
                attempt then fails in planning rather than being skipped).
        """
        key = tuple(invocation.inputs)
        if key not in self._cutoffs:
            self._cutoffs[key] = inputs_height_cutoff(key)
        return self._cutoffs[key]

    def arguments(self, invocation: Invocation) -> list[str]:
        """Expanded legacy argument template plus ``-epsg`` and :data:`LICENCE_FLAG`."""
        workload = self._workload(invocation)
        values = {"{output}": os.fspath(invocation.output_dir), "{cores}": str(invocation.threads)}
        if "{height_cutoff}" in workload.args:
            values["{height_cutoff}"] = self.height_cutoff(invocation)
        args: list[str] = []
        for arg in workload.args:
            if arg == "{inputs}":
                args.extend(os.fspath(p) for p in invocation.inputs)
            else:
                args.append(values.get(arg, arg))
        epsg = self._epsg(invocation)
        if epsg is not None:
            args += ["-epsg", str(epsg)]
        return [*args, LICENCE_FLAG]

    def thread_env(self, threads: int) -> dict[str, str]:
        """Environment delta of the legacy harness: caps of 1, OpenMP at the budget."""
        env = dict.fromkeys(LEGACY_THREAD_VARIABLES, "1")
        env.update(dict.fromkeys(NATIVE_THREAD_VARIABLES, str(threads)))
        return env

    def build_command(self, invocation: Invocation) -> CommandSpec:
        """``<bin_dir>/<command>[.exe] <legacy args> [-epsg N] -fail`` as an argv tuple."""
        workload = self._workload(invocation)
        self._check_inputs(invocation)
        self._epsg(invocation)
        executable = self._executable(workload.command)
        argv = native_argv(executable, *self.arguments(invocation))
        if self.windows:
            length = len(subprocess.list2cmdline(argv))
            if length > WINDOWS_COMMAND_LINE_LIMIT:
                raise UnsupportedCombinationError(
                    f"command line of {length} characters exceeds the Windows limit of "
                    f"{WINDOWS_COMMAND_LINE_LIMIT}; the pinned call passes every tile with -i"
                )
        accepted = tuple(
            name
            for name, applies in (
                (CUTOFF_EXCEPTION, self._cutoff_applies(workload)),
                (DUPLICATE_XY_EXCEPTION, self._duplicate_xy_applies(workload)),
                (MERGED_COUNT_EXCEPTION, self._merged_count_applies(workload)),
            )
            if applies
        )
        return CommandSpec(
            argv=argv,
            cwd=invocation.work_dir,
            env=self.thread_env(invocation.threads),
            accepted_exceptions=accepted,
        )

    def _cutoff_applies(self, workload: LastoolsWorkload) -> bool:
        return self.accept_known_cutoff_warning and workload.cutoff_warning_applies

    def _duplicate_xy_applies(self, workload: LastoolsWorkload) -> bool:
        return self.accept_known_duplicate_xy_warning and workload.duplicate_xy_warning_applies

    def _merged_count_applies(self, workload: LastoolsWorkload) -> bool:
        return self.accept_known_merged_point_count_warning and "-buffered" in workload.args

    def _policy(self, workload: LastoolsWorkload) -> str | None:
        policies = [
            policy
            for policy, applies in (
                (CUTOFF_POLICY, self._cutoff_applies(workload)),
                (DUPLICATE_XY_POLICY, self._duplicate_xy_applies(workload)),
                (MERGED_COUNT_POLICY, self._merged_count_applies(workload)),
            )
            if applies
        ]
        return ", ".join(policies) or None

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        """The rasters or point cloud of each input tile, named as the workload contract declares."""
        workload = self._workload(invocation)
        return [
            ArtifactSpec(
                invocation.output_dir / pattern.replace("{stem}", path.stem),
                workload.kind,
                required=True,
                description=f"{workload.workload_id} output of {workload.command} for {path.name}",
            )
            for path in invocation.inputs
            for pattern in workload.artifact_patterns
        ]

    def invocation_metadata(self, invocation: Invocation) -> Mapping[str, MetadataValue]:
        """Command, template digest, cutoff, CRS assignment and exception policy."""
        workload = self._workload(invocation)
        return {
            "lastools_command": workload.command,
            "legacy_call": workload.legacy_call,
            "legacy_template_sha256": workload.template_sha256,
            "legacy_deviation": (
                "; ".join(f"{f} {old} -> {new}" for f, old, new in workload.legacy_deviation)
                or None
            ),
            "buffer": workload.buffer,
            "height_cutoff": (
                self.height_cutoff(invocation) if "{height_cutoff}" in workload.args else None
            ),
            "epsg": self._epsg(invocation),
            "licence_flag": LICENCE_FLAG,
            "threading_model": str(ThreadingModel.NATIVE_THREADS),
            "accepted_exception_policy": self._policy(workload),
            "inner_timing_boundary": None,
        }

    # -- interpretation ---------------------------------------------------------------

    def interpret(self, invocation: Invocation, process: ProcessResult) -> Interpretation:
        """Read the LAStools logs and outputs; anything unexplained fails the attempt."""
        workload = self._workload(invocation)
        stdout = process.stdout_text() if process.stdout_path.exists() else ""
        stderr = process.stderr_text() if process.stderr_path.exists() else ""
        lines = output_lines(stdout, stderr)
        errors = [line for line in lines if _ERROR_RE.match(line)]
        warning_indices = [i for i, line in enumerate(lines) if _WARNING_RE.match(line)]
        unlicensed = [line for line in lines if _UNLICENSED_RE.search(line)]

        known: list[int] = []
        duplicates: list[int] = []
        cutoff = None
        if self._cutoff_applies(workload):
            cutoff = self.height_cutoff(invocation)
            known = known_cutoff_warnings(
                lines, process.argv, (os.fspath(p) for p in invocation.inputs), cutoff
            )
        if self._duplicate_xy_applies(workload):
            duplicates = known_duplicate_xy_warnings(lines, process.argv)
        merged: list[int] = []
        if self._merged_count_applies(workload):
            merged = known_merged_count_warnings(lines, process.argv)
        recognized = set(known) | set(duplicates) | set(merged)
        unknown = [lines[i] for i in warning_indices if i not in recognized]
        exit_code = process.exit_code
        # Only the cutoff warning is documented with a raw exit status of 1.
        known_status = (
            exit_code == 1 and not errors and bool(known) and bool(warning_indices) and not unknown
        )
        outputs = self._output_problems(invocation)

        failure = None
        if exit_code != 0 and not known_status:
            failure = f"LAStools exited with status {exit_code}"
            if errors:
                failure += f": {_quote(errors)}"
        elif errors:
            failure = (
                f"LAStools reported an error despite exit status {exit_code}: {_quote(errors)}"
            )
        elif unlicensed:
            failure = (
                "LAStools reported an unlicensed run; outputs may be perturbed and are not "
                "accepted (licensing is never bypassed)"
            )
        elif unknown:
            failure = (
                "LAStools reported warnings outside the accepted exceptions "
                f"(accept_known_cutoff_warning={self.accept_known_cutoff_warning}, "
                f"accept_known_duplicate_xy_warning={self.accept_known_duplicate_xy_warning}, "
                "accept_known_merged_point_count_warning="
                f"{self.accept_known_merged_point_count_warning}): "
                f"{_quote(unknown)}"
            )
        elif outputs:
            failure = (
                f"incomplete LAStools outputs ({_quote(outputs)}); a multi-core LAStools parent "
                "can exit 0 after a child process failed"
            )

        accepted = None
        if failure is None and known:
            evidence = tuple(line.strip() for i in known for line in lines[i : i + 3])
            accepted = AcceptedException(CUTOFF_EXCEPTION, exit_code or 0, CUTOFF_POLICY, evidence)
        elif failure is None and duplicates:
            evidence = tuple(lines[i].strip() for i in duplicates)
            accepted = AcceptedException(
                DUPLICATE_XY_EXCEPTION, exit_code or 0, DUPLICATE_XY_POLICY, evidence
            )
        elif failure is None and merged:
            evidence = tuple(lines[i].strip() for i in merged)
            accepted = AcceptedException(
                MERGED_COUNT_EXCEPTION, exit_code or 0, MERGED_COUNT_POLICY, evidence
            )
        return Interpretation(
            failure_reason=failure,
            warnings=tuple(line.strip() for line in unknown),
            details={
                "lastools_command": workload.command,
                "raw_exit_code": exit_code,
                "error_lines": len(errors),
                "warning_lines": len(warning_indices),
                "accepted_warning_blocks": len(known),
                "accepted_duplicate_xy_warnings": len(duplicates),
                "accepted_merged_point_count_warnings": len(merged),
                "unlicensed_notices": len(unlicensed),
                "height_cutoff": cutoff,
                "output_problems": outputs,
                "note": (
                    "Execution checks only: native numerical, cell-ownership, edge and "
                    "encoding conventions remain unverified (WORKLOADS.md)."
                ),
            },
            accepted_exception=accepted,
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
