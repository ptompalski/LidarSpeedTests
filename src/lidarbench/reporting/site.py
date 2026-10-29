"""Stage and render the Quarto website of a report (optional dependencies).

The report directory is itself a Quarto project. Templates ship with the
package (``reporting/templates``); the legacy ``R/reporting.R`` is staged from
the source checkout when present so its ``benchmark_report_plot()`` draws the
mean ± SD figures, as in the legacy site. Rendering needs Quarto, Rscript and
the R packages ``knitr`` and ``rmarkdown`` (``ggplot2`` for figures). Missing
pieces are reported as diagnostics; the tables are complete without them.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from dataclasses import dataclass, field
from importlib import resources
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import sha256_file

QUARTO_ENV = "QUARTO"
RSCRIPT_ENV = "LIDARBENCH_RSCRIPT"
REQUIRED_R_PACKAGES = ("knitr", "rmarkdown")
OPTIONAL_R_PACKAGES = ("ggplot2",)
RENDER_LOG = "render.log"
SITE_DIR = "_site"
LEGACY_REPORTING = "R/reporting.R"

TEMPLATE_FILES = (
    "_quarto.yml",
    "styles.css",
    "lidarbench_report.R",
    "index.qmd",
    "performance.qmd",
    "comparisons.qmd",
    "scaling.qmd",
    "resources.qmd",
    "issues.qmd",
    "environment.qmd",
    "historical.qmd",
    "downloads.qmd",
)

_R_PROBE = r"""
if (file.exists("renv/activate.R") && identical(Sys.getenv("LIDARBENCH_RENV"), "on")) {
  suppressMessages(source("renv/activate.R"))
}
cat("R_VERSION\t", R.version.string, "\n", sep = "")
for (p in .libPaths()) cat("LIB\t", normalizePath(p, winslash = "/"), "\n", sep = "")
for (p in strsplit(Sys.getenv("LIDARBENCH_PACKAGES"), ",")[[1]]) {
  ok <- requireNamespace(p, quietly = TRUE)
  v <- if (ok) as.character(utils::packageVersion(p)) else ""
  cat("PKG\t", p, "\t", ok, "\t", v, "\n", sep = "")
}
"""


def repo_root() -> Path | None:
    """Source checkout containing this package (with ``R/reporting.R``), if any."""
    root = Path(__file__).resolve().parents[3]
    return root if (root / LEGACY_REPORTING).is_file() else None


def stage_site(out_dir: Path, checkout: Path | None) -> dict[str, str]:
    """Copy the Quarto templates (and legacy ``R/reporting.R``); return file digests."""
    staged: dict[str, str] = {}
    templates = resources.files("lidarbench.reporting").joinpath("templates")
    for name in TEMPLATE_FILES:
        target = out_dir / name
        with open(target, "xb") as handle:
            handle.write(templates.joinpath(name).read_bytes())
        staged[name] = sha256_file(target)
    if checkout is not None and (checkout / LEGACY_REPORTING).is_file():
        target = out_dir / LEGACY_REPORTING
        target.parent.mkdir(exist_ok=True)
        with open(target, "xb") as handle:
            handle.write((checkout / LEGACY_REPORTING).read_bytes())
        staged[LEGACY_REPORTING] = sha256_file(target)
    return staged


@dataclass
class RenderProbe:
    """What rendering needs and what was found."""

    quarto: str | None = None
    quarto_version: str | None = None
    rscript: str | None = None
    r_version: str | None = None
    renv: bool = False
    lib_paths: list[str] = field(default_factory=list)
    packages: dict[str, str | None] = field(default_factory=dict)
    problems: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)

    @property
    def ready(self) -> bool:
        """Whether rendering can be attempted."""
        return not self.problems

    def to_record(self) -> dict[str, Any]:
        """JSON-ready form."""
        return {
            "quarto": self.quarto,
            "quarto_version": self.quarto_version,
            "rscript": self.rscript,
            "r_version": self.r_version,
            "renv": self.renv,
            "lib_paths": list(self.lib_paths),
            "packages": dict(self.packages),
            "problems": list(self.problems),
            "warnings": list(self.warnings),
        }


def _which(option: str | None, env: str, name: str) -> str | None:
    candidate = option or os.environ.get(env) or name
    found = shutil.which(candidate)
    if found is None and Path(candidate).is_file():
        found = str(Path(candidate).resolve())
    return found


def probe_render(
    quarto: str | None = None,
    rscript: str | None = None,
    checkout: Path | None = None,
    use_renv: bool = True,
    timeout_s: float = 120,
) -> RenderProbe:
    """Find Quarto, Rscript and the R packages the pages need."""
    probe = RenderProbe()
    probe.quarto = _which(quarto, QUARTO_ENV, "quarto")
    if probe.quarto is None:
        probe.problems.append(
            "Quarto was not found (install it from https://quarto.org, put it on PATH, "
            f"set ${QUARTO_ENV} or pass --quarto)"
        )
    else:
        try:
            out = subprocess.run(
                [probe.quarto, "--version"],
                capture_output=True,
                text=True,
                timeout=timeout_s,
                check=False,
                stdin=subprocess.DEVNULL,
            )
            if out.returncode == 0:
                probe.quarto_version = out.stdout.strip()
            else:
                probe.problems.append(f"{probe.quarto} --version failed: {out.stderr.strip()}")
        except (OSError, subprocess.SubprocessError) as exc:
            probe.problems.append(f"cannot run Quarto: {exc}")
    probe.rscript = _which(rscript, RSCRIPT_ENV, "Rscript")
    if probe.rscript is None:
        probe.problems.append(
            f"Rscript was not found (install R, put it on PATH, set ${RSCRIPT_ENV} or pass "
            "--rscript); the Quarto pages use the knitr engine"
        )
        return probe
    probe.renv = bool(use_renv and checkout and (checkout / "renv" / "activate.R").is_file())
    env = {
        **os.environ,
        "LIDARBENCH_RENV": "on" if probe.renv else "off",
        "LIDARBENCH_PACKAGES": ",".join(REQUIRED_R_PACKAGES + OPTIONAL_R_PACKAGES),
    }
    try:
        out = subprocess.run(
            [probe.rscript, "--vanilla", "-e", _R_PROBE],
            capture_output=True,
            text=True,
            timeout=timeout_s,
            check=False,
            cwd=checkout if probe.renv else None,
            env=env,
            stdin=subprocess.DEVNULL,
        )
    except (OSError, subprocess.SubprocessError) as exc:
        probe.problems.append(f"cannot run Rscript: {exc}")
        return probe
    if out.returncode != 0:
        probe.problems.append(f"Rscript probe failed: {out.stderr.strip()[-500:]}")
        return probe
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if parts[0] == "R_VERSION" and len(parts) > 1:
            probe.r_version = parts[1]
        elif parts[0] == "LIB" and len(parts) > 1:
            probe.lib_paths.append(parts[1])
        elif parts[0] == "PKG" and len(parts) > 3:
            probe.packages[parts[1]] = parts[3] if parts[2] == "TRUE" else None
    for name in REQUIRED_R_PACKAGES:
        if not probe.packages.get(name):
            probe.problems.append(f"R package {name!r} is not installed (needed by Quarto/knitr)")
    for name in OPTIONAL_R_PACKAGES:
        if not probe.packages.get(name):
            probe.warnings.append(f"R package {name!r} is not installed: figures are omitted")
    return probe


@dataclass
class RenderResult:
    """Outcome of one render."""

    status: str
    """``rendered``, ``failed``, ``skipped`` or ``not_requested``."""
    detail: str | None = None
    site: str | None = None

    def to_record(self) -> dict[str, Any]:
        """JSON-ready form."""
        return {"status": self.status, "detail": self.detail, "site": self.site}


def render_site(out_dir: Path, probe: RenderProbe, timeout_s: float | None = 3600) -> RenderResult:
    """Run ``quarto render`` in the report directory; log to ``render.log``."""
    assert probe.quarto is not None and probe.rscript is not None
    env = {k: v for k, v in os.environ.items() if k not in ("QUARTO_PROJECT_DIR", "QUARTO_PROFILE")}
    env["QUARTO_R"] = str(Path(probe.rscript).parent)
    if probe.lib_paths:
        env["R_LIBS"] = os.pathsep.join(probe.lib_paths)
        env["R_LIBS_USER"] = os.pathsep.join(probe.lib_paths)
    try:
        out = subprocess.run(
            [probe.quarto, "render", "--no-cache"],
            cwd=out_dir,
            env=env,
            capture_output=True,
            text=True,
            timeout=timeout_s,
            check=False,
            stdin=subprocess.DEVNULL,
        )
        log = out.stdout + out.stderr
        code: int | None = out.returncode
    except subprocess.TimeoutExpired as exc:
        log, code = f"quarto render timed out after {timeout_s} s\n{exc}", None
    except OSError as exc:
        log, code = f"cannot run quarto: {exc}", None
    with open(out_dir / RENDER_LOG, "x", encoding="utf-8") as handle:
        handle.write(log)
    index = out_dir / SITE_DIR / "index.html"
    if code != 0:
        tail = "\n".join(log.strip().splitlines()[-15:])
        return RenderResult("failed", f"quarto render exited with {code}; see {RENDER_LOG}\n{tail}")
    if not index.is_file():
        return RenderResult("failed", f"Quarto produced no {SITE_DIR}/index.html; see {RENDER_LOG}")
    return RenderResult("rendered", None, f"{SITE_DIR}/index.html")
