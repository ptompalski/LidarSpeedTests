"""Integration test of bridges/r/export_petawawa_receipts.R (base R only; skipped without R)."""

from __future__ import annotations

import json
import shutil
import subprocess
from pathlib import Path

import pytest

from lidarbench.datasets import legacy_petawawa

REPO = Path(__file__).resolve().parents[2]
EXPORTER = REPO / "bridges" / "r" / "export_petawawa_receipts.R"
RSCRIPT = shutil.which("Rscript")

pytestmark = [
    pytest.mark.integration,
    pytest.mark.skipif(RSCRIPT is None, reason="Rscript not installed"),
]

MAKE_RECEIPTS = r"""
args <- commandArgs(TRUE); root <- args[1]
dir.create(file.path(root, "download"), recursive = TRUE)
dir.create(file.path(root, "full", "repaired"), recursive = TRUE)
dir.create(file.path(root, "full", "prepared", "1_org", "tile_1000_density_1"), recursive = TRUE)
policy <- list(version = "fresh-petawawa-v4", repair = "documented repair")
saveRDS(list(config = list(archive_url = "https://x/a.zip", archive_md5 = NULL, crs = 2959L,
  vertical_epsg = 5713L, metadata_confirmation = "tester", seed = 1L,
  executables = c(sort = "lassort64", index = "lasindex64"), tool_license = "yes"),
  policy = list(version = "fresh-petawawa-v3")), file.path(root, "fresh-root.rds"))
saveRDS(list(url = "https://x/a.zip", md5 = "a", bytes = 10, authenticity = "none"),
  file.path(root, "download", "download.rds"))
versions <- list(sort = list(status = 0L, stdout = "",
  stderr = "LAStools lassort (by info@rapidlasso.de) version 260914 (non-profit)\n"))
saveRDS(list(policy = list(version = "fresh-petawawa-v3"), archive_md5 = "a",
  members = "PRF/t.laz", tools_md5 = "m", versions = versions, implementation = "i"),
  file.path(root, "full", "stage-identity.rds"))
saveRDS(list(method = "fresh-petawawa-v4", densities = 1,
  config = list(seed = 1L, source_citation = "c", license = "l", crs = "WKT",
    repair_provenance = "r", source_root = "/secret/path"),
  sources = data.frame(tile = "t", path = "/secret/path/t.laz", md5 = "s"),
  parameters = policy, tools = data.frame(name = "sort", path = "/opt/bin/lassort64", md5 = "m"),
  versions = versions, implementation = "i",
  runtime = list(R = "4.6.1", platform = "p", lidR = "4", sf = "1", native = c(GDAL = "3")),
  fresh = list(archive_md5 = "a", implementation = "f", repairs_md5 = "rr")),
  file.path(root, "full", "prepared", "preparation.rds"))
saveRDS(list(status = "data-qc-passed", policy = policy, archive_md5 = "a",
  preparation_md5 = "p"), file.path(root, "full", "ready.rds"))
saveRDS(list(tile = "t", archive_member = "PRF/t.laz", raw_md5 = "raw", md5 = "s",
  source_filter = list(filter = "-drop_synthetic", source_points = 5L),
  qc = list(points = 4L, area_m2 = 1), actions = data.frame(action = c("drop_duplicate_XYZ",
  "raise_return_count", "raise_return_count"), source_Z = c(1, 2, 3))),
  file.path(root, "full", "repaired", "t.laz.rds"))
saveRDS(list(md5 = c("laz", "lax"), source_md5 = "s", seed = 7L,
  summary = list(points = 4L), index_validation = list(status = "passed")),
  file.path(root, "full", "prepared", "1_org", "tile_1000_density_1", "t.laz.rds"))
"""


def test_exporter_reads_receipts_without_packages(tmp_path):
    root = tmp_path / "fresh root"
    script = tmp_path / "make.R"
    script.write_text(MAKE_RECEIPTS, encoding="utf-8")
    subprocess.run([RSCRIPT, "--vanilla", str(script), str(root)], check=True)
    output = tmp_path / "export.json"
    result = subprocess.run(
        [
            RSCRIPT,
            "--vanilla",
            str(EXPORTER),
            "--root",
            str(root),
            "--stage",
            "full",
            "--output",
            str(output),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    text = output.read_text(encoding="utf-8")
    assert "non-profit" not in text and "/secret/path" not in text and "/opt/bin" not in text
    export, _ = legacy_petawawa.load_export(output)
    assert export["preparation"]["tool_versions"]["sort"] == {"status": 0, "build": "260914"}
    assert export["preparation"]["tools"][0] == {"name": "sort", "file": "lassort64", "md5": "m"}
    assert export["repairs"][0]["actions"] == {"drop_duplicate_XYZ": 1, "raise_return_count": 2}
    assert export["repairs"][0]["negative_source_z"] == 0
    assert export["repairs"][0]["policy_version"] is None
    assert export["tiles"] == [
        {
            "density": 1,
            "tile": "t",
            "path": "1_org/tile_1000_density_1/t.laz",
            "laz_md5": "laz",
            "lax_md5": "lax",
            "source_md5": "s",
            "seed": 7,
            "points": 4,
            "index_validation": "passed",
        }
    ]
    assert export["root"]["tool_license_confirmed"] is True
    assert export["negative_z_transition"] is None and export["parallel_transition"] is False

    again = subprocess.run(
        [
            RSCRIPT,
            "--vanilla",
            str(EXPORTER),
            "--root",
            str(root),
            "--stage",
            "full",
            "--output",
            str(output),
        ],
        capture_output=True,
        text=True,
    )
    assert again.returncode == 2 and "refusing to overwrite" in again.stderr
    missing = subprocess.run(
        [
            RSCRIPT,
            "--vanilla",
            str(EXPORTER),
            "--root",
            str(tmp_path / "none"),
            "--stage",
            "full",
            "--output",
            str(tmp_path / "x.json"),
        ],
        capture_output=True,
        text=True,
    )
    assert missing.returncode == 1 and "missing receipt" in missing.stderr
    json.loads(text)
