"""lasR adapter: legacy lasR workloads through the R bridge (``Rscript``).

Threading follows the legacy harness: ``set_parallel_strategy(concurrent_files(n))``
with ``OMP_NUM_THREADS``/``OMP_THREAD_LIMIT`` at the thread budget and every other
math/BLAS pool capped at one thread (``benchmark_with_resources()``).

lasR indexes unindexed files by writing ``.lax`` files next to them, which would
modify the protected input tree. The bridge therefore refuses inputs without a
nonempty ``.lax`` index, as the legacy harness does for declared inputs.
"""

from __future__ import annotations

from types import MappingProxyType

from lidarbench.execution.adapters.base import (
    ArtifactKind,
    ImplementationStatus,
    ThreadingModel,
    WorkloadSupport,
)
from lidarbench.execution.adapters.rscript import RBridgeAdapter, RWorkload

_NOT_PORTED = "not yet available through the Python orchestrator: no workloads-v1 contract"
_TIN_NORMALIZATION = (
    "lasR <= 0.21.3 TIN normalization produces NA at ground hull boundaries and corrupt "
    "LAZ elevations (blocked in the legacy harness; no contract yet)"
)


class LasRAdapter(RBridgeAdapter):
    """lasR via ``Rscript``; per-file pipelines with a 20 m buffer and ``chunk = 0``."""

    adapter_id = "lasr"
    display_name = "lasR (R package via Rscript)"
    r_package = "lasR"
    contract_tool = "lasR"
    required_packages = ("lasR", "data.table", "jsonlite")
    threading_model = ThreadingModel.CONCURRENT_FILES
    native_thread_variables = ("OMP_NUM_THREADS", "OMP_THREAD_LIMIT")
    workloads = MappingProxyType(
        {
            "pixel_metrics_1a": RWorkload(
                "pixel_metrics_1a",
                call=(
                    "exec(pipeline = rasterize(20, 'z_mean', ofile = fout), on = f, "
                    "with = list(buffer = 20, chunk = 0, progress = FALSE))"
                ),
                buffer=20.0,
            ),
            "generate_DSM1": RWorkload(
                "generate_DSM1",
                call=(
                    "exec(pipeline = dsm(1, tin = FALSE, ofile = fout), on = f, "
                    "with = list(buffer = 20, chunk = 0, progress = FALSE))"
                ),
                buffer=20.0,
            ),
            "pixel_metrics_2": RWorkload(
                "pixel_metrics_2",
                call=(
                    "exec(pipeline = rasterize(20, c('z_max','z_mean','z_sd','z_above2','z_p5',"
                    "'z_p25','z_p50','z_p75','z_p95'), ofile = fout), on = f, "
                    "with = list(buffer = 20, chunk = 0, progress = FALSE))"
                ),
                buffer=20.0,
            ),
            # workloads-v8: the legacy candidate (blocked there). Not repeatable near
            # first returns sharing XY with different Z; structural validation only.
            "generate_DSM2": RWorkload(
                "generate_DSM2",
                call=(
                    "exec(pipeline = dsm(1, tin = TRUE, ofile = fout), on = f, "
                    "with = list(buffer = 20, chunk = 0, progress = FALSE))"
                ),
                buffer=20.0,
            ),
            "generate_DEM": RWorkload(
                "generate_DEM",
                call=(
                    "exec(pipeline = dtm(res = 1, ofile = fout), on = f, "
                    "with = list(buffer = 20, chunk = 0, progress = FALSE))"
                ),
                buffer=20.0,
            ),
            # workloads-v9: runnable with the pinned lasR 0.21.4 (r-lidar/lasR#355 fixed the
            # NaN heights that made it diagnostic_only in workloads-v7 and workloads-v8).
            "normalization": RWorkload(
                "normalization",
                call=(
                    "exec(pipeline = normalize() + write_las(fout), on = f, "
                    "with = list(buffer = 20, chunk = 0, progress = FALSE))"
                ),
                buffer=20.0,
                output_extension="laz",
                artifact_pattern="{stem}.laz",
                kind=ArtifactKind.POINT_CLOUD,
            ),
        }
    )
    other_workloads = (
        *(
            WorkloadSupport(w, ImplementationStatus.BLOCKED, reason=_TIN_NORMALIZATION)
            for w in ("complex_task1", "complex_task2")
        ),
        *(
            WorkloadSupport(w, ImplementationStatus.UNSUPPORTED, reason=_NOT_PORTED)
            for w in ("tree_detection",)
        ),
    )
