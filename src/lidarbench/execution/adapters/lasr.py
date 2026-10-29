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

from lidarbench.execution.adapters.base import ImplementationStatus, ThreadingModel, WorkloadSupport
from lidarbench.execution.adapters.rscript import RBridgeAdapter, RWorkload

_NOT_PORTED = "not yet available through the Python orchestrator: no workloads-v1 contract"
_TIN_NORMALIZATION = (
    "pinned lasR 0.21.2 TIN normalization produces NA at ground hull boundaries and corrupt "
    "LAZ elevations (blocked in the legacy harness)"
)
_UNSTABLE_TIN = (
    "pinned lasR 0.21.2 first-return TIN is non-repeatable near duplicate XY with different Z "
    "(blocked in the legacy harness)"
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
        }
    )
    other_workloads = (
        *(
            WorkloadSupport(w, ImplementationStatus.BLOCKED, reason=_TIN_NORMALIZATION)
            for w in ("normalization", "complex_task1", "complex_task2")
        ),
        WorkloadSupport("generate_DSM2", ImplementationStatus.BLOCKED, reason=_UNSTABLE_TIN),
        *(
            WorkloadSupport(w, ImplementationStatus.UNSUPPORTED, reason=_NOT_PORTED)
            for w in ("pixel_metrics_2", "generate_DEM", "tree_detection")
        ),
    )
