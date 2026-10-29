"""lidR adapter: legacy lidR workloads through the R bridge (``Rscript``).

Threading follows the legacy harness: ``future`` multisession with one worker
process per requested thread, ``lidR::set_lidr_threads(1)`` and data.table and
BLAS/OpenMP capped at one thread (``benchmark_with_resources()``).
"""

from __future__ import annotations

from types import MappingProxyType

from lidarbench.execution.adapters.base import ImplementationStatus, ThreadingModel, WorkloadSupport
from lidarbench.execution.adapters.rscript import RBridgeAdapter, RWorkload

_NOT_PORTED = "not yet available through the Python orchestrator: no workloads-v1 contract"


class LidRAdapter(RBridgeAdapter):
    """lidR via ``Rscript`` and the legacy catalog configuration (``prepare_input()``)."""

    adapter_id = "lidr"
    display_name = "lidR (R package via Rscript)"
    r_package = "lidR"
    contract_tool = "lidR"
    required_packages = ("lidR", "future", "data.table", "jsonlite")
    threading_model = ThreadingModel.WORKER_PROCESSES
    workloads = MappingProxyType(
        {
            "pixel_metrics_1a": RWorkload(
                "pixel_metrics_1a",
                call=(
                    "lidR::pixel_metrics(las = ctg, ~list(z_mean = mean(Z)), res = 20, "
                    "start = c(0, 0))"
                ),
                buffer=20.0,
            ),
            "generate_DSM1": RWorkload(
                "generate_DSM1",
                call=(
                    "lidR::rasterize_canopy(ctg, res = 1, algorithm = p2r(subcircle = 0, "
                    "na.fill = NULL))"
                ),
                buffer=20.0,
            ),
        }
    )
    other_workloads = (
        *(
            WorkloadSupport(w, ImplementationStatus.UNSUPPORTED, reason=_NOT_PORTED)
            for w in (
                "normalization",
                "pixel_metrics_2",
                "generate_DEM",
                "generate_DSM2",
                "tree_detection",
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
