"""lidR adapter: legacy lidR workloads through the R bridge (``Rscript``).

Threading follows the legacy harness: ``future`` workers, one per requested
thread, ``lidR::set_lidr_threads(1)`` and data.table and BLAS/OpenMP capped at
one thread (``benchmark_with_resources()``). The ``future`` backend is the tool
option ``future_backend``: ``"multisession"`` (default, the legacy harness and
lidR's documented example) or ``"mirai"`` (``future.mirai::mirai_multisession``;
its daemons get the same start-up before the timed call).

Option ``accept_known_tin_fallback_warning`` (default false) accepts, for
``generate_DEM`` only, the ``tin()`` warning that cells too far from any ground point
were filled from the nearest ground point; any other R warning still fails.
"""

from __future__ import annotations

import re
from collections.abc import Mapping
from types import MappingProxyType
from typing import Any

from lidarbench.execution.adapters.base import (
    ImplementationStatus,
    Invocation,
    MetadataValue,
    ThreadingModel,
    ToolIdentity,
    WorkloadSupport,
)
from lidarbench.execution.adapters.rscript import KnownWarning, RBridgeAdapter, RWorkload

_NOT_PORTED = "not yet available through the Python orchestrator: no workloads-v1 contract"
FUTURE_BACKENDS = MappingProxyType({"multisession": (), "mirai": ("future.mirai", "mirai")})
"""Supported ``future_backend`` values and the extra R packages each needs."""
TIN_FALLBACK_POLICY = "lidr.accept_known_tin_fallback_warning"
TIN_FALLBACK = KnownWarning(
    "lidr_tin_nearest_neighbour_fallback",
    TIN_FALLBACK_POLICY,
    re.compile(
        r"Interpolation of [0-9]+ points failed because they are too far from ground points\. "
        r"Nearest neighbour was used but interpolation is weak for those points"
    ),
)
"""lidR ``tin()``: cells too far from any ground point get the nearest ground point's Z."""
TIN_FALLBACK_WORKLOADS = frozenset({"generate_DEM"})


class LidRAdapter(RBridgeAdapter):
    """lidR via ``Rscript`` and the legacy catalog configuration (``prepare_input()``).

    Extra option: ``future_backend`` (``"multisession"`` default, or ``"mirai"``).
    """

    adapter_id = "lidr"
    display_name = "lidR (R package via Rscript)"
    r_package = "lidR"
    contract_tool = "lidR"
    required_packages: tuple[str, ...] = ("lidR", "future", "data.table", "jsonlite")
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
            "generate_DEM": RWorkload(
                "generate_DEM",
                call=(
                    "lidR::rasterize_terrain(las = ctg, res = 1, algorithm = tin(), "
                    "use_class = c(2L, 9L), shape = 'convex')"
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

    def __init__(
        self,
        *,
        future_backend: str = "multisession",
        accept_known_tin_fallback_warning: bool = False,
        **options: Any,
    ) -> None:
        if not isinstance(future_backend, str) or future_backend not in FUTURE_BACKENDS:
            raise TypeError(
                f"option 'future_backend' must be one of {sorted(FUTURE_BACKENDS)}, "
                f"got {future_backend!r}"
            )
        if not isinstance(accept_known_tin_fallback_warning, bool):
            raise TypeError(
                "option 'accept_known_tin_fallback_warning' must be bool, "
                f"got {accept_known_tin_fallback_warning!r}"
            )
        super().__init__(**options)
        self.future_backend = future_backend
        self.accept_known_tin_fallback_warning = accept_known_tin_fallback_warning
        self.required_packages = (
            *type(self).required_packages,
            *FUTURE_BACKENDS[future_backend],
        )

    def known_warning(self, invocation: Invocation) -> KnownWarning | None:
        """The lidR TIN nearest-neighbour fallback warning, when opted in (DEM only)."""
        if (
            self.accept_known_tin_fallback_warning
            and invocation.workload_id in TIN_FALLBACK_WORKLOADS
        ):
            return TIN_FALLBACK
        return None

    def identify(self) -> ToolIdentity:
        """The base identity plus the ``future`` backend and the warning policy."""
        identity = super().identify()
        return ToolIdentity(
            tool=identity.tool,
            version=identity.version,
            executables=identity.executables,
            details={
                **identity.details,
                "future_backend": self.future_backend,
                "accept_known_tin_fallback_warning": self.accept_known_tin_fallback_warning,
            },
        )

    def bridge_invocation(self, invocation: Invocation) -> dict[str, Any]:
        """The base document plus ``lidr_future_backend``."""
        return {**super().bridge_invocation(invocation), "lidr_future_backend": self.future_backend}

    def invocation_metadata(self, invocation: Invocation) -> Mapping[str, MetadataValue]:
        """The base metadata plus the ``future`` backend and the accepted-warning policy."""
        known = self.known_warning(invocation)
        return {
            **super().invocation_metadata(invocation),
            "future_backend": self.future_backend,
            "accepted_exception_policy": known.policy if known else None,
        }
