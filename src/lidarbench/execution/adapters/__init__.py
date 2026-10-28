"""Tool adapters: plugins that describe how a tool runs a workload.

See ``design/ADAPTERS.md`` for the contract. Adapters are discovered through the
``lidarbench.adapters`` entry-point group (:class:`AdapterRegistry`).

The ``cli``, ``lastools``, ``rscript`` and ``pylib`` modules are placeholders
for later phases and are not registered.
"""

from lidarbench.execution.adapters.base import (
    ADAPTER_SCHEMA,
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ConfigFile,
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    UnavailableReason,
    UnsupportedCombinationError,
    WorkloadSupport,
    argv_value,
    thread_cap_env,
)
from lidarbench.execution.adapters.registry import (
    ENTRY_POINT_GROUP,
    AdapterLoadFailure,
    AdapterRegistry,
)

__all__ = [
    "ADAPTER_SCHEMA",
    "ENTRY_POINT_GROUP",
    "AdapterLoadFailure",
    "AdapterRegistry",
    "ArtifactKind",
    "ArtifactSpec",
    "Availability",
    "CommandSpec",
    "ConfigFile",
    "ImplementationStatus",
    "Invocation",
    "ThreadingModel",
    "ThreadingSupport",
    "ToolAdapter",
    "ToolIdentity",
    "UnavailableReason",
    "UnsupportedCombinationError",
    "WorkloadSupport",
    "argv_value",
    "thread_cap_env",
]
