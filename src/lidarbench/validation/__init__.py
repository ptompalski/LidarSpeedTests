"""Independent output validation (artifacts, structure, numerical references).

Importing this package needs only the standard library. Decoding rasters and
point clouds and computing references use the optional ``validation`` extra
(numpy, rasterio, laspy); when it is missing the affected level is recorded as
``unavailable``. See ``design/VALIDATION.md``.
"""

from lidarbench.validation.comparability import (
    ComparisonSubject,
    GateResult,
    NotComparableError,
    comparison_gate,
    require_comparable,
    speedup,
)
from lidarbench.validation.pipeline import (
    REPORT_SCHEMA,
    ValidationReport,
    ValidationRequest,
    validate_outputs,
)
from lidarbench.validation.policies import Policies
from lidarbench.validation.status import (
    AttemptVerdicts,
    Comparability,
    ComparabilityPolicy,
    Level,
    LevelVerdict,
    Outcome,
    ReportMode,
    Status,
)

__all__ = [
    "REPORT_SCHEMA",
    "AttemptVerdicts",
    "Comparability",
    "ComparabilityPolicy",
    "ComparisonSubject",
    "GateResult",
    "Level",
    "LevelVerdict",
    "NotComparableError",
    "Outcome",
    "Policies",
    "ReportMode",
    "Status",
    "ValidationReport",
    "ValidationRequest",
    "comparison_gate",
    "require_comparable",
    "speedup",
    "validate_outputs",
]
