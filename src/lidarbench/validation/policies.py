"""Frozen opt-in acceptance policies for validation (default: strict)."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class Policies:
    """Frozen opt-in acceptance policies (default: strict).

    ``accept_known_extent_discrepancies`` permits a raster to extend outward by at
    most one cell per edge with the full expected core present. Such products can
    at best be ``accepted_exception``, never ``passed``.
    """

    accept_known_extent_discrepancies: bool = False

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {"accept_known_extent_discrepancies": self.accept_known_extent_discrepancies}
