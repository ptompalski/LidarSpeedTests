"""Descriptive statistics for repeated measurements (stdlib only).

Variability is reported as the sample standard deviation (``n - 1``), the
coefficient of variation and the quartiles (``statistics.quantiles`` with the
``inclusive`` method, which equals R's default type 7). A single sample has no
variability: those fields are ``None``, never zero.
"""

from __future__ import annotations

import math
import statistics
from collections.abc import Iterable
from typing import Any

DESCRIBE_FIELDS = ("n", "mean", "sd", "cv", "median", "q1", "q3", "min", "max")


def finite(values: Iterable[Any]) -> list[float]:
    """Finite numeric values; ``None``, booleans, NaN and infinities are dropped."""
    out = []
    for value in values:
        if value is None or isinstance(value, bool):
            continue
        number = float(value)
        if math.isfinite(number):
            out.append(number)
    return out


def describe(values: Iterable[Any]) -> dict[str, Any]:
    """Count, mean, sample SD, CV, median, quartiles and range of finite values."""
    data = finite(values)
    n = len(data)
    result: dict[str, Any] = dict.fromkeys(DESCRIBE_FIELDS)
    result["n"] = n
    if not n:
        return result
    mean = statistics.fmean(data)
    result.update(mean=mean, median=statistics.median(data), min=min(data), max=max(data))
    if n >= 2:
        sd = statistics.stdev(data)
        q1, _, q3 = statistics.quantiles(data, n=4, method="inclusive")
        result.update(sd=sd, cv=sd / mean if mean else None, q1=q1, q3=q3)
    return result


def mean_or_none(values: Iterable[Any]) -> float | None:
    """Mean of finite values, or ``None`` when there are none."""
    data = finite(values)
    return statistics.fmean(data) if data else None


def max_or_none(values: Iterable[Any]) -> float | None:
    """Maximum of finite values, or ``None`` when there are none."""
    data = finite(values)
    return max(data) if data else None
