"""Discovery and registration of tool adapters.

Adapters are discovered through the ``lidarbench.adapters`` entry-point group.
A third-party package adds an adapter by declaring, in its own ``pyproject.toml``::

    [project.entry-points."lidarbench.adapters"]
    mytool = "mypackage.adapter:MyToolAdapter"

No orchestrator code changes are needed. A plugin that fails to load is recorded
as an :class:`AdapterLoadFailure`; it never prevents other adapters from loading.
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from importlib.metadata import EntryPoint, entry_points

from lidarbench.execution.adapters.base import ADAPTER_ID_PATTERN, ToolAdapter

ENTRY_POINT_GROUP = "lidarbench.adapters"


@dataclass(frozen=True)
class AdapterLoadFailure:
    """An adapter entry point that could not be loaded or registered."""

    name: str
    target: str
    error: str


class AdapterRegistry:
    """Mapping from adapter ID to adapter class."""

    def __init__(self) -> None:
        self._adapters: dict[str, type[ToolAdapter]] = {}
        self._failures: list[AdapterLoadFailure] = []

    @classmethod
    def discover(cls, eps: Iterable[EntryPoint] | None = None) -> AdapterRegistry:
        """Build a registry from installed entry points.

        Args:
            eps: Entry points to load; defaults to the installed
                ``lidarbench.adapters`` group.
        """
        registry = cls()
        if eps is None:
            eps = entry_points(group=ENTRY_POINT_GROUP)
        for ep in sorted(eps, key=lambda e: e.name):
            try:
                loaded = ep.load()
            except Exception as exc:
                registry._fail(ep.name, ep.value, f"{type(exc).__name__}: {exc}")
                continue
            if not (isinstance(loaded, type) and issubclass(loaded, ToolAdapter)):
                registry._fail(ep.name, ep.value, "target is not a ToolAdapter subclass")
                continue
            if getattr(loaded, "adapter_id", None) != ep.name:
                registry._fail(
                    ep.name,
                    ep.value,
                    f"entry-point name must equal adapter_id "
                    f"({getattr(loaded, 'adapter_id', None)!r})",
                )
                continue
            try:
                registry.register(loaded)
            except ValueError as exc:
                registry._fail(ep.name, ep.value, str(exc))
        return registry

    def _fail(self, name: str, target: str, error: str) -> None:
        self._failures.append(AdapterLoadFailure(name, target, error))

    def register(self, adapter_cls: type[ToolAdapter]) -> None:
        """Register an adapter class explicitly.

        Raises:
            ValueError: The ID is invalid or already registered, the class is
                abstract, or it lacks a display name.
        """
        adapter_id = getattr(adapter_cls, "adapter_id", None)
        if not isinstance(adapter_id, str) or not ADAPTER_ID_PATTERN.match(adapter_id):
            raise ValueError(f"invalid adapter_id {adapter_id!r} on {adapter_cls.__name__}")
        if not isinstance(getattr(adapter_cls, "display_name", None), str):
            raise ValueError(f"{adapter_cls.__name__} has no display_name")
        if getattr(adapter_cls, "__abstractmethods__", None):
            missing = ", ".join(sorted(adapter_cls.__abstractmethods__))
            raise ValueError(f"{adapter_cls.__name__} is abstract (missing: {missing})")
        existing = self._adapters.get(adapter_id)
        if existing is not None and existing is not adapter_cls:
            raise ValueError(
                f"adapter_id {adapter_id!r} already registered by {existing.__qualname__}"
            )
        self._adapters[adapter_id] = adapter_cls

    def ids(self) -> list[str]:
        """Sorted IDs of registered adapters."""
        return sorted(self._adapters)

    def __contains__(self, adapter_id: object) -> bool:
        return adapter_id in self._adapters

    def adapter_class(self, adapter_id: str) -> type[ToolAdapter]:
        """Return the registered class for ``adapter_id``.

        Raises:
            KeyError: Unknown adapter ID.
        """
        try:
            return self._adapters[adapter_id]
        except KeyError:
            known = ", ".join(self.ids()) or "none"
            raise KeyError(f"unknown adapter {adapter_id!r} (registered: {known})") from None

    def create(self, adapter_id: str) -> ToolAdapter:
        """Instantiate the adapter registered as ``adapter_id``."""
        return self.adapter_class(adapter_id)()

    @property
    def failures(self) -> tuple[AdapterLoadFailure, ...]:
        """Entry points that could not be loaded."""
        return tuple(self._failures)
