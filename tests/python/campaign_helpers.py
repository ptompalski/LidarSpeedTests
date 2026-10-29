"""Helpers for campaign tests: a scripted adapter and temporary campaign layouts."""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.config.campaign import load_config
from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters.base import (
    ArtifactKind,
    ArtifactSpec,
    Availability,
    CommandSpec,
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    ThreadingSupport,
    ToolAdapter,
    ToolIdentity,
    WorkloadSupport,
)
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.adapters.synthetic import SyntheticAdapter

_SCRIPTS = {
    "scripted.ok": "import pathlib,sys; pathlib.Path(sys.argv[1], 'ok.txt').write_text('ok')",
    "scripted.fail": "import sys; sys.exit(3)",
    "scripted.sleep": "import time; time.sleep(30)",
    "scripted.nooutput": "pass",
}


class ScriptedAdapter(ToolAdapter):
    """Runs tiny Python snippets: success, failure, sleep, missing output."""

    adapter_id = "scripted"
    display_name = "Scripted test adapter"

    def probe(self, platform_info: PlatformInfo) -> Availability:
        return Availability.ok({"python": Path(sys.executable)})

    def identify(self) -> ToolIdentity:
        return ToolIdentity("scripted", "1.0", {"python": Path(sys.executable)})

    def workload_support(self) -> list[WorkloadSupport]:
        threads = ThreadingSupport(ThreadingModel.NATIVE_THREADS, 1, None)
        return [
            WorkloadSupport(w, ImplementationStatus.RUNNABLE, threading=threads) for w in _SCRIPTS
        ]

    def build_command(self, invocation: Invocation) -> CommandSpec:
        code = _SCRIPTS[invocation.workload_id]
        argv = (sys.executable, "-c", code, str(invocation.output_dir))
        return CommandSpec(argv=argv, cwd=invocation.work_dir)

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        if invocation.workload_id in ("scripted.ok", "scripted.nooutput"):
            return [ArtifactSpec(invocation.output_dir / "ok.txt", ArtifactKind.OTHER)]
        return []


def registry() -> AdapterRegistry:
    reg = AdapterRegistry()
    reg.register(SyntheticAdapter)
    reg.register(ScriptedAdapter)
    return reg


def no_git() -> dict[str, Any]:
    return {"available": False, "reason": "test"}


def write_layout(tmp_path: Path) -> Path:
    """Create data/, scratch/ and state/ under ``tmp_path``; return the base dir."""
    base = tmp_path / "bench root"
    (base / "data").mkdir(parents=True)
    (base / "scratch").mkdir()
    (base / "state").mkdir()
    (base / "data" / "tile 1.laz").write_bytes(b"abc")
    (base / "data" / "tile2.laz").write_bytes(b"defg")
    (base / "data" / "tile2.lax").write_bytes(b"index")
    return base


def write_config(
    base: Path,
    *,
    campaign_id: str = "test-campaign",
    workloads: str = '[[workloads]]\nid = "synthetic.checksum"\n',
    tools: str = '[[tools]]\nid = "synthetic"\n',
    cpu_counts: str = "[1]",
    scheduling: str = "repetitions = 2\nwarmups = 1\ncooldown_s = 0",
    monitoring: str = "enabled = false",
    extra: str = "",
    name: str = "campaign.toml",
) -> Path:
    text = "\n".join(
        [
            'schema = "lidarbench.campaign-config/1"',
            f'campaign_id = "{campaign_id}"',
            'workspace = "workspace"',
            f"cpu_counts = {cpu_counts}",
            'validation_mode = "performance-only"',
            extra,
            "[scheduling]",
            scheduling,
            "[monitoring]",
            monitoring,
            "[scratch]",
            'roots = { local = "scratch" }',
            "[[datasets]]",
            'id = "tiny"',
            'path = "data"',
            'include = ["*.laz"]',
            "",
        ]
    )
    path = base / name
    path.write_text(text + workloads + "\n" + tools, encoding="utf-8")
    return path


def make_runner(config_path: Path, base: Path, **kwargs: Any) -> CampaignRunner:
    options = kwargs.pop("options", RunOptions())
    kwargs.setdefault("registry", registry())
    kwargs.setdefault("state_dir", base / "state")
    kwargs.setdefault("git_info", no_git)
    kwargs.setdefault("report", lambda line: None)
    kwargs.setdefault("handle_signals", False)
    return CampaignRunner(load_config(config_path), options=options, **kwargs)
