"""Fake adapters demonstrating each launch style without real LiDAR tools."""

from __future__ import annotations

import json
import os
from pathlib import Path

from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters import (
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
    WorkloadSupport,
    thread_cap_env,
)
from lidarbench.execution.adapters.launchers import native_argv, resolve_executable, rscript_argv


class FakeRscriptAdapter(ToolAdapter):
    """Rscript-style adapter: script + JSON invocation file + worker processes."""

    adapter_id = "fake-rscript"
    display_name = "Fake Rscript adapter"

    def __init__(self, rscript: str = "Rscript-not-installed-lidarbench", script: str = "") -> None:
        self.rscript = rscript
        self.script = Path(script) if script else Path("/opt/bridge/runner.R").absolute()

    def probe(self, platform_info: PlatformInfo) -> Availability:
        exe = resolve_executable(self.rscript)
        if exe is None:
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED, f"Rscript not found: {self.rscript}"
            )
        return Availability.ok({"rscript": exe})

    def identify(self) -> ToolIdentity:
        return ToolIdentity(tool="fake-r", version="0.0.1")

    def workload_support(self) -> list[WorkloadSupport]:
        return [
            WorkloadSupport(
                "demo.metrics",
                ImplementationStatus.RUNNABLE,
                threading=ThreadingSupport(ThreadingModel.WORKER_PROCESSES, 1, None),
            )
        ]

    def build_command(self, invocation: Invocation) -> CommandSpec:
        exe = resolve_executable(self.rscript) or Path(self.rscript).absolute()
        payload = {
            "inputs": [os.fspath(p) for p in invocation.inputs],
            "output_dir": os.fspath(invocation.output_dir),
            "workers": invocation.threads,
        }
        cfg = ConfigFile("invocation.json", json.dumps(payload, sort_keys=True))
        argv = rscript_argv(exe, self.script, invocation.config_path("invocation.json"))
        return CommandSpec(
            argv=argv,
            cwd=invocation.work_dir,
            env=thread_cap_env(1),
            config_files=(cfg,),
        )

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        return [ArtifactSpec(invocation.output_dir / "metrics.tif", ArtifactKind.RASTER)]


class FakeWindowsOnlyAdapter(ToolAdapter):
    """Native-executable adapter for a tool that only exists on Windows."""

    adapter_id = "fake-windows-only"
    display_name = "Fake Windows-only native tool"

    def probe(self, platform_info: PlatformInfo) -> Availability:
        if platform_info.system != "Windows":
            return Availability.unavailable(
                UnavailableReason.PLATFORM_UNSUPPORTED,
                f"vendor ships no {platform_info.system} build",
            )
        exe = resolve_executable("fakewintool.exe")
        if exe is None:
            return Availability.unavailable(
                UnavailableReason.NOT_INSTALLED, "fakewintool.exe not on PATH"
            )
        return Availability.ok({"main": exe})

    def identify(self) -> ToolIdentity:
        return ToolIdentity(tool="fakewintool", version=None)

    def workload_support(self) -> list[WorkloadSupport]:
        return [
            WorkloadSupport(
                "demo.metrics",
                ImplementationStatus.DIAGNOSTIC_ONLY,
                reason="native statistic conventions unverified",
                threading=ThreadingSupport(ThreadingModel.NATIVE_THREADS, 1, 8),
            ),
            WorkloadSupport(
                "demo.tin",
                ImplementationStatus.UNSUPPORTED,
                reason="tool has no TIN rasterizer",
            ),
        ]

    def build_command(self, invocation: Invocation) -> CommandSpec:
        exe = Path("C:/Tools/fakewintool.exe")
        argv = native_argv(
            exe,
            "-i",
            *invocation.inputs,
            "-odir",
            invocation.output_dir,
            "-cores",
            invocation.threads,
        )
        return CommandSpec(argv=argv, cwd=invocation.work_dir)

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        return []


class FakePdalAdapter(ToolAdapter):
    """Config-file adapter in the style of ``pdal pipeline <pipeline.json>``."""

    adapter_id = "fake-pdal"
    display_name = "Fake PDAL-style pipeline tool"

    def __init__(self, executable: Path | None = None) -> None:
        self.executable = executable or Path("/usr/local/bin/pdal").absolute()

    def probe(self, platform_info: PlatformInfo) -> Availability:
        return Availability.ok({"pdal": self.executable})

    def identify(self) -> ToolIdentity:
        return ToolIdentity(tool="fake-pdal", version="9.9.9", details={"driver": "fake"})

    def workload_support(self) -> list[WorkloadSupport]:
        return [
            WorkloadSupport("demo.metrics", ImplementationStatus.RUNNABLE),
            WorkloadSupport(
                "demo.tin",
                ImplementationStatus.BLOCKED,
                reason="awaiting confirmation of triangulation semantics",
            ),
        ]

    def build_command(self, invocation: Invocation) -> CommandSpec:
        pipeline = [
            *[{"type": "readers.las", "filename": os.fspath(p)} for p in invocation.inputs],
            {
                "type": "writers.gdal",
                "filename": os.fspath(invocation.output_dir / "metrics.tif"),
                "resolution": invocation.parameters.get("resolution", 20),
            },
        ]
        cfg = ConfigFile("pipeline.json", json.dumps({"pipeline": pipeline}, indent=2))
        argv = native_argv(self.executable, "pipeline", invocation.config_path("pipeline.json"))
        return CommandSpec(argv=argv, cwd=invocation.work_dir, config_files=(cfg,))

    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        return [ArtifactSpec(invocation.output_dir / "metrics.tif", ArtifactKind.RASTER)]

    def invocation_metadata(self, invocation: Invocation) -> dict[str, str]:
        return {"pipeline_file": "pipeline.json"}
