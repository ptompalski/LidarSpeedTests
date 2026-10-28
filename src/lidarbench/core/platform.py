"""Platform detection and capability probes."""

import platform
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

import psutil


@dataclass(frozen=True)
class PlatformInfo:
    """Information about the current platform."""

    system: Literal["Linux", "Windows", "Darwin"]
    """Operating system: Linux, Windows, or Darwin (macOS)."""

    platform_release: str
    """OS release/version string."""

    architecture: str
    """CPU architecture (x86_64, arm64, etc.)."""

    python_version: str
    """Python version string."""

    cpu_count: int
    """Logical CPU count."""

    total_memory_bytes: int
    """Total system memory in bytes."""


def get_platform_info() -> PlatformInfo:
    """Get platform and hardware information.
    
    Returns:
        PlatformInfo with current system details.
    """
    return PlatformInfo(
        system=platform.system(),
        platform_release=platform.release(),
        architecture=platform.machine(),
        python_version=platform.python_version(),
        cpu_count=psutil.cpu_count(logical=True) or 1,
        total_memory_bytes=psutil.virtual_memory().total,
    )


def is_windows() -> bool:
    """Check if running on Windows."""
    return platform.system() == "Windows"


def is_linux() -> bool:
    """Check if running on Linux."""
    return platform.system() == "Linux"


def is_macos() -> bool:
    """Check if running on macOS."""
    return platform.system() == "Darwin"


def get_executable_extension() -> str:
    """Get the platform-specific executable extension (.exe on Windows, empty otherwise)."""
    return ".exe" if is_windows() else ""


def get_path_separator() -> str:
    """Get the platform-specific PATH separator (: on Unix, ; on Windows)."""
    return ";" if is_windows() else ":"


def which(name: str, paths: list[str] | None = None) -> Path | None:
    """Find an executable in PATH or a specific list of paths.
    
    Cross-platform compatible replacement for shutil.which that returns Path.
    
    Args:
        name: Executable name (with or without platform extension).
        paths: List of directories to search (defaults to PATH environment variable).
    
    Returns:
        Path to the executable if found, None otherwise.
    """
    import os

    # Normalize the executable name for the current platform
    if is_windows() and not name.endswith(".exe"):
        exe_name = name + ".exe"
    else:
        exe_name = name

    # Determine search paths
    if paths is None:
        path_env = os.environ.get("PATH", "")
        paths = path_env.split(os.pathsep) if path_env else []

    # Search for the executable
    for path_str in paths:
        if not path_str:
            continue
        candidate = Path(path_str) / exe_name
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return candidate

    return None
