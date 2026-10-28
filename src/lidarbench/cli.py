"""Command-line interface implementation."""

import argparse
import sys
from typing import Optional

import lidarbench


def add_subparsers(parser: argparse.ArgumentParser) -> None:
    """Add subcommands to the argument parser.
    
    Currently includes:
    - version: display version information
    - help: display help (implicit)
    
    Additional commands (init, preflight, run, resume, etc.) are placeholders
    and will be implemented in later phases.
    """
    subparsers = parser.add_subparsers(dest="command", help="command to run")

    # version command
    subparsers.add_parser(
        "version",
        help="display version information",
    )

    # Placeholder commands (not yet implemented; --help works but execution warns)
    subparsers.add_parser(
        "init",
        help="[PLACEHOLDER] initialize a new campaign workspace",
    )

    subparsers.add_parser(
        "preflight",
        help="[PLACEHOLDER] validate configuration and check tool availability",
    )

    subparsers.add_parser(
        "run",
        help="[PLACEHOLDER] execute a campaign",
    )

    subparsers.add_parser(
        "resume",
        help="[PLACEHOLDER] resume a previous campaign",
    )

    subparsers.add_parser(
        "report",
        help="[PLACEHOLDER] generate a report from a campaign",
    )

    subparsers.add_parser(
        "doctor",
        help="[PLACEHOLDER] diagnose environment and configuration",
    )


def main(args: Optional[list[str]] = None) -> int:
    """Main CLI entry point.
    
    Args:
        args: Command-line arguments (defaults to sys.argv[1:] if None)
    
    Returns:
        Exit code (0 on success, non-zero on failure)
    """
    parser = argparse.ArgumentParser(
        prog="lidarbench",
        description=(
            "Cross-platform LiDAR processing software benchmarking orchestration. "
            "Benchmark any LiDAR processing tool (lidR, lasR, LAStools, PDAL, etc.) "
            "natively on Linux, Windows and macOS."
        ),
    )

    parser.add_argument(
        "--version",
        action="version",
        version=f"%(prog)s {lidarbench.__version__}",
    )

    add_subparsers(parser)

    parsed = parser.parse_args(args)

    # Handle version as a top-level flag or as the version command
    if not parsed.command:
        # No command specified; show help
        parser.print_help()
        return 0

    if parsed.command == "version":
        print(f"lidarbench {lidarbench.__version__}")
        return 0

    # Placeholder commands
    if parsed.command in ("init", "preflight", "run", "resume", "report", "doctor"):
        print(
            f"Error: '{parsed.command}' command is not yet implemented.",
            file=sys.stderr,
        )
        return 1

    # Should not reach here, but handle any unknown command gracefully
    print(f"Error: unknown command '{parsed.command}'", file=sys.stderr)
    return 1


def app() -> int:
    """Application entry point (used by __main__.py and setup.py entry point)."""
    return main()
