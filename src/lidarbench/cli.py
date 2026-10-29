"""Command-line interface implementation."""

import argparse
import sys

import lidarbench


def add_subparsers(parser: argparse.ArgumentParser) -> None:
    """Add subcommands to the argument parser.

    Currently includes:
    - version: display version information
    - check: display operating system facts and adapter capabilities
    - run / resume: create or resume a campaign and execute its schedule
    - export: write a verified results table (JSONL, CSV or Parquet)
    - lock: show or explicitly release a stale lock
    - scratch: list or explicitly delete orphaned run-owned scratch
    - validate: validate outputs against a workload contract (outside any timing)
    - data: list, verify, register, prepare and download datasets
    - help: display help (implicit)

    Additional commands (init, preflight, report, doctor) are placeholders
    and will be implemented in later phases.
    """
    subparsers = parser.add_subparsers(dest="command", help="command to run")

    # version command
    subparsers.add_parser(
        "version",
        help="display version information",
    )

    check = subparsers.add_parser(
        "check",
        help="display operating system information and detected adapter capabilities",
    )
    check.add_argument("--json", action="store_true", help="print the report as JSON")
    check.add_argument(
        "--adapter",
        action="append",
        metavar="ID",
        help="only report this adapter (repeatable)",
    )

    scratch = subparsers.add_parser(
        "scratch",
        help="list or explicitly delete orphaned run-owned scratch directories",
        description=(
            "Lists scratch directories left by crashed runs of a campaign. With --clean "
            "--confirm, deletes those whose ownership marker matches and whose attempt "
            "already has a receipt or tombstone."
        ),
    )
    scratch.add_argument("config", help="campaign configuration (TOML)")
    scratch.add_argument("--clean", action="store_true", help="delete proven orphans")
    scratch.add_argument("--confirm", action="store_true", help="required with --clean")

    # Placeholder commands (not yet implemented; --help works but execution warns)
    subparsers.add_parser(
        "init",
        help="[PLACEHOLDER] initialize a new campaign workspace",
    )

    subparsers.add_parser(
        "preflight",
        help="[PLACEHOLDER] validate configuration and check tool availability",
    )

    for name, text in (
        ("run", "create or resume a campaign and execute its frozen schedule"),
        ("resume", "resume an existing campaign (never creates one)"),
    ):
        run = subparsers.add_parser(name, help=text, description=text)
        _add_run_arguments(run)

    export = subparsers.add_parser(
        "export",
        help="write a verified per-attempt results table",
        description="Verify a campaign and write one row per attempt.",
    )
    export.add_argument("campaign", help="campaign directory (<workspace>/campaigns/<id>)")
    export.add_argument(
        "--format", default="jsonl", choices=("jsonl", "csv", "parquet"), help="output format"
    )
    export.add_argument("-o", "--output", help="new output file (default: stdout)")

    lock = subparsers.add_parser(
        "lock",
        help="show or explicitly release a workspace or machine lock",
        description=(
            "Locks are never removed automatically. 'release' removes a lock only if "
            "its owner process no longer exists (or with --force when it cannot be verified)."
        ),
    )
    lock.add_argument("action", choices=("status", "release"))
    lock.add_argument(
        "target",
        nargs="?",
        help="workspace directory or lock file (default: this user's machine lock)",
    )
    lock.add_argument("--confirm", action="store_true", help="required to release")
    lock.add_argument(
        "--force", action="store_true", help="also release a lock that cannot be verified"
    )

    validate = subparsers.add_parser(
        "validate",
        help="validate tool outputs against a workload contract (untimed)",
        description=(
            "Checks artifacts, raster structure and values against an independent "
            "reference. Exit 0: validated; 3: descriptive only (accepted exception or "
            "unverified); 1: failed or incomplete; 2: usage error."
        ),
    )
    validate.add_argument("--workload", required=True, help="contract id, e.g. pixel_metrics_1a")
    validate.add_argument("--tool", required=True, help="implementation name, e.g. lidR")
    validate.add_argument("--outputs", required=True, help="directory containing the outputs")
    validate.add_argument(
        "--input", action="append", metavar="FILE", help="input tile (repeatable)"
    )
    validate.add_argument("--input-dir", metavar="DIR", help="all *.las/*.laz tiles in DIR")
    validate.add_argument("--suite", help="workload suite directory (default: workloads-v2)")
    validate.add_argument("--epsg", type=int, help="declared horizontal CRS of the inputs")
    validate.add_argument("--mode", default="numerical", choices=("numerical", "performance-only"))
    validate.add_argument(
        "--execution-status",
        default="succeeded",
        help="execution status of the attempt (outputs are inspected only if 'succeeded')",
    )
    validate.add_argument(
        "--accept-known-extent-discrepancies",
        action="store_true",
        help="opt-in: accept outward extension by at most one cell per edge (accepted_exception)",
    )
    validate.add_argument("-o", "--report", help="write the JSON report to this new file")
    validate.add_argument("--json", action="store_true", help="print the full JSON report")

    from lidarbench.datasets.commands import add_data_parser

    add_data_parser(subparsers)

    subparsers.add_parser(
        "report",
        help="[PLACEHOLDER] generate a report from a campaign",
    )

    subparsers.add_parser(
        "doctor",
        help="[PLACEHOLDER] diagnose environment and configuration",
    )


def _add_run_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("config", help="campaign configuration (TOML)")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="show the schedule, resume decisions and planned commands; write nothing",
    )
    parser.add_argument("--json", action="store_true", help="print the summary as JSON")
    select = parser.add_argument_group(
        "selection (restricts which frozen schedule rows run now; never changes identity)"
    )
    select.add_argument("--workload", action="append", metavar="NAME", help="repeatable")
    select.add_argument("--dataset", action="append", metavar="ID", help="repeatable")
    select.add_argument("--tool", action="append", metavar="ID", help="repeatable")
    select.add_argument("--cpus", action="append", type=int, metavar="N", help="repeatable")
    select.add_argument(
        "--phase", action="append", choices=("warmup", "measurement"), help="repeatable"
    )
    select.add_argument("--limit", type=int, metavar="N", help="dispatch at most N entries")
    parser.add_argument(
        "--retry-failed",
        action="store_true",
        help="retry entries whose attempts failed or timed out (new attempts; old ones kept)",
    )
    parser.add_argument(
        "--allow-dirty",
        action="store_true",
        help="allow creating a campaign from an uncommitted lidarbench checkout (recorded)",
    )


def main(args: list[str] | None = None) -> int:
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

    if parsed.command == "check":
        from lidarbench.check import run_check

        return run_check(as_json=parsed.json, adapter_ids=parsed.adapter)

    if parsed.command in ("run", "resume"):
        from lidarbench.campaign.commands import run_command

        return run_command(
            parsed.config,
            dry_run=parsed.dry_run,
            as_json=parsed.json,
            workloads=parsed.workload,
            datasets=parsed.dataset,
            tools=parsed.tool,
            cpus=parsed.cpus,
            phases=parsed.phase,
            limit=parsed.limit,
            retry_failed=parsed.retry_failed,
            allow_dirty=parsed.allow_dirty,
            require_existing=parsed.command == "resume",
        )

    if parsed.command == "export":
        from lidarbench.campaign.commands import export_command

        return export_command(parsed.campaign, fmt=parsed.format, output=parsed.output)

    if parsed.command == "lock":
        from lidarbench.campaign.commands import lock_command

        return lock_command(
            parsed.action, parsed.target, confirm=parsed.confirm, force=parsed.force
        )

    if parsed.command == "scratch":
        from lidarbench.campaign.commands import scratch_command

        return scratch_command(parsed.config, clean=parsed.clean, confirm=parsed.confirm)

    if parsed.command == "validate":
        from lidarbench.validation.commands import validate_command

        return validate_command(
            workload=parsed.workload,
            tool=parsed.tool,
            outputs=parsed.outputs,
            input_files=parsed.input,
            input_dir=parsed.input_dir,
            suite=parsed.suite,
            epsg=parsed.epsg,
            mode=parsed.mode,
            execution_status=parsed.execution_status,
            accept_known_extent_discrepancies=parsed.accept_known_extent_discrepancies,
            report_path=parsed.report,
            as_json=parsed.json,
        )

    if parsed.command == "data":
        from lidarbench.datasets.commands import data_command

        return data_command(parsed)

    # Placeholder commands
    if parsed.command in ("init", "preflight", "report", "doctor"):
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
