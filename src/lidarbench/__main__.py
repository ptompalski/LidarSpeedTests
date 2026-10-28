"""Command-line interface entry point."""

import sys


def main() -> int:
    """Entry point for the lidarbench command."""
    from lidarbench.cli import app

    return app()


if __name__ == "__main__":
    sys.exit(main())
