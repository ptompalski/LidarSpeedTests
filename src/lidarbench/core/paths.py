"""Cross-platform path handling and validation."""

from pathlib import Path


def normalize_path(path: str | Path) -> Path:
    """Normalize a path for the current platform.
    
    Converts to absolute, resolves symlinks, and uses the platform's path separator.
    
    Args:
        path: Path as string or Path object.
    
    Returns:
        Normalized absolute Path.
    """
    p = Path(path).resolve()
    return p


def is_path_contained(path: Path, container: Path) -> bool:
    """Check if a path is contained within another path.
    
    Args:
        path: Path to check.
        container: Potential container path.
    
    Returns:
        True if path is inside container or equal to it.
    """
    try:
        path.resolve().relative_to(container.resolve())
        return True
    except ValueError:
        return False


def get_path_components(path: Path) -> list[str]:
    """Get the components of a path.
    
    Args:
        path: Path to decompose.
    
    Returns:
        List of path components (from root to leaf).
    """
    return list(path.parts)


def is_protected_path(path: Path) -> bool:
    """Check if a path is a system-protected location (root, home, etc.).
    
    Protected paths cannot be used as scratch roots to avoid accidental damage.
    
    Args:
        path: Path to check.
    
    Returns:
        True if path is a protected system location.
    """
    path = path.resolve()
    home = Path.home()

    # Check for filesystem root
    if path == path.anchor or str(path) in ("/", "C:\\", "C:/"):
        return True

    # Check for home directory
    if path == home:
        return True

    # Check for common system directories on Unix
    protected = {Path(x) for x in ("/etc", "/usr", "/bin", "/sbin", "/lib", "/var")}
    for protected_dir in protected:
        if path == protected_dir or is_path_contained(path, protected_dir):
            return True

    return False


def is_within_working_directory(path: Path, cwd: Path | None = None) -> bool:
    """Check if a path is within or equal to the working directory.
    
    Args:
        path: Path to check.
        cwd: Current working directory (defaults to Path.cwd()).
    
    Returns:
        True if path is inside cwd or equal to it.
    """
    if cwd is None:
        cwd = Path.cwd()
    return is_path_contained(path, cwd)
