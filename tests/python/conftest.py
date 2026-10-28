"""Pytest configuration and shared fixtures."""

import pytest


@pytest.fixture
def tmp_project_dir(tmp_path):
    """Temporary project directory for testing."""
    return tmp_path


@pytest.fixture
def sample_config():
    """Sample configuration dictionary for testing."""
    return {
        "campaign_id": "test-campaign-001",
        "description": "Test campaign",
        "cores": [1, 2],
        "datasets": ["petawawa-test"],
        "tools": ["lidR", "lasR"],
    }
