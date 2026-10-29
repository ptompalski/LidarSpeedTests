"""Campaign runner end to end with the synthetic and scripted test adapters."""

from __future__ import annotations

import json
import os
import stat
import sys
import threading
from pathlib import Path

import pytest
from campaign_helpers import make_runner, write_config, write_layout

from lidarbench import cli
from lidarbench.campaign.results import RESULT_COLUMNS, collect_rows, render_rows, write_results
from lidarbench.campaign.runner import (
    Action,
    IdentityChangedError,
    IdentityMismatchError,
    RunError,
    RunOptions,
    Selection,
)
from lidarbench.campaign.store import CampaignIntegrityError, CampaignStore
from lidarbench.config.campaign import ConfigError
from lidarbench.core.canonical import sha256_file
from lidarbench.core.locks import ExclusiveLock, LockHeldError
from lidarbench.core.store import read_jsonl
from lidarbench.execution.process import CancellationToken

pytestmark = [pytest.mark.integration, pytest.mark.timeout(300)]

SCRIPTED = '[[tools]]\nid = "scripted"\n'


@pytest.fixture
def base(tmp_path):
    return write_layout(tmp_path)


def events(store_path: Path) -> list[dict]:
    return list(read_jsonl(store_path / "events.jsonl").records)


def results(store_path: Path) -> list[dict]:
    out = []
    for attempt in sorted((store_path / "attempts").iterdir()):
        result = attempt / "result.json"
        if result.exists():
            out.append(json.loads(result.read_text(encoding="utf-8")))
    return out


def tree_digest(path: Path) -> dict[str, str]:
    return {
        p.relative_to(path).as_posix(): sha256_file(p)
        for p in sorted(path.rglob("*"))
        if p.is_file()
    }


class TestCreateAndRun:
    def test_full_run_records_complete_provenance(self, base):
        config = write_config(
            base,
            workloads='[[workloads]]\nid = "synthetic.checksum"\n'
            '[[workloads]]\nid = "synthetic.blocked"\n',
            tools='[[tools]]\nid = "synthetic"\n[[tools]]\nid = "synth-b"\nadapter = "synthetic"\n',
        )
        runner = make_runner(config, base)
        summary = runner.run()
        assert summary.campaign_state == "created" and summary.exit_code == 0
        assert summary.statuses == {"succeeded": 6, "skipped_blocked": 6}
        path = summary.campaign_path

        manifest = json.loads((path / "campaign.json").read_text(encoding="utf-8"))
        assert manifest["identity_sha256"] == summary.identity_sha256
        assert set(manifest["components"]) == {
            "code",
            "config",
            "workloads",
            "tools",
            "datasets",
            "environment",
            "schedule",
        }
        identity = {
            n: json.loads((path / "identity" / f"{n}.json").read_text(encoding="utf-8"))
            for n in ("code", "tools", "datasets", "environment", "workloads", "config")
        }
        assert identity["code"]["package_files"]["campaign/runner.py"]
        tool = identity["tools"]["tools"][0]
        assert tool["executables"]["python"]["sha256"] and tool["version"]
        files = {f["path"]: f for f in identity["datasets"]["datasets"][0]["files"]}
        assert set(files) == {"tile 1.laz", "tile2.laz", "tile2.lax"}
        assert identity["datasets"]["datasets"][0]["inputs"] == ["tile 1.laz", "tile2.laz"]
        assert identity["environment"]["logical_cpus"] >= 1
        impl = identity["workloads"]["workloads"][1]["implementations"]["synthetic"]
        assert impl["status"] == "blocked" and impl["reason"]
        assert identity["config"]["scheduling"]["seed"] == 20261020
        schedule = json.loads((path / "schedule.json").read_text(encoding="utf-8"))
        assert len(schedule["entries"]) == 12 and schedule["seed"] == 20261020
        assert (path / "schedule.csv").exists() and (path / "provenance.json").exists()
        assert len(list((path / "verifications").iterdir())) == 1

        for result in results(path):
            assert result["structural_status"] == "not_run"
            assert result["numerical_status"] == "unverified"
            assert "does not imply numerical validity" in result["validation_note"]
            assert result["cache_policy"] == "uncontrolled-warm"
            if result["execution_status"] == "succeeded":
                assert result["timing"]["timing_boundary"] == "process"
                assert result["timing"]["elapsed_ns"] > 0
                assert result["outputs"] and result["outputs"][0]["sha256"]
                assert result["scratch"]["cleanup"]["complete"]
                assert {"started.json", "invocation.json", "execution.json"} <= set(result["files"])
            else:
                assert result["skip_reason"] and result["timing"] is None
        for attempt in (path / "attempts").iterdir():
            for f in attempt.rglob("*"):
                if f.is_file():
                    assert not os.stat(f).st_mode & stat.S_IWUSR, f
        assert list((base / "scratch").iterdir()) == []
        assert not (base / "workspace" / ".lidarbench.lock").exists()
        assert not (base / "state" / "benchmark-execution.lock").exists()
        kinds = [e["event"] for e in events(path)]
        assert kinds[:3] == ["campaign_created", "run_started", "identity_verified"]
        assert kinds[-2:] == ["integrity_checked", "run_finished"]
        assert [e["seq"] for e in events(path)] == list(range(1, len(kinds) + 1))

    def test_resume_skips_complete_entries_without_new_attempts(self, base):
        config = write_config(base)
        make_runner(config, base).run()
        store_path = base / "workspace" / "campaigns" / "test-campaign"
        before = tree_digest(store_path / "attempts")
        summary = make_runner(config, base).run()
        assert summary.campaign_state == "resumed"
        assert summary.action_counts() == {"skip-complete": 2, "skip-warmup": 1}
        assert summary.statuses == {}
        assert tree_digest(store_path / "attempts") == before

    def test_dry_run_writes_nothing_and_previews_commands(self, base):
        config = write_config(
            base,
            workloads='[[workloads]]\nid = "synthetic.unsupported"\n'
            '[[workloads]]\nid = "synthetic.hashchain"\n'
            "parameters = { iterations = 10 }\n",
        )
        summary = make_runner(config, base).dry_run()
        assert summary.campaign_state == "new" and summary.dry_run
        assert not (base / "workspace").exists()
        statuses = {p["status"] for p in summary.previews.values()}
        assert statuses == {"ready", "skipped_unsupported"}
        ready = next(p for p in summary.previews.values() if p["status"] == "ready")
        assert ready["argv"][0] == sys.executable or Path(ready["argv"][0]).is_absolute()
        record = summary.to_record()
        assert record["entries"][0]["action"] == "warmup"

    def test_dry_run_refuses_while_machine_lock_active(self, base):
        config = write_config(base)
        with ExclusiveLock(base / "state" / "benchmark-execution.lock", "other"):
            with pytest.raises(LockHeldError):
                make_runner(config, base).dry_run()

    def test_dirty_checkout_refused_unless_allowed(self, base):
        config = write_config(base)

        def dirty():
            return {"available": True, "dirty": True, "status_paths": ["src/x.py"]}

        with pytest.raises(RunError, match="uncommitted"):
            make_runner(config, base, git_info=dirty).run()
        assert not (base / "workspace" / "campaigns" / "test-campaign").exists()
        summary = make_runner(
            config, base, git_info=dirty, options=RunOptions(allow_dirty=True)
        ).run()
        provenance = json.loads(
            (summary.campaign_path / "provenance.json").read_text(encoding="utf-8")
        )
        assert provenance["allow_dirty"] and provenance["git"]["dirty"]

    def test_resume_only_requires_existing(self, base):
        with pytest.raises(RunError, match="does not exist"):
            make_runner(write_config(base), base, options=RunOptions(require_existing=True)).run()


class TestIdentity:
    def test_changed_input_requires_new_campaign(self, base):
        config = write_config(base)
        first = make_runner(config, base).run()
        (base / "data" / "tile2.lax").write_bytes(b"changed companion")
        with pytest.raises(IdentityMismatchError) as info:
            make_runner(config, base).run()
        assert list(info.value.differences) == ["datasets"]
        assert len(results(first.campaign_path)) == 3
        assert events(first.campaign_path)[-1]["event"] == "identity_mismatch"
        dry = make_runner(config, base).dry_run()
        assert dry.campaign_state == "mismatch" and dry.exit_code == 1

    @pytest.mark.parametrize(
        ("change", "component"),
        [
            ({"cpu_counts": "[1, 2]"}, "config"),
            ({"scheduling": "repetitions = 3\nwarmups = 1\ncooldown_s = 0"}, "config"),
            ({"workloads": '[[workloads]]\nid = "synthetic.checksum"\ntimeout_s = 9\n'}, "config"),
            (
                {"tools": '[[tools]]\nid = "synthetic"\noptions = { python = "nope-python" }\n'},
                "tools",
            ),
        ],
    )
    def test_changed_definitions_require_new_campaign(self, base, change, component):
        make_runner(write_config(base), base).run()
        with pytest.raises(IdentityMismatchError) as info:
            make_runner(write_config(base, **change), base).run()
        assert component in info.value.differences

    def test_input_change_during_run_stops_before_next_entry(self, base):
        config = write_config(base)

        def report(line):
            if line.strip().startswith("-> "):
                (base / "data" / "tile2.laz").write_bytes(b"tampered")

        with pytest.raises(IdentityChangedError):
            make_runner(config, base, report=report).run()
        path = base / "workspace" / "campaigns" / "test-campaign"
        kinds = [e["event"] for e in events(path)]
        assert "identity_changed" in kinds and kinds[-1] == "run_aborted"

    def test_file_added_to_dataset_during_run_stops_campaign(self, base):
        config = write_config(base)

        def report(line):
            if line.strip().startswith("-> "):
                (base / "data" / "tile 1.lax").write_bytes(b"new index")

        with pytest.raises(IdentityChangedError, match="added"):
            make_runner(config, base, report=report).run()

    def test_cancel_during_hashing_releases_locks(self, base):
        token = CancellationToken()
        token.cancel("test")
        with pytest.raises(RunError) as info:
            make_runner(write_config(base), base, cancel=token).run()
        assert info.value.exit_code == 130
        assert not (base / "workspace" / ".lidarbench.lock").exists()
        assert not (base / "state" / "benchmark-execution.lock").exists()

    def test_tampered_attempt_blocks_resume_and_export(self, base):
        summary = make_runner(write_config(base), base).run()
        result = next((summary.campaign_path / "attempts").iterdir()) / "result.json"
        os.chmod(result, 0o644)
        result.write_text(result.read_text(encoding="utf-8").replace("succeeded", "failed"))
        with pytest.raises(CampaignIntegrityError):
            make_runner(write_config(base), base).run()
        with pytest.raises(CampaignIntegrityError):
            collect_rows(summary.campaign_path)


class TestInterruptionAndRetry:
    def test_cancel_mid_attempt_publishes_interrupted_and_resume_retries(self, base):
        config = write_config(
            base,
            workloads='[[workloads]]\nid = "scripted.sleep"\n',
            tools=SCRIPTED,
            scheduling="repetitions = 2\nwarmups = 0\ncooldown_s = 0",
        )
        token = CancellationToken()

        def report(line):
            if line.startswith("[1/"):
                threading.Timer(1.0, token.cancel, args=("test interrupt",)).start()

        summary = make_runner(config, base, cancel=token, report=report).run()
        assert summary.stop_reason == "interrupted" and summary.exit_code == 130
        assert summary.statuses == {"interrupted": 1}
        path = summary.campaign_path
        assert events(path)[-1]["event"] == "run_interrupted"
        assert list((base / "scratch").iterdir()) == []
        first = results(path)
        assert [r["execution_status"] for r in first] == ["interrupted"]
        assert first[0]["launched"] and first[0]["timing"]["elapsed_ns"] > 0

        dry = make_runner(config, base).dry_run()
        actions = [(d.entry.repetition, d.action) for d in dry.decisions]
        assert actions == [(1, Action.RETRY_INTERRUPTED), (2, Action.RUN)]
        assert dry.decisions[0].previous_attempts == (first[0]["attempt_id"],)
        assert results(path) == first

    def test_crash_leaves_incomplete_attempt_that_is_tombstoned(self, base, monkeypatch):
        config = write_config(base, scheduling="repetitions = 2\nwarmups = 0\ncooldown_s = 0")
        calls = {"n": 0}
        original = CampaignStore.seal_attempt

        def crash(path, result):
            calls["n"] += 1
            if calls["n"] == 2:
                raise OSError("simulated power loss")
            return original(path, result)

        monkeypatch.setattr(CampaignStore, "seal_attempt", staticmethod(crash))
        with pytest.raises(OSError, match="power loss"):
            make_runner(config, base).run()
        monkeypatch.undo()
        path = base / "workspace" / "campaigns" / "test-campaign"
        incomplete = [
            a for a in (path / "attempts").iterdir() if not (a / "completed.json").exists()
        ]
        assert len(incomplete) == 1
        snapshot = tree_digest(incomplete[0])

        summary = make_runner(config, base).run()
        assert summary.statuses == {"succeeded": 1}
        assert any("incomplete" in w for w in summary.warnings)
        tomb = json.loads((incomplete[0] / "abandoned.json").read_text(encoding="utf-8"))
        assert tomb["reason"] and tomb["detected_by_run"] == summary.run_id
        after = tree_digest(incomplete[0])
        after.pop("abandoned.json")
        assert after == snapshot
        assert tomb["files_sha256"] == {
            rel: sha for rel, sha in snapshot.items() if rel in tomb["files_present"]
        }
        assert all(not os.access(incomplete[0] / rel, os.W_OK) for rel in tomb["files_sha256"])
        rows = collect_rows(path)
        states = sorted((r["record_state"], r["execution_status"], r["superseded"]) for r in rows)
        assert states == [
            ("abandoned", "interrupted", True),
            ("completed", "succeeded", False),
            ("completed", "succeeded", False),
        ]
        # Tampering with an abandoned attempt after its tombstone is detected.
        victim = incomplete[0] / "started.json"
        victim.chmod(0o644)
        victim.write_text(victim.read_text(encoding="utf-8") + " ", encoding="utf-8")
        with pytest.raises(CampaignIntegrityError, match="changed after its tombstone"):
            collect_rows(path)

    def test_failures_and_timeouts_need_explicit_retry(self, base):
        config = write_config(
            base,
            workloads='[[workloads]]\nid = "scripted.fail"\n'
            '[[workloads]]\nid = "scripted.sleep"\ntimeout_s = 0.5\n'
            '[[workloads]]\nid = "scripted.nooutput"\n',
            tools=SCRIPTED,
            scheduling="repetitions = 1\nwarmups = 0\ncooldown_s = 0",
        )
        first = make_runner(config, base).run()
        assert first.statuses == {"failed": 1, "timed_out": 1, "succeeded": 1}
        by_workload = {r["entry"]["workload"]: r for r in results(first.campaign_path)}
        assert by_workload["scripted.fail"]["exit_code"] == 3
        assert by_workload["scripted.nooutput"]["missing_required_outputs"] == ["ok.txt"] or (
            by_workload["scripted.nooutput"]["missing_required_outputs"][0].endswith("ok.txt")
        )
        again = make_runner(config, base).run()
        assert again.action_counts() == {"skip-failed": 3} and again.statuses == {}
        retried = make_runner(config, base, options=RunOptions(retry_failed=True)).run()
        assert retried.action_counts() == {"retry-failed": 3}
        assert len(results(first.campaign_path)) == 6

    def test_warmups_rerun_only_for_pending_units(self, base):
        config = write_config(
            base,
            workloads='[[workloads]]\nid = "scripted.ok"\n[[workloads]]\nid = "scripted.fail"\n',
            tools=SCRIPTED,
            scheduling="repetitions = 1\nwarmups = 1\ncooldown_s = 0",
        )
        make_runner(config, base).run()
        summary = make_runner(config, base, options=RunOptions(retry_failed=True)).dry_run()
        actions = {(d.entry.phase, d.entry.workload): d.action for d in summary.decisions}
        assert actions[("warmup", "scripted.fail")] == Action.WARMUP
        assert actions[("warmup", "scripted.ok")] == Action.SKIP_WARMUP
        assert actions[("measurement", "scripted.ok")] == Action.SKIP_COMPLETE


class TestSelectionAndLocks:
    def test_selection_and_limit(self, base):
        config = write_config(
            base,
            cpu_counts="[1, 2]",
            tools=SCRIPTED,
            workloads='[[workloads]]\nid = "scripted.ok"\n',
        )
        selection = Selection(cpus=frozenset({2}), phases=frozenset({"measurement"}), limit=1)
        summary = make_runner(config, base, options=RunOptions(selection=selection)).run()
        assert summary.statuses == {"succeeded": 1}
        assert summary.action_counts() == {"run": 1, "skip-limit": 1, "skip-unselected": 4}
        rows = collect_rows(summary.campaign_path)
        assert [(r["cpus"], r["threading_model"]) for r in rows] == [(2, "native_threads")]
        with pytest.raises(ConfigError, match="unknown tool"):
            make_runner(
                config, base, options=RunOptions(selection=Selection(tools=frozenset({"x"})))
            ).run()

    def test_concurrent_runs_are_refused(self, base):
        config = write_config(base)
        (base / "workspace").mkdir()
        with ExclusiveLock(base / "workspace" / ".lidarbench.lock", "other"):
            with pytest.raises(LockHeldError):
                make_runner(config, base).run()
        with ExclusiveLock(base / "state" / "benchmark-execution.lock", "other"):
            with pytest.raises(LockHeldError):
                make_runner(config, base).run()
            assert not (base / "workspace" / ".lidarbench.lock").exists()

    def test_workspace_inside_dataset_is_refused(self, base):
        config = write_config(base)
        text = config.read_text(encoding="utf-8").replace(
            'workspace = "workspace"', 'workspace = "data/ws"'
        )
        config.write_text(text, encoding="utf-8")
        with pytest.raises(ConfigError, match="overlaps dataset"):
            make_runner(config, base).run()
        assert not (base / "data" / "ws").exists()

    def test_unsafe_scratch_root_is_refused(self, base):
        config = write_config(base)
        text = config.read_text(encoding="utf-8").replace('"scratch" }', '"data" }')
        config.write_text(text, encoding="utf-8")
        with pytest.raises(ConfigError, match="overlaps"):
            make_runner(config, base).run()


class TestResultsAndCli:
    def test_export_formats(self, base, tmp_path):
        summary = make_runner(write_config(base), base).run()
        rows = collect_rows(summary.campaign_path)
        assert len(rows) == 3 and all(list(r) == list(RESULT_COLUMNS) for r in rows)
        csv_text = render_rows(rows, "csv").decode("utf-8").splitlines()
        assert csv_text[0] == ",".join(RESULT_COLUMNS) and len(csv_text) == 4
        jsonl = [json.loads(line) for line in render_rows(rows, "jsonl").decode().splitlines()]
        assert jsonl[0]["numerical_status"] == "unverified"
        out = tmp_path / "results.csv"
        write_results(rows, out, "csv")
        with pytest.raises(FileExistsError):
            write_results(rows, out, "csv")
        try:
            import pyarrow  # noqa: F401
        except ImportError:
            with pytest.raises(RuntimeError, match="pyarrow"):
                write_results(rows, tmp_path / "r.parquet", "parquet")
        else:
            write_results(rows, tmp_path / "r.parquet", "parquet")

    def test_cli_run_dry_run_export_and_lock(self, base, monkeypatch, capsys):
        monkeypatch.setenv("LIDARBENCH_STATE_DIR", str(base / "state"))
        config = str(
            write_config(
                base,
                workloads='[[workloads]]\nid = "synthetic.hashchain"\n'
                "parameters = { iterations = 10 }\n",
            )
        )
        assert cli.main(["run", config, "--dry-run", "--json"]) == 0
        dry = json.loads(capsys.readouterr().out)
        assert dry["dry_run"] and dry["campaign_state"] == "new"
        assert cli.main(["resume", config]) == 1
        assert "does not exist" in capsys.readouterr().err
        assert cli.main(["run", config, "--allow-dirty", "--tool", "synthetic"]) == 0
        assert "attempts : succeeded=3" in capsys.readouterr().out
        campaign = str(base / "workspace" / "campaigns" / "test-campaign")
        assert cli.main(["export", campaign, "--format", "csv"]) == 0
        assert capsys.readouterr().out.startswith("campaign_id,")
        assert cli.main(["lock", "status", str(base / "workspace")]) == 0
        assert json.loads(capsys.readouterr().out)["exists"] is False
        assert cli.main(["lock", "release", str(base / "workspace")]) == 2
        assert cli.main(["scratch", config]) == 0
        assert json.loads(capsys.readouterr().out)["orphans"] == []


@pytest.mark.slow
def test_monitored_run_records_monitoring(base):
    config = write_config(
        base,
        workloads='[[workloads]]\nid = "synthetic.hashchain"\nparameters = { iterations = 200000 }\n',
        monitoring="enabled = true\ninterval_s = 0.1",
        scheduling="repetitions = 1\nwarmups = 0\ncooldown_s = 0",
    )
    summary = make_runner(config, base).run()
    assert summary.statuses == {"succeeded": 1}
    result = results(summary.campaign_path)[0]
    assert result["monitoring_status"] in ("available", "partial")
    assert any(name.startswith("monitor/") for name in result["files"])
    row = collect_rows(summary.campaign_path)[0]
    assert row["machine_memory_used_peak_bytes"]


class TestRetainedScratchAndIdentity:
    def test_retained_failure_scratch_is_logged_not_warned_as_orphan(self, base):
        config = write_config(
            base,
            workloads='[[workloads]]\nid = "scripted.fail"\n',
            tools=SCRIPTED,
            scheduling="repetitions = 1\nwarmups = 0\ncooldown_s = 0",
        )
        config.write_text(
            config.read_text(encoding="utf-8").replace(
                'roots = { local = "scratch" }',
                'roots = { local = "scratch" }\nretain = "on-failure"',
            ),
            encoding="utf-8",
        )
        first = make_runner(config, base).run()
        assert first.statuses == {"failed": 1}
        again = make_runner(config, base).run()
        assert not any("orphaned" in w for w in again.warnings)
        events = read_jsonl(again.campaign_path / "events.jsonl").records
        assert any(e["event"] == "retained_scratch" for e in events)

    def test_identity_freezes_contracts_and_validation_settings(self, base):
        config = write_config(base)
        summary = make_runner(config, base).run()
        workloads = json.loads(
            (summary.campaign_path / "identity" / "workloads.json").read_text(encoding="utf-8")
        )
        # Synthetic workloads have no contract in the suite: recorded explicitly as null.
        assert all(w["contract"] is None for w in workloads["workloads"])
        frozen = json.loads(
            (summary.campaign_path / "identity" / "config.json").read_text(encoding="utf-8")
        )
        assert frozen["validation"]["suite"].endswith("workloads-v4")
        assert frozen["validation"]["accept_known_extent_discrepancies"] is False
        assert frozen["scheduling"]["cpu_affinity"] == "pinned"
