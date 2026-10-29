"""On-disk campaign store (``lidarbench.campaign/1``).

Layout::

    <workspace>/
      .lidarbench.lock                 workspace lock (only while a run is active)
      campaigns/<campaign_id>/
        campaign.json                  receipt: identity digest, component digests,
                                       SHA-256 of every frozen file; written last
        identity/<component>.json      frozen identity components
        schedule.json, schedule.csv    frozen schedule (JSON is authoritative)
        provenance.json                recorded, never compared
        events.jsonl                   append-only event log (sequence numbers)
        verifications/<run_id>.json    one identity verification per run invocation
        attempts/<attempt_id>/
          started.json                 entry, sequence number, reason, host, pid
          invocation.json              resolved command, inputs, scratch (untimed)
          execution.json, stdout.log, stderr.log, monitor/   (execution engine)
          result.json                  consolidated record + SHA-256 of all files
          completed.json               receipt: SHA-256 of result.json; written last
          abandoned.json               tombstone for an attempt that never completed

Every file is created exactly once. Attempts are never edited or deleted;
an attempt without ``completed.json`` found by a later run receives an
``abandoned.json`` tombstone and its entry is retried with a new attempt.
"""

from __future__ import annotations

import os
import socket
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import lidarbench
from lidarbench.campaign.identity import IDENTITY_COMPONENTS
from lidarbench.campaign.schedule import ScheduleEntry
from lidarbench.core.canonical import digest, sha256_file
from lidarbench.core.clock import iso_utc, now
from lidarbench.core.store import (
    PENDING_MARKER,
    AppendOnlyLog,
    iter_files,
    make_read_only,
    publish_bytes,
    publish_json,
    read_json,
)

CAMPAIGN_SCHEMA = "lidarbench.campaign/1"
STARTED_SCHEMA = "lidarbench.attempt-started/1"
INVOCATION_SCHEMA = "lidarbench.attempt-invocation/1"
RESULT_SCHEMA = "lidarbench.attempt/1"
COMPLETED_SCHEMA = "lidarbench.attempt-completed/1"
ABANDONED_SCHEMA = "lidarbench.attempt-abandoned/1"
VERIFICATION_SCHEMA = "lidarbench.identity-verification/1"

CAMPAIGNS_DIR = "campaigns"
CAMPAIGN_RECORD = "campaign.json"
IDENTITY_DIR = "identity"
SCHEDULE_JSON = "schedule.json"
SCHEDULE_CSV = "schedule.csv"
PROVENANCE = "provenance.json"
EVENTS = "events.jsonl"
VERIFICATIONS_DIR = "verifications"
ATTEMPTS_DIR = "attempts"
STARTED = "started.json"
INVOCATION = "invocation.json"
RESULT = "result.json"
COMPLETED = "completed.json"
ABANDONED = "abandoned.json"

SKIP_STATUSES = frozenset({"skipped_unavailable", "skipped_unsupported", "skipped_blocked"})
VERDICT_STATUSES = frozenset({"succeeded", "failed", "timed_out", *SKIP_STATUSES})


class CampaignIntegrityError(RuntimeError):
    """A frozen record or attempt does not match its digests; nothing is modified."""


class CampaignExistsError(RuntimeError):
    """The campaign directory exists but is not a complete campaign."""


def timestamp() -> str:
    """Current UTC time as ISO 8601."""
    return iso_utc(now().wall_ns)


def campaign_dir(workspace: Path, campaign_id: str) -> Path:
    """Directory of a campaign in a workspace."""
    return Path(workspace) / CAMPAIGNS_DIR / campaign_id


@dataclass(frozen=True)
class AttemptInfo:
    """One attempt directory as found on disk (verified)."""

    attempt_id: str
    path: Path
    state: str
    """``completed``, ``abandoned`` or ``incomplete`` (no receipt, no tombstone yet)."""
    started: dict[str, Any] | None
    result: dict[str, Any] | None
    abandoned: dict[str, Any] | None = None

    @property
    def entry_key(self) -> str | None:
        """Schedule entry key, if the start record exists."""
        return self.started["entry"]["entry_key"] if self.started else None

    @property
    def sequence(self) -> int:
        """Start sequence number (0 if unknown)."""
        return int(self.started.get("sequence", 0)) if self.started else 0

    @property
    def execution_status(self) -> str | None:
        """Published execution status, ``interrupted`` for tombstones."""
        if self.result is not None:
            return str(self.result["execution_status"])
        if self.state == "abandoned":
            return "interrupted"
        return None

    @property
    def is_verdict(self) -> bool:
        """Whether the attempt is a verdict about the tool.

        True for recorded skips and for launched processes that reached
        ``succeeded``, ``failed`` or ``timed_out``. Interruptions and failures
        before launch (scratch, inputs, monitor start, planning) are not.
        """
        if self.state != "completed" or self.result is None:
            return False
        status = self.execution_status
        if status in SKIP_STATUSES:
            return True
        return status in VERDICT_STATUSES and bool(self.result.get("launched"))

    @property
    def eligible(self) -> bool:
        """Whether the attempt completes its schedule entry for resume purposes.

        Eligible: ``succeeded`` with every required output present, or a recorded
        skip. This is an *execution* criterion only; numerical validity is a
        separate verdict that execution success never implies.
        """
        if self.result is None or self.state != "completed":
            return False
        status = self.result["execution_status"]
        if status in SKIP_STATUSES:
            return True
        return status == "succeeded" and not self.result.get("missing_required_outputs")


class CampaignStore:
    """Read and write one campaign directory."""

    def __init__(self, path: Path) -> None:
        self.path = Path(path)
        self.manifest: dict[str, Any] | None = None
        self.components: dict[str, Any] = {}
        self.schedule: tuple[ScheduleEntry, ...] = ()
        self._frozen_files: dict[str, str] = {}
        self._events: AppendOnlyLog | None = None

    @property
    def campaign_id(self) -> str:
        """Campaign ID (from the loaded manifest)."""
        assert self.manifest is not None
        return str(self.manifest["campaign_id"])

    @property
    def identity_sha256(self) -> str:
        """Frozen identity digest."""
        assert self.manifest is not None
        return str(self.manifest["identity_sha256"])

    @property
    def attempts_dir(self) -> Path:
        """Directory holding attempt directories."""
        return self.path / ATTEMPTS_DIR

    @property
    def exists(self) -> bool:
        """Whether a complete campaign (with ``campaign.json``) exists."""
        return (self.path / CAMPAIGN_RECORD).is_file()

    def events(self) -> AppendOnlyLog:
        """The append-only event log."""
        if self._events is None:
            self._events = AppendOnlyLog(self.path / EVENTS)
        return self._events

    def log(self, event: str, **fields: Any) -> dict[str, Any]:
        """Append an event."""
        return self.events().append({"event": event, "at": timestamp(), **fields})

    # -- creation ---------------------------------------------------------------

    def create(
        self,
        campaign_id: str,
        components: Mapping[str, Any],
        digests: Mapping[str, str],
        identity_sha256: str,
        schedule_csv: bytes,
        provenance: Mapping[str, Any],
    ) -> None:
        """Freeze a new campaign. ``campaign.json`` is published last.

        Raises:
            CampaignExistsError: The directory already exists.
        """
        if self.path.exists():
            raise CampaignExistsError(f"campaign directory already exists: {self.path}")
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.path.mkdir()
        (self.path / IDENTITY_DIR).mkdir()
        (self.path / ATTEMPTS_DIR).mkdir()
        (self.path / VERIFICATIONS_DIR).mkdir()
        files: dict[str, str] = {}
        component_files: dict[str, dict[str, str]] = {}
        for name in IDENTITY_COMPONENTS:
            rel = SCHEDULE_JSON if name == "schedule" else f"{IDENTITY_DIR}/{name}.json"
            files[rel] = publish_json(self.path / rel, components[name])
            component_files[name] = {"file": rel, "sha256": digests[name]}
        files[SCHEDULE_CSV] = publish_bytes(self.path / SCHEDULE_CSV, schedule_csv)
        files[PROVENANCE] = publish_json(self.path / PROVENANCE, dict(provenance))
        manifest = {
            "schema": CAMPAIGN_SCHEMA,
            "campaign_id": campaign_id,
            "identity_sha256": identity_sha256,
            "identity_algorithm": "sha256 of canonical JSON {component: sha256(canonical component)}",
            "components": component_files,
            "files": files,
            "schedule_entries": len(components["schedule"]["entries"]),
            "created_at": timestamp(),
            "created_by": {
                "lidarbench_version": lidarbench.__version__,
                "host": socket.gethostname(),
                "pid": os.getpid(),
            },
        }
        for rel in files:
            make_read_only(self.path / rel)
        publish_json(self.path / CAMPAIGN_RECORD, manifest)
        make_read_only(self.path / CAMPAIGN_RECORD)
        self.load()
        self.log("campaign_created", identity_sha256=identity_sha256)

    # -- loading and verification -------------------------------------------------

    def load(self) -> None:
        """Load and verify ``campaign.json`` and every frozen file it lists.

        Raises:
            CampaignExistsError: The directory exists without ``campaign.json``.
            CampaignIntegrityError: A digest does not match.
        """
        record_path = self.path / CAMPAIGN_RECORD
        if not record_path.is_file():
            raise CampaignExistsError(
                f"{self.path} exists but has no {CAMPAIGN_RECORD} (incomplete creation). "
                "Nothing was resumed; inspect it, or use a new campaign_id."
            )
        manifest = read_json(record_path)
        if not isinstance(manifest, dict) or manifest.get("schema") != CAMPAIGN_SCHEMA:
            raise CampaignIntegrityError(f"{record_path}: not a {CAMPAIGN_SCHEMA} record")
        if manifest.get("campaign_id") != self.path.name:
            raise CampaignIntegrityError(
                f"{record_path}: campaign_id {manifest.get('campaign_id')!r} does not match "
                f"directory {self.path.name!r}"
            )
        frozen: dict[str, str] = {}
        for rel, sha in manifest["files"].items():
            actual = self._file_sha(rel)
            if actual != sha:
                raise CampaignIntegrityError(f"frozen file {rel} changed or is missing")
            frozen[rel] = sha
        components = {}
        for name in IDENTITY_COMPONENTS:
            entry = manifest["components"][name]
            value = read_json(self.path / entry["file"])
            if digest(value) != entry["sha256"]:
                raise CampaignIntegrityError(f"identity component {name} digest mismatch")
            components[name] = value
        digests = {n: manifest["components"][n]["sha256"] for n in IDENTITY_COMPONENTS}
        if digest(digests) != manifest["identity_sha256"]:
            raise CampaignIntegrityError("identity_sha256 does not match component digests")
        frozen[CAMPAIGN_RECORD] = sha256_file(record_path)
        self.manifest = manifest
        self.components = components
        self.schedule = tuple(
            ScheduleEntry.from_record(r) for r in components["schedule"]["entries"]
        )
        self._frozen_files = frozen

    def _file_sha(self, rel: str) -> str | None:
        try:
            return sha256_file(self.path / rel)
        except OSError:
            return None

    def frozen_file_problems(self) -> list[str]:
        """Re-hash the frozen records (small files; cheap per-entry check)."""
        return [
            f"frozen campaign file {rel} changed or is missing"
            for rel, sha in self._frozen_files.items()
            if self._file_sha(rel) != sha
        ]

    def component_digests(self) -> dict[str, str]:
        """Frozen component digests."""
        assert self.manifest is not None
        return {n: self.manifest["components"][n]["sha256"] for n in IDENTITY_COMPONENTS}

    def publish_verification(self, run_id: str, record: Mapping[str, Any]) -> None:
        """Write the identity verification record of one run invocation."""
        directory = self.path / VERIFICATIONS_DIR
        directory.mkdir(exist_ok=True)
        path = directory / f"{run_id}.json"
        publish_json(path, {"schema": VERIFICATION_SCHEMA, "run_id": run_id, **record})
        make_read_only(path)

    # -- attempts -----------------------------------------------------------------

    def read_attempt(self, path: Path) -> AttemptInfo:
        """Read and verify one attempt directory.

        Raises:
            CampaignIntegrityError: A receipt, digest or identity field mismatches.
        """
        attempt_id = path.name
        started = self._read_optional(path / STARTED)
        if started is not None:
            self._check_started(path, started)
        abandoned = self._read_optional(path / ABANDONED)
        if not (path / COMPLETED).exists():
            state = "abandoned" if abandoned is not None else "incomplete"
            for rel, sha in ((abandoned or {}).get("files_sha256") or {}).items():
                if sha is not None and self._sha_or_none(path / rel) != sha:
                    raise CampaignIntegrityError(
                        f"{path}: abandoned attempt file {rel} changed after its tombstone"
                    )
            return AttemptInfo(attempt_id, path, state, started, None, abandoned)
        if started is None:
            raise CampaignIntegrityError(f"{path}: receipt without {STARTED}")
        receipt = read_json(path / COMPLETED)
        result_sha = self._sha_or_none(path / RESULT)
        if not isinstance(receipt, dict) or receipt.get("result_sha256") != result_sha:
            raise CampaignIntegrityError(f"{path}: {RESULT} does not match its receipt")
        result = read_json(path / RESULT)
        for key in ("campaign_id", "identity_sha256", "attempt_id"):
            expected = attempt_id if key == "attempt_id" else getattr(self, key)
            if result.get(key) != expected:
                raise CampaignIntegrityError(f"{path}: result {key} mismatch")
        if result.get("entry") != started.get("entry"):
            raise CampaignIntegrityError(f"{path}: result entry differs from {STARTED}")
        for rel, sha in result.get("files", {}).items():
            if self._sha_or_none(path / rel) != sha:
                raise CampaignIntegrityError(f"{path}: attempt file {rel} changed or is missing")
        return AttemptInfo(attempt_id, path, "completed", started, result, abandoned)

    def _check_started(self, path: Path, started: dict[str, Any]) -> None:
        if (
            started.get("schema") != STARTED_SCHEMA
            or started.get("attempt_id") != path.name
            or started.get("campaign_id") != self.campaign_id
            or started.get("identity_sha256") != self.identity_sha256
        ):
            raise CampaignIntegrityError(f"{path}: {STARTED} does not belong to this campaign")
        entry = started.get("entry", {})
        index = entry.get("schedule_index")
        if (
            not isinstance(index, int)
            or not 1 <= index <= len(self.schedule)
            or self.schedule[index - 1].to_record() != entry
        ):
            raise CampaignIntegrityError(f"{path}: entry is not a row of the frozen schedule")

    @staticmethod
    def _sha_or_none(path: Path) -> str | None:
        try:
            return sha256_file(path)
        except OSError:
            return None

    @staticmethod
    def _read_optional(path: Path) -> dict[str, Any] | None:
        if not path.exists():
            return None
        try:
            value = read_json(path)
        except ValueError as exc:
            raise CampaignIntegrityError(f"{path}: malformed JSON: {exc}") from exc
        if not isinstance(value, dict):
            raise CampaignIntegrityError(f"{path}: not a JSON object")
        return value

    def attempts(self) -> list[AttemptInfo]:
        """All attempts, verified, ordered by start sequence then ID."""
        if not self.attempts_dir.is_dir():
            return []
        infos = [
            self.read_attempt(p)
            for p in sorted(self.attempts_dir.iterdir())
            if p.is_dir() and not p.is_symlink()
        ]
        return sorted(infos, key=lambda a: (a.sequence, a.attempt_id))

    def new_attempt_dir(self, attempt_id: str) -> Path:
        """Reserve an attempt directory (exclusive)."""
        path = self.attempts_dir / attempt_id
        path.mkdir()
        return path

    def tombstone(self, info: AttemptInfo, run_id: str, reason: str) -> dict[str, Any]:
        """Write ``abandoned.json`` into an incomplete attempt.

        The tombstone records the SHA-256 of every file the attempt left, and those
        files are then made read-only like other published records. Their content
        is never changed.
        """
        files = sorted(
            p.relative_to(info.path).as_posix()
            for p in iter_files(info.path)
            if PENDING_MARKER not in p.name
        )
        digests: dict[str, str | None] = {}
        for rel in files:
            try:
                digests[rel] = sha256_file(info.path / rel)
            except OSError:
                digests[rel] = None
        record = {
            "schema": ABANDONED_SCHEMA,
            "attempt_id": info.attempt_id,
            "campaign_id": self.campaign_id,
            "identity_sha256": self.identity_sha256,
            "entry_key": info.entry_key,
            "reason": reason,
            "detected_at": timestamp(),
            "detected_by_run": run_id,
            "files_present": files,
            "files_sha256": digests,
        }
        publish_json(info.path / ABANDONED, record)
        make_read_only(info.path / ABANDONED)
        for rel, sha in digests.items():
            if sha is not None:
                try:
                    make_read_only(info.path / rel)
                except OSError:
                    pass
        return record

    @staticmethod
    def attempt_files(path: Path) -> dict[str, str]:
        """SHA-256 of every file in an attempt directory except the final records."""
        excluded = {RESULT, COMPLETED, ABANDONED}
        return {
            p.relative_to(path).as_posix(): sha256_file(p)
            for p in iter_files(path)
            if p.relative_to(path).as_posix() not in excluded and PENDING_MARKER not in p.name
        }

    @staticmethod
    def seal_attempt(path: Path, result: dict[str, Any]) -> str:
        """Publish ``result.json`` then ``completed.json`` and make files read-only.

        Returns:
            SHA-256 of ``result.json``.
        """
        result_sha = publish_json(path / RESULT, result)
        publish_json(
            path / COMPLETED,
            {
                "schema": COMPLETED_SCHEMA,
                "attempt_id": path.name,
                "result_sha256": result_sha,
                "completed_at": timestamp(),
            },
        )
        for file in iter_files(path):
            if PENDING_MARKER not in file.name:
                make_read_only(file)
        return result_sha
