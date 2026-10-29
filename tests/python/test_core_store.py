"""Canonical JSON, write-once publishing, append-only logs and sortable IDs."""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest

from lidarbench.core.canonical import CanonicalError, canonical_bytes, digest, sha256_file
from lidarbench.core.store import (
    AppendOnlyLog,
    RecordExistsError,
    make_read_only,
    new_sortable_id,
    publish_bytes,
    publish_json,
    read_jsonl,
)


class TestCanonical:
    def test_sorted_compact_utf8_with_newline(self):
        assert canonical_bytes({"b": 1, "a": [1.5, "é", None, True]}) == (
            '{"a":[1.5,"é",null,true],"b":1}\n'.encode()
        )

    def test_digest_ignores_key_order_and_tuple_vs_list(self):
        assert digest({"x": (1, 2), "y": {"b": 1, "a": 2}}) == digest(
            {"y": {"a": 2, "b": 1}, "x": [1, 2]}
        )

    @pytest.mark.parametrize("value", [math.nan, math.inf, Path("x"), {1: "a"}, {"s": {1}}, b"x"])
    def test_rejects_ambiguous_values(self, value):
        with pytest.raises(CanonicalError):
            canonical_bytes({"v": value})

    def test_file_digest(self, tmp_path):
        path = tmp_path / "f"
        path.write_bytes(b"abc")
        assert sha256_file(path) == (
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        )


class TestPublish:
    def test_publish_once_and_never_overwrite(self, tmp_path):
        path = tmp_path / "record.json"
        sha = publish_json(path, {"a": 1})
        assert json.loads(path.read_text(encoding="utf-8")) == {"a": 1}
        assert sha == sha256_file(path)
        with pytest.raises(RecordExistsError):
            publish_json(path, {"a": 2})
        assert json.loads(path.read_text(encoding="utf-8")) == {"a": 1}
        assert [p.name for p in tmp_path.iterdir()] == ["record.json"]

    def test_refuses_existing_symlink(self, tmp_path):
        target = tmp_path / "target"
        target.write_text("x")
        link = tmp_path / "link"
        try:
            link.symlink_to(target)
        except OSError:
            pytest.skip("symlinks not permitted")
        with pytest.raises(RecordExistsError):
            publish_bytes(link, b"y")
        assert target.read_text() == "x"

    def test_make_read_only(self, tmp_path):
        path = tmp_path / "f"
        publish_bytes(path, b"x")
        make_read_only(path)
        with pytest.raises(PermissionError):
            open(path, "wb")


class TestAppendOnlyLog:
    def test_sequence_numbers_continue_across_instances(self, tmp_path):
        path = tmp_path / "events.jsonl"
        log = AppendOnlyLog(path)
        assert log.append({"event": "a"})["seq"] == 1
        assert log.append({"event": "b"})["seq"] == 2
        again = AppendOnlyLog(path)
        assert again.append({"event": "c"})["seq"] == 3
        assert [r["event"] for r in read_jsonl(path).records] == ["a", "b", "c"]

    def test_torn_tail_is_preserved_and_terminated(self, tmp_path):
        path = tmp_path / "events.jsonl"
        AppendOnlyLog(path).append({"event": "a"})
        with open(path, "ab") as handle:
            handle.write(b'{"seq": 2, "ev')
        before = path.read_bytes()
        read = read_jsonl(path)
        assert read.torn_tail and len(read.records) == 1
        log = AppendOnlyLog(path)
        assert log.append({"event": "b"})["seq"] == 2
        assert path.read_bytes().startswith(before)
        lines = path.read_bytes().split(b"\n")
        assert lines[1] == b'{"seq": 2, "ev'
        final = read_jsonl(path)
        assert [r["event"] for r in final.records] == ["a", "b"]
        assert final.damaged_lines == (2,) and not final.torn_tail

    def test_damaged_lines_are_reported(self, tmp_path):
        path = tmp_path / "events.jsonl"
        path.write_bytes(b'not json\n[1]\n{"seq": 1}\n')
        read = read_jsonl(path)
        assert read.damaged_lines == (1, 2) and len(read.records) == 1


def test_sortable_ids_are_unique_ordered_and_portable():
    ids = [new_sortable_id() for _ in range(2000)]
    assert len(set(ids)) == len(ids)
    assert ids == sorted(ids)
    assert all(len(i) == 26 and i.isalnum() and i.isupper() for i in ids)
