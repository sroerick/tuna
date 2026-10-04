#!/usr/bin/env python3
"""board_migrate.py — the one-time legacy board migration (board.borg L7).

Journaled, idempotent: walks todo/ on the live store, and for each
LEGACY item (a JSON byte record from the host-OCaml page era) re-encodes
the value IN PLACE as an L1 tree record - path untouched, ops chained,
actor the root/system identity.  A legacy row that cannot parse keeps
its EXACT bytes verbatim as the title (state forced open) - no path is
ever silently dropped.  Re-running is a no-op: after the first pass no
byte-kind values remain under todo/, so zero puts.

Reads journal as ordinary value fetches (hash-gated reads always have);
13.6's idempotence assertion counts WRITE ops (put) only.

Title bytes ride pytwin (the python twin of Tuna.Cstr.encode + the law-5
int codec + the record norm) because the reader's string literals carry
no escapes - run `pytwin.py parity` (verify-13 does) before trusting a
migration with quote-bearing legacy titles.

Env: TUNA_HTTP_PORT (18090), TUNA_SMOKE_TOKEN or
     /tmp/tuna-dev/bootstrap.token.
Usage: board_migrate.py [--prefix todo/] [--dry-run]
"""

import base64
import datetime
import json
import os
import sys
import urllib.request

sys.path.insert(0, os.path.dirname(__file__))
import pytwin  # noqa: E402

BASE = "http://127.0.0.1:" + os.environ.get("TUNA_HTTP_PORT", "18090")
TOKEN = os.environ.get("TUNA_SMOKE_TOKEN") or open(
    "/tmp/tuna-dev/bootstrap.token").read().strip()
if TOKEN.startswith("TUNA_BOOTSTRAP_TOKEN="):
    TOKEN = TOKEN.split("=", 1)[1]
AUTH = {"Authorization": "Bearer " + TOKEN, "Content-Type": "application/json"}
PREFIX = "todo/"


def post(path, obj):
    req = urllib.request.Request(BASE + path, data=json.dumps(obj).encode(),
                                 headers=AUTH)
    try:
        return json.load(urllib.request.urlopen(req))
    except urllib.error.HTTPError as e:
        raise SystemExit(f"board_migrate: POST {path} -> {e.code}: "
                         f"{e.read().decode()[:300]}")


def get(path):
    req = urllib.request.Request(BASE + path, headers=AUTH)
    try:
        return json.load(urllib.request.urlopen(req))
    except urllib.error.HTTPError as e:
        raise SystemExit(f"board_migrate: GET {path} -> {e.code}")


def iso_to_epoch(s):
    try:
        return int(datetime.datetime.strptime(
            s, "%Y-%m-%dT%H:%M:%SZ").replace(
                tzinfo=datetime.timezone.utc).timestamp())
    except (ValueError, TypeError):
        return None


def record_for(raw_bytes):
    """L1 ternary for one legacy payload, or None when not legacy.

    Parseable JSON record -> field-for-field; anything else -> the raw
    bytes kept verbatim as the title (L7 tail: never dropped)."""
    def keep_raw():
        return pytwin.record("open", raw_bytes, b"migration", 0)
    try:
        j = json.loads(raw_bytes.decode("utf-8"))
    except (ValueError, UnicodeDecodeError):
        return keep_raw()
    if not isinstance(j, dict):
        return keep_raw()
    title = j.get("title")
    state = j.get("state")
    who = j.get("created_by")
    when = iso_to_epoch(j.get("created_at"))
    if not isinstance(title, str) or state not in ("open", "done") \
            or not isinstance(who, str) or when is None:
        return keep_raw()
    return pytwin.record(state, title.encode("utf-8"), who.encode("utf-8"),
                         when)


def main():
    dry = "--dry-run" in sys.argv
    args = sys.argv[1:]
    for i, a in enumerate(args):
        if a == "--prefix" and i + 1 < len(args):
            global PREFIX
            PREFIX = args[i + 1]
    entries = post("/api/tree/list", {"prefix": PREFIX, "limit": 1000})
    rows = entries.get("entries", [])
    puts = 0
    kept = 0
    trees = 0
    for e in rows:
        path = e["path"]
        if not path.startswith(PREFIX):
            continue  # prefix is a range; the slash-pinned slice only
        v = get("/api/fed/value/" + e["value_hash"])
        if v.get("kind") != "bytes":
            trees += 1
            continue
        raw = base64.b64decode(v.get("payload", ""))
        rec = record_for(raw)
        if dry:
            print(f"  would re-encode {path} ({len(raw)} legacy bytes)")
            puts += 1
            continue
        r = post("/api/tree/put", {"path": path, "value_ternary": rec})
        if r.get("_err") or "version" not in r:
            raise SystemExit(f"board_migrate: put {path} failed: {r}")
        puts += 1
        print(f"  re-encoded {path} -> record (version {r['version']}, "
              f"journaled put)")
    print(f"board_migrate: {puts} rows re-encoded, {trees} already trees, "
          f"{kept} skipped{' (DRY RUN)' if dry else ''}")


if __name__ == "__main__":
    main()
