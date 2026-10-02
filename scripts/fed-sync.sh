#!/usr/bin/env bash
# fed-sync.sh - sync a namespace from a peer tuna instance (M12 F2
# ops-chain sync, borg/federation.borg): pull the verifiable op window,
# pull every value its effects cite (rehash-verified), and apply the
# window into a local destination namespace with those values inline.
#
#   usage: fed-sync.sh <peer-base> <local-base> <local-token> \
#                      <src-prefix> <dst-prefix> [after-seq]
#
# <peer-base>  = the instance being pulled from
# <local-base> = the instance being applied to
# <local-token>= a valid bearer at BOTH (value reads are hash-gated and
#                ops reads are open to any bearer; the apply needs a
#                local fed-peer identity whose name is fed-peer-<name>,
#                so <dst-prefix> must be ns/<name>/... unless admin)
# [after-seq]  = the PEER's last_seq from the previous sync (default 0,
#                for a first sync).  This is the source watermark; the
#                apply itself pins no destination head (the reads above
#                journal into the local log too, so a destination head
#                is not a stable thing to pin).
#
# Exits 0 only if every pulled value rehashed AND the apply answered ok.
set -e
peer=$1; local=$2; token=$3; src=$4; dst=$5; after=${6:-0}
[ -n "$peer" ] && [ -n "$local" ] && [ -n "$token" ] && [ -n "$src" ] \
  && [ -n "$dst" ] || {
  echo "usage: fed-sync.sh <peer-base> <local-base> <token> <src> <dst> [after-seq]" >&2
  exit 2
}

tmp=$(mktemp -d /tmp/fed-sync.XXXXXX)
trap 'rm -rf "$tmp"' EXIT
auth="Authorization: Bearer $token"

# 1. pull a contiguous window (each row carries its true prev_hash).
curl -sf -m 60 -X GET "$peer/api/fed/ops?after_seq=$after&limit=5000" \
  -H "$auth" -o "$tmp/window.json" || {
  echo "fed-sync: peer $peer unreachable or refused the ops pull" >&2
  exit 1
}
verified=$(python3 -c "import json;print(json.load(open('$tmp/window.json'))['verified'])")
[ "$verified" = True ] || { echo "fed-sync: peer window did not verify" >&2; exit 1; }
last=$(python3 -c "import json;print(json.load(open('$tmp/window.json'))['last_seq'])")

# 2. pull every value cited by an effect row under <src> and rehash it
#    locally (the apply re-verifies inline, but a client-side rehash is
#    the F1 contract: trust nothing but the hash).  Values are bundled
#    into the apply body, so no separate local write is needed.
python3 - "$tmp/window.json" "$src" <<'PY' > "$tmp/hashes.txt"
import json, sys
win = json.load(open(sys.argv[1])); src = sys.argv[2]
for op in win.get("ops", []):
    if op.get("op") in ("put", "cas") and op.get("value_hash") \
       and op.get("path", "").startswith(src):
        print(op["value_hash"])
PY

: > "$tmp/values.jsonl"
while read -r hash; do
  [ -n "$hash" ] || continue
  curl -sf -m 60 -X GET "$peer/api/fed/value/$hash" -H "$auth" \
    -o "$tmp/value.json" || { echo "fed-sync: peer has no value $hash" >&2; exit 1; }
  python3 - "$tmp/value.json" > "$tmp/value.bin" <<'PY'
import base64, json, sys
v = json.load(open(sys.argv[1]))
sys.stdout.buffer.write(v["payload"].encode() if v["kind"] == "tree"
                        else base64.b64decode(v["payload"]))
PY
  got=$(openssl dgst -sha256 -r < "$tmp/value.bin" | cut -d' ' -f1)
  [ "$got" = "$hash" ] || {
    echo "fed-sync: value REHASH MISMATCH got=$got want=$hash" >&2; exit 1; }
  # append the value object (hash/kind/payload) to the inline bundle
  python3 - "$tmp/value.json" "$hash" >> "$tmp/values.jsonl" <<'PY'
import json, sys
v = json.load(open(sys.argv[1]))
print(json.dumps({"hash": sys.argv[2], "kind": v["kind"],
                  "payload": v["payload"]}))
PY
  echo "fed-sync: pulled value $hash"
done < "$tmp/hashes.txt"

# 3. apply the window (filtered to <src>) with the pulled values bundled
#    inline.  No destination head is pinned: the peer's seqs name source
#    rows, and the local reads above journaled into the local log.
python3 - "$tmp/window.json" "$tmp/values.jsonl" "$src" "$dst" \
  > "$tmp/apply.json" <<'PY'
import json, sys
win = json.load(open(sys.argv[1]))
src, dst = sys.argv[3], sys.argv[4]
ops = [o for o in win.get("ops", []) if o.get("path", "").startswith(src)]
values = [json.loads(l) for l in open(sys.argv[2]) if l.strip()]
print(json.dumps({"src_prefix": src, "dst_prefix": dst,
                  "ops": ops, "values": values}))
PY
code=$(curl -s -m 60 -o "$tmp/apply.out" -w '%{http_code}' \
  -X POST "$local/api/fed/ops/apply" -H "$auth" \
  --data-binary "@$tmp/apply.json")
if [ "$code" != 200 ]; then
  echo "fed-sync: apply failed ($code): $(cat "$tmp/apply.out")" >&2
  exit 1
fi
python3 - "$tmp/apply.out" "$last" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
print("fed-sync: applied %d effects, source last_seq %s, shadowed %d"
      % (d["applied_count"], sys.argv[2], len(d["shadowed"])))
PY
