#!/bin/bash
# acceptance.criteria 2: PATH-TAGGED CALLSITES.
#   Every journal row's callsite (program_hash, path) resolves through
#   the retained provenance map to a source span on the full corpus
#   (call-sites.provenance); zero un-attributable diagnostics.
#
#   Corpus here: a source-compiled program with TWO prim callsites
#   (bracket abstraction may duplicate paths — every JOURNAL row must
#   still resolve).  Bare-ternary programs legitimately journal "" (no
#   provenance exists); the criterion holds for compiled programs.
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[2.1] compile from source, run both callsites"
G=$(mint_grant echo)
# two echo callsites; the inner call's result feeds the outer call's args
H=$(post_source '"(lambda (x) (prim \"echo\" (prim \"echo\" x)))"')
RUN=$(do_run "$H" '["10"]' "[\"$G\"]")
NROWS=$(echo "$RUN" | jget "len(d['journal'])")
[ "$NROWS" = "2" ] || fail "expected 2 journaled calls, got $NROWS"

echo "[2.2] every journal row's (program_hash, path) resolves to a span"
RUN_JSON=$(echo "$RUN" | jget "json.dumps({'run': d['run'], 'journal': d['journal']})")
python3 -c 'import json,sys
d = json.loads(sys.argv[1])
prog_hash = sys.argv[2]
rows = d["journal"]
assert d["run"]["program_hash"] == prog_hash, "run must pair with the program hash"
assert all(r["callsite_path"] != "" for r in rows), "un-attributable journal row (empty callsite)"
' "$RUN_JSON" "$H" || exit 1
# resolve paths through the provenance map (GET /api/programs/:hash ir column)
# NOTE: the API returns ir as a PARSED json object, so it must be
# re-dumped as JSON — `print()` of a python dict is not JSON (the
# single-quote repr breaks the reader below).
PROG_JSON=/tmp/prog_$$.json
IR_JSON=/tmp/ir_$$.json
curl -sf -H "$AUTH" $BASE/api/programs/$H > "$PROG_JSON" || fail "fetch program ir"
python3 -c 'import json,sys
prog = json.load(open(sys.argv[1]))
ir = prog["ir"]
assert ir is not None, "compiled program must carry provenance ir"
if isinstance(ir, str): ir = json.loads(ir)
json.dump(ir, open(sys.argv[2], "w"))
' "$PROG_JSON" "$IR_JSON" || { rm -f "$PROG_JSON" "$IR_JSON"; fail "extract ir"; }
rm -f "$PROG_JSON"
python3 -c 'import json,sys
ir = json.load(open(sys.argv[1]))
tags = {t["path"]: t for t in ir["tags"]}
run = json.loads(sys.argv[2])
for r in run["journal"]:
    path = r["callsite_path"]
    t = tags.get(path)
    assert t is not None, f"callsite {path} not in provenance map"
    assert t.get("span"), f"callsite {path} has no source span"
    assert t.get("ir") is not None, f"callsite {path} has no IR node id"
print("both callsites resolve to spans")
' /tmp/ir_$$.json "$RUN_JSON" || { rm -f /tmp/ir_$$.json; exit 1; }
rm -f /tmp/ir_$$.json

echo "[2.3] divergence surface joins the span (call-sites.provenance end-to-end)"
FORK=$(curl -sf -X POST -H "$AUTH" --data-binary '{"edits":[{"seq":0,"result_ternary":"0"}]}' \
  $BASE/api/journals/$(echo "$RUN" | jget "d['run']['id']")/fork) \
  || fail "counterfactual fork"
echo "$FORK" | grep -q '"verify":"failed"' || fail "edited fork must diverge"
SPAN=$(echo "$FORK" | jget "d['verify']['span'] is not None")
[ "$SPAN" = "True" ] || fail "divergence JSON must carry the provenance span"

echo "ACCEPT 2 OK: every callsite (program_hash, path) resolves to a source span"
