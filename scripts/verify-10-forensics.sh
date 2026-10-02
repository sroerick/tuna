#!/bin/bash
# acceptance (borg/forensics.borg): divergence forensics, the full
# SEAL -> DIFF -> CAUSE -> LOCATE -> FIX -> PROVE -> HANDOFF loop over a
# deliberate-fault corpus, unattended against the live server.
#
# The corpus driver (scripts/forensics-corpus/forensics.py) does the
# work; this script is the gate wrapper.  It prints the MEASURED number
# beside every gate and mirrors deriv-check exit semantics:
#   0 green   (all gates pass)
#   1 any-miss (a gate missed; record F9 SUPERLATIVE OVERREACH)
#   2 malformed (bad corpus / unreachable server / CORE touched)
#
# Gates (frozen; a miss is F9, never a quietly relabeled test):
#   1. LOCALIZATION  >=90% class-1 exact mutation path by first-diff
#                    alone; >=80% class-2 at the mutated def's boundary
#                    (provenance names its IR span).
#   2. QUERY BUDGET  class-1/2 localized within <=8 API queries;
#                    class-3 within <=12 including the fork.
#   3. COLLABORATION two protocol runs from identical starting hashes
#                    agree on every localization answer exactly.
#   4. COUNTERFACTUAL every class-3 fork reaches a different terminal
#                    status and replay-verifies both records; every
#                    class-4 omega closes as `loop` with a closure pair.
#   5. UNATTENDED    this script, zero human state.
#
# CORE UNTOUCHED (chapter acceptance 3): interpreter/, compiler/, and
# common/ must be byte-identical to HEAD.  A dirty core is malformed (2).
#
# Requires: live dev server (scripts/dev.sh start); verify-lib.sh env.
set -e
. "$(dirname "$0")/verify-lib.sh"

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DRIVER="$ROOT/scripts/forensics-corpus/forensics.py"
[ -f "$DRIVER" ] || fail "missing corpus driver $DRIVER"

# -- CORE UNTOUCHED (acceptance 3) ------------------------------------
dirty=$(git -C "$ROOT" status --porcelain -- interpreter compiler common 2>/dev/null || true)
if [ -n "$dirty" ]; then
  echo "MALFORMED: core is not byte-identical to HEAD:" >&2
  echo "$dirty" >&2
  exit 2
fi

# -- run the corpus driver unattended ---------------------------------
OUT=$(mktemp)
ERR=$(mktemp)
trap 'rm -f "$OUT" "$ERR"' EXIT
# the driver reads these from env; pin them to the gate's server/token
export TUNA_HTTP_PORT="${TUNA_HTTP_PORT:-18090}"
export TUNA_SMOKE_TOKEN="$TOKEN"
set +e
python3 "$DRIVER" --json >"$OUT" 2>"$ERR"
rc=$?
set -e
if [ "$rc" = "2" ]; then
  echo "MALFORMED: corpus driver could not run:" >&2
  cat "$ERR" >&2
  exit 2
fi
if ! python3 -c "import json,sys; json.load(open(sys.argv[1]))" "$OUT" 2>/dev/null; then
  echo "MALFORMED: corpus driver produced no parseable JSON:" >&2
  cat "$OUT" "$ERR" >&2
  exit 2
fi

# -- print measured numbers beside each gate --------------------------
python3 - "$OUT" <<'PYEOF'
import json, sys

d = json.load(open(sys.argv[1]))
a = d["aggregate"]
res = d["results"]

def cls(n):
    return [r for r in res if r.get("class") == n and "error" not in r]

print("verify-10-forensics: measured numbers")
print("  corpus: %d faults (%d class-1, %d class-2, %d class-3, %d class-4)"
      % (len(res), a["class1_total"], a["class2_total"],
         a["class3_total"], a["class4_total"]))
print("  [1] LOCALIZATION   class-1 %d/%d = %.1f%% (>=90)  class-2 %d/%d = %.1f%% (>=80)"
      % (a["class1_localized"], a["class1_total"], a["localization_class1_pct"],
         a["class2_localized"], a["class2_total"], a["localization_class2_pct"]))
print("  [2] QUERY BUDGET   class-1/2 max %d queries (<=8)  class-3 max %d (<=12)"
      % (a["max_queries_class12"], a["max_queries_class3"]))
print("  [3] COLLABORATION  %s" % ("agree exactly" if a["gate3_collaboration"] else "MISMATCH"))
c3 = cls(3); c4 = cls(4); c12 = cls(1) + cls(2)
print("  [4] COUNTERFACTUAL  class-3 %d/%d different terminal + both verified; "
      "class-4 %d/%d loop closure pair"
      % (sum(1 for r in c3 if r["different_terminal"]), len(c3),
         sum(1 for r in c4 if r["loop_not_fuel"] and r["has_pair"]), len(c4)))
print("  [5] UNATTENDED    exit semantics: this script (0 green / 1 miss / 2 malformed)")
print("  counterexample pinned (count preserves, behavior differs): %s"
      % a["counterexample_pinned"])
for r in res:
    if "error" in r:
        print("  ERROR %s: %s" % (r["id"], r["error"]))
PYEOF

# -- verdict ----------------------------------------------------------
if [ "$rc" = "0" ]; then
  echo "verify-10-forensics: green"
  exit 0
fi
echo "verify-10-forensics: GATE MISS (F9 SUPERLATIVE OVERREACH - record in FINDINGS.md)" >&2
exit 1
