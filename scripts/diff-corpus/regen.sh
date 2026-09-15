#!/usr/bin/env bash
# regen.sh — (re)build every scripts/diff-corpus/*.corpus from the
# definitions below, then generate the expect_* lines with the
# instrumented upstream copy (`gen refeval`) and cross-check the Common
# Lisp reference twin (reference/tree-calc.lisp). Fails loudly if the
# two engines disagree.
#
# Corpus format (one file per entry):
#   name <slug>
#   program <ternary>
#   arg <ternary>        (zero or more lines)
#   fuel <n>
#   size_cap <n>
#   expect_status <status>      — normal | fuel_exhausted | size_exhausted
#   expect_result <ternary or ->
#   expect_steps <n>
#
# The expect lines are generated ONCE from the instrumented verbatim
# port of upstream apply (tools/gen refeval) and checked in; the OCaml
# twin is independent of tuna's interpreter by construction (see
# tools/gen/gen.ml header). scripts/verify-differential.sh compares all
# three (refeval / CL twin / tuna CLI) against these checked-in lines.
set -euo pipefail
cd "$(dirname "$0")/../.."

eval $(opam env --switch=poohstack --set-switch)
dune build tools/gen/gen.exe cli/bin/main.exe
GEN=_build/default/tools/gen/gen.exe

NOT=22102000        # not: not true = false, not false = true
TRUE=10
FALSE=0
ID=21100            # identity: id x = x (found in M2)
OMEGA=221000        # self-application fixed point: f f = f
GROWER=2100         # apply g r = Fork (r, Stem r)
K=$($GEN term k)
S_K_K=$($GEN term s_k_k)
NOT_NOT=$($GEN term not_not)
AND=$($GEN term and)
OR=$($GEN term or)
SUCC_CH=$($GEN term succ_ch)
CHURCH0=$($GEN term church 0)
CHURCH2=$($GEN term church 2)
CHURCH3=$($GEN term church 3)
ADD_CH=$($GEN term add)
MUL_CH=$($GEN term mul)
FIX=$($GEN term fix)
NAT0=$($GEN nat 0); NAT5=$($GEN nat 5); NAT7=$($GEN nat 7)
SUCC_DAG=$($GEN dag scripts/diff-corpus/succ.dag)

emit() { # slug  -> writes <slug>.corpus with expects appended
  local slug=$1
  echo "$slug"
  local t; t=$(mktemp)
  $GEN refeval "$tmp.def" > "$t.refeval"
  sbcl --script reference/tree-calc.lisp "$tmp.def" > "$t.cl"
  if ! diff -q "$t.refeval" "$t.cl" >/dev/null; then
    echo "MISMATCH between refeval and CL twin on $slug:" >&2
    echo "  refeval: $(cat "$t.refeval")" >&2
    echo "  cl:      $(cat "$t.cl")" >&2
    exit 1
  fi
  { cat "$tmp.def"; echo
    local status result steps
    read -r status result steps < "$t.refeval"
    printf 'expect_status %s\nexpect_result %s\nexpect_steps %s\n' \
      "$status" "$result" "$steps"
  } > "scripts/diff-corpus/$slug.corpus"
  rm -f "$t.refeval" "$t.cl"
}

def() { # slug fuel cap program [args...]  (writes temp def file for emit)
  local slug=$1 fuel=$2 cap=$3 program=$4; shift 4
  { echo "name $slug"; echo "program $program";
    for a in "$@"; do echo "arg $a"; done
    echo "fuel $fuel"; echo "size_cap $cap"; } > "$tmp.def"
}

tmp=$(mktemp); trap 'rm -f "$tmp.def"' EXIT

# --- boolean corpus (published upstream values) ----------------------
def not_true  1000 1000 $NOT $TRUE;            emit not_true
def not_false 1000 1000 $NOT $FALSE;           emit not_false
def not_not_true 1000 1000 $NOT_NOT $TRUE;     emit not_not_true
def id_not 1000 1000 $ID $NOT;                 emit id_not
def k_xy 1000 1000 $K $NOT $TRUE;              emit k_xy
def s_k_k_id 1000 1000 $S_K_K $NOT;            emit s_k_k_id
def and_tt 20000 50000 $AND $TRUE $TRUE;      emit and_tt
def and_tf 20000 50000 $AND $TRUE $FALSE;      emit and_tf
def and_ft 20000 50000 $AND $FALSE $TRUE;      emit and_ft
def or_tt 20000 50000 $OR $TRUE $TRUE;         emit or_tt
def or_tf 20000 50000 $OR $TRUE $FALSE;        emit or_tf
def or_ft 20000 50000 $OR $FALSE $TRUE;        emit or_ft
def or_ff 20000 50000 $OR $FALSE $FALSE;       emit or_ff
def and_ff 20000 50000 $AND $FALSE $FALSE;     emit and_ff

# --- self-application / recursion budgets -----------------------------
# omega applied to 4 copies of itself: 1 triage firing per application,
# returns omega unchanged (upstream: f f = f). fuel 17 is ample.
def omega_4self 17 1000 $OMEGA $OMEGA $OMEGA $OMEGA $OMEGA
emit omega_4self
# grower g (apply g r = Fork(r, Stem r)); g g has live size 10:
# cap 8 trips, cap 10 lets it finish — same steps, different status.
def grower_cap8  1000 8  $GROWER $GROWER $GROWER; emit grower_cap8
def grower_cap10 1000 10 $GROWER $GROWER $GROWER; emit grower_cap10
# fix i_op = Y(identity) = pure self-application: fuel must run out.
def fix_fuel 500 100000 $FIX $FALSE;            emit fix_fuel

# --- nat encoding (upstream LSB-first bool list) ----------------------
# successor via the upstream DAG format (succ.dag), on nat encoding:
def succ_dag_7 1000 1000 "$SUCC_DAG" $NAT7;     emit succ_dag_7
# church succ on church numerals, with not as f, false as x:
#   succ n not false = not (n not false) = not^(n+1) false.
#   succ(0) = not false = true; succ(5) = not^6 false = false.
def succ_zero 1000 1000 $SUCC_CH $CHURCH0 $NOT $FALSE;  emit succ_zero
CHURCH5=$($GEN term church 5)
def succ_five 1000 1000 $SUCC_CH $CHURCH5 $NOT $FALSE;  emit succ_five

# --- church-encoded arithmetic (observable via not/false) -------------
# add m n f x = m f (n f x): add 2 3 not false = not^5 false = true.
def add_ch_2_3 20000 50000 $ADD_CH $CHURCH2 $CHURCH3 $NOT $FALSE
emit add_ch_2_3
# mul m n f = m (n f): mul 2 3 not false = not^6 false = false.
def mul_ch_2_3 20000 50000 $MUL_CH $CHURCH2 $CHURCH3 $NOT $FALSE
emit mul_ch_2_3
# church 3 succ church 0 not false = not^3 false = true.
def church3_succ_zero 20000 50000 $CHURCH3 $SUCC_CH $CHURCH0 $NOT $FALSE
emit church3_succ_zero

echo "all corpus entries regenerated; refeval == CL twin on every entry"
