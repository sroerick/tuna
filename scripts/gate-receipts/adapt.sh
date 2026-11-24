#!/usr/bin/env bash
# adapt.sh - host-side log adapters for gate receipts
# (borg/gate-receipts.borg, observation-record / counts).
#
#   adapt.sh alcotest   <log>   -> "run passed failed skipped" | unparsed
#   adapt.sh differential <log> -> "run passed failed skipped" | unparsed
#   adapt.sh make-test  <log>   -> aggregate over every suite in a
#                                  `make -C tuna test` log | unparsed
#
# The adapter runs host-side because a 10k-line log is a priced
# non-starter in the calculus (the chapter's prim-creep discipline).
# Its counts are an attested fact like the exit code; the log hash lets
# a reviewer re-parse.  No python, no jq (claims.sh has the same rule).
#
# Rules, and why they are not just "grep the summary":
#
#   alcotest.  A suite summary is "Test Successful in Xs. N tests run."
#   or "F failures! in Xs. N tests run." where N counts PASSES, so
#   run = N + F.  A line printed starting "SKIP" is a skip even when
#   alcotest marks the case [OK] - the pp PG suite's fail-open shape
#   (15 [OK] cases that print SKIP and test nothing).
#
#   differential.  "all N corpus entries agree" is N passes;
#   "F/N corpus entries FAILED" is N run, N-F passed, F failed.  A
#   FAILED line outranks the "sbcl missing: ... skipped" line: the old
#   `&& ... || echo` rule printed that skip sentence even when the
#   harness had just failed 93/93, and believing the sentence is the
#   fail-open this chapter exists to catch.
#
#   make-test.  Sum every suite's counts.  If a suite says it was
#   skipped (postgres down, node absent, sbcl missing) and no suite
#   failed, the gate is unparsed, never pass: part of the gate did not
#   run.  A real failure still outranks a skipped suite, because the
#   log states it.  A log with no recognisable suite summary is
#   unparsed (a cached dune run that prints nothing is not evidence).

set -eu

usage() {
  echo "usage: adapt.sh <alcotest|differential|make-test> <log>  (log = - for stdin)" >&2
  exit 2
}
[ $# -eq 2 ] || usage
adapter="$1"
log="$2"
case "$adapter" in
  alcotest|differential|make-test) : ;;
  *) usage ;;
esac
if [ "$log" != "-" ]; then
  [ -f "$log" ] || { echo "adapt.sh: no such log: $log" >&2; exit 2; }
fi

LC_ALL=C awk -v adapter="$adapter" '
  # summary lines share a shape: find the count immediately before the
  # word "test"/"tests" in "N tests run."
  function passes(   i, n) {
    n = -1
    for (i = 1; i <= NF; i++) if ($i == "test" || $i == "tests") n = $(i-1)
    return n
  }
  function alcotest_success() {
    n = passes()
    if (n < 0) return
    run += n; passed += n; summaries++
  }
  function alcotest_failure() {
    f = $1 + 0
    n = passes()
    if (n < 0) return
    run += n + f; passed += n; failed += f; summaries++
  }
  function differential_ok() {
    for (i = 1; i <= NF; i++) if ($i == "all") { n = $(i+1) + 0; break }
    run += n; passed += n; summaries++
  }
  function differential_fail() {
    split($2, a, "/")
    f = a[1] + 0; n = a[2] + 0
    run += n; passed += n - f; failed += f; summaries++
  }
  adapter == "alcotest" {
    if ($0 ~ /^Test Successful in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_success()
    else if ($0 ~ /^[0-9]+ failures?! in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_failure()
    else if ($0 ~ /^SKIP/) skipped++
    next
  }
  adapter == "differential" {
    if ($0 ~ /^differential: [0-9]+\/[0-9]+ corpus entries FAILED/) differential_fail()
    else if ($0 ~ /^differential: all [0-9]+ corpus entries agree/) differential_ok()
    else if ($0 ~ /^sbcl missing: differential harness skipped/) skip_marker = 1
    next
  }
  adapter == "make-test" {
    if ($0 ~ /^Test Successful in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_success()
    else if ($0 ~ /^[0-9]+ failures?! in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_failure()
    else if ($0 ~ /^differential: [0-9]+\/[0-9]+ corpus entries FAILED/) differential_fail()
    else if ($0 ~ /^differential: all [0-9]+ corpus entries agree/) differential_ok()
    else if ($0 ~ /^SKIP/) skipped++
    else if ($0 ~ /^store tests skipped \(postgres down/) skip_marker = 1
    else if ($0 ~ /^filmstrip_cdp: skip/) skip_marker = 1
    else if ($0 ~ /^sbcl missing: differential harness skipped/) skip_marker = 1
    next
  }
  END {
    if (adapter == "differential") {
      if (summaries > 0) { printf "%d %d %d %d\n", run, passed, failed, skipped; exit }
      if (skip_marker)   { print "0 0 0 0"; exit }
      print "unparsed"; exit
    }
    if (summaries == 0) { print "unparsed"; exit }
    if (failed > 0)     { printf "%d %d %d %d\n", run, passed, failed, skipped; exit }
    if (skip_marker)    { print "unparsed"; exit }
    printf "%d %d %d %d\n", run, passed, failed, skipped
  }
' "$log"
