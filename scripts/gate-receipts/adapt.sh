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
  echo "usage: adapt.sh <alcotest|differential|make-test|unittest> <log>  (log = - for stdin)" >&2
  exit 2
}
[ $# -eq 2 ] || usage
adapter="$1"
log="$2"
case "$adapter" in
  alcotest|differential|make-test|unittest) : ;;
  *) usage ;;
esac
if [ "$log" != "-" ]; then
  [ -f "$log" ] || { echo "adapt.sh: no such log: $log" >&2; exit 2; }
fi

LC_ALL=C awk -v adapter="$adapter" '
  # a harness may colourise its summary (a pty-captured log, FORCE_COLOR,
  # or a tool that colours unconditionally).  Strip CSI sequences first, or
  # a green suite parses as unparsed and a gate the book calls verified is
  # refused.
  {
    gsub(/\033\[[0-9;]*[A-Za-z]/, "")
    line = $0
  }
  BEGIN { ran = -1 }
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
  # python -m unittest.  "Ran N tests" then "OK"[ (skipped=K)] or
  # "FAILED (...)".  Stock 3.14 also emits "FAILED (unexpected successes=N)"
  # for a suite that did not do what it said - a FAILING suite - so a
  # FAILED line with no recognised count is one failure, never zero.
  function ut_failures(   f) {
    f = 0
    if (match(line, /failures=[0-9]+/)) f += substr(line, RSTART + 9, RLENGTH - 9) + 0
    if (match(line, /errors=[0-9]+/)) f += substr(line, RSTART + 7, RLENGTH - 7) + 0
    if (match(line, /unexpected successes=[0-9]+/))
      f += substr(line, RSTART + 21, RLENGTH - 21) + 0
    if (f < 1) f = 1
    return f
  }
  function unittest_ok(   k) {
    if (ran < 0) return
    k = 0
    if (match(line, /skipped=[0-9]+/)) k = substr(line, RSTART + 8, RLENGTH - 8) + 0
    run += ran; passed += ran - k; skipped += k; summaries++
    ran = -1
  }
  function unittest_failed(   f) {
    if (ran < 0) ran = 0
    f = ut_failures()
    if (f > ran) ran = f
    run += ran; passed += ran - f; failed += f; summaries++
    ran = -1
  }
  adapter == "alcotest" {
    if (line ~ /^Test Successful in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_success()
    else if (line ~ /^[0-9]+ failures?! in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_failure()
    else if (line ~ /^SKIP/) skipped++
    next
  }
  adapter == "differential" {
    if (line ~ /^differential: [0-9]+\/[0-9]+ corpus entries FAILED/) differential_fail()
    else if (line ~ /^differential: all [0-9]+ corpus entries agree/) differential_ok()
    else if (line ~ /^sbcl missing: differential harness skipped/) skip_marker = 1
    next
  }
  adapter == "make-test" {
    if (line ~ /^Test Successful in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_success()
    else if (line ~ /^[0-9]+ failures?! in [0-9.]+s\. [0-9]+ tests? run\.$/) alcotest_failure()
    else if (line ~ /^differential: [0-9]+\/[0-9]+ corpus entries FAILED/) differential_fail()
    else if (line ~ /^differential: all [0-9]+ corpus entries agree/) differential_ok()
    else if (line ~ /^COUNTS run=[0-9]+ passed=[0-9]+ failed=[0-9]+ skipped=[0-9]+$/) {
      r = p = f = k = 0
      for (i = 1; i <= NF; i++) {
        split($i, kv, "=")
        if (kv[1] == "run") r = kv[2]
        else if (kv[1] == "passed") p = kv[2]
        else if (kv[1] == "failed") f = kv[2]
        else if (kv[1] == "skipped") k = kv[2]
      }
      if (r == p + f + k) { run += r; passed += p; failed += f; skipped += k; summaries++ }
      # an inconsistent line is not a summary: the gate stays unparsed
      # rather than believing numbers that cannot describe one run
    }
    else if (line ~ /^Ran [0-9]+ tests? in /) { ran = $2 + 0 }
    else if (line ~ /^OK/) unittest_ok()
    else if (line ~ /^FAILED \(/) unittest_failed()
    else if (line ~ /^SKIP/) skipped++
    else if (line ~ /^store tests skipped \(postgres down/) skip_marker = 1
    else if (line ~ /^filmstrip_cdp: skip/) skip_marker = 1
    else if (line ~ /^sbcl missing: differential harness skipped/) skip_marker = 1
    next
  }
  # python -m unittest: "Ran N tests in Xs" then "OK"/"OK (skipped=S)" or
  # "FAILED (failures=F, errors=E)".
  adapter == "unittest" {
    if (line ~ /^Ran [0-9]+ tests? in /) { ran = $2 + 0 }
    else if (line ~ /^OK/) unittest_ok()
    else if (line ~ /^FAILED \(/) unittest_failed()
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
