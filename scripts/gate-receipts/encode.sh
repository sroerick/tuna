#!/usr/bin/env bash
# encode.sh - build the gate-receipt input ternaries (bash + awk only).
#
#   encode.sh observation   -> the observation record ternary
#   encode.sh skip          -> the skip-allowed list ternary
#   encode.sh cstr          -> stdin text as the claims.sh cstr ternary
#                              (the one string codec; each input line gets
#                              its newline, matching claims.sh's encoder)
#
# The observation is the chapter's dialect v1.1 record: a list of
# [key value] two-lists, sorted by key, where key n is the small-nat tag
# Stem^n Leaf and values use the book's codecs:
#
#   1 gate      cstr    2 project  cstr    3 command  cstr
#   4 rev       cstr    5 tree_clean law-5  6 diff_hash cstr
#   7 exit      law-5   8 log_hash cstr    9 counts   [run passed failed
#                                                     skipped] of law-5
#                                                     ints, or tag 1
#                                                     (%10) for unparsed
#  10 adapter   cstr   11 runner  cstr    12 started_at cstr
#
# Values arrive as environment variables (G_*) so spaces and newlines
# survive; nothing is passed through a line-oriented interface.
# cstr is the book's one string codec (common/lib/cstr.ml): a list of
# bytes, each a little-endian bit list.  law-5 ints are
# fork(false, magnitude-bits), canonical zero fork(false,nil) = %200.
# No python, no jq.

set -eu
mode="${1:-}"
case "$mode" in
  observation|skip|cstr) : ;;
  *) echo "usage: encode.sh observation|skip|cstr" >&2; exit 2 ;;
esac

LC_ALL=C awk -v mode="$mode" '
  function pair(a, b) { return "2" a b }

  # tag n = Stem^n Leaf = n "1" digits then "0"
  function tag(n,   i, s) {
    s = "0"
    for (i = 0; i < n; i++) s = "1" s
    return s
  }

  # byte v as the cstr little-endian bit list (LSB first)
  function ctree(v,   arr, n, j, out) {
    n = 0
    while (v > 0) { arr[n++] = (v % 2 == 1 ? "10" : "0"); v = int(v / 2) }
    out = "0"
    for (j = n - 1; j >= 0; j--) out = "2" arr[j] out
    return out
  }

  # string -> list of byte trees
  function cstr(s,   i, out) {
    out = ""
    for (i = 1; i <= length(s); i++) out = out "2" ctree(ORD[substr(s, i, 1)])
    return out "0"
  }

  # nonnegative decimal -> canonical law-5 int tree
  function int_enc(s,   v, bits, n, i, mag) {
    v = s + 0
    if (v == 0) return "200"
    n = 0
    while (v > 0) { bits[n++] = (v % 2 == 1 ? "10" : "0"); v = int(v / 2) }
    mag = "0"
    for (i = n - 1; i >= 0; i--) mag = "2" bits[i] mag
    return "20" mag
  }

  # [key value] two-list
  function entry(k, v) { return pair(tag(k), pair(v, "0")) }

  # list of e[1..n]; n = 0 is the leaf
  function lst(n, e,   i, out) {
    out = "0"
    for (i = n; i >= 1; i--) out = pair(e[i], out)
    return out
  }

  BEGIN {
    for (i = 0; i < 256; i++) ORD[sprintf("%c", i)] = i

    if (mode == "cstr") {
      cstr_mode = 1
    } else if (mode == "skip") {
      n = 0
      if (ENVIRON["G_SKIP"] != "") {
        k = split(ENVIRON["G_SKIP"], s, "\n")
        for (i = 1; i <= k; i++) if (s[i] != "") e[++n] = cstr(s[i])
      }
      print lst(n, e)
      exit
    } else {
      counts = ENVIRON["G_COUNTS"]
      if (counts == "unparsed") {
        ctree_v = tag(1)
      } else {
        k = split(counts, c, " ")
        if (k != 4) { print "encode.sh: counts must be 4 ints or unparsed" > "/dev/stderr"; exit 3 }
        for (i = 1; i <= 4; i++) cv[i] = int_enc(c[i])
        ctree_v = lst(4, cv)
      }

      e[1]  = entry(1,  cstr(ENVIRON["G_GATE"]))
      e[2]  = entry(2,  cstr(ENVIRON["G_PROJECT"]))
      e[3]  = entry(3,  cstr(ENVIRON["G_COMMAND"]))
      e[4]  = entry(4,  cstr(ENVIRON["G_REV"]))
      e[5]  = entry(5,  int_enc(ENVIRON["G_TREE_CLEAN"]))
      e[6]  = entry(6,  cstr(ENVIRON["G_DIFF_HASH"]))
      e[7]  = entry(7,  int_enc(ENVIRON["G_EXIT"]))
      e[8]  = entry(8,  cstr(ENVIRON["G_LOG_HASH"]))
      e[9]  = entry(9,  ctree_v)
      e[10] = entry(10, cstr(ENVIRON["G_ADAPTER"]))
      e[11] = entry(11, cstr(ENVIRON["G_RUNNER"]))
      e[12] = entry(12, cstr(ENVIRON["G_STARTED_AT"]))
      print lst(12, e)
      exit
    }
  }

  cstr_mode { all = all $0 "\n" }

  END { if (cstr_mode) print cstr(all) }
'
