#!/usr/bin/env python3
"""pytwin.py — the python twin of the common/ codecs the rim uses.

The migration (board_migrate.py) and page-parity probes (verify-13)
must build cstr string trees, law-5 int trees, and L1 board records
OUTSIDE the REPL: the reader's string literals carry no escapes, so a
quote-bearing legacy title can never round-trip through a sabra
literal.  This module is the explicit twin of:

  - Tuna.Cstr.encode   (common/lib/cstr.ml): per-byte char trees on a
    right spine, bytes UTF-8, char tree = little-endian bit list with
    Stem Leaf = 1 bit and Leaf = 0 bit, first byte outermost;
  - Tuna.Int_enc       (common/lib/int_enc.ml): positive n = Fork(Leaf,
    mag-bits), zero = Fork(Leaf, Leaf), bit bools as cstr's,
    magnitude LSB-first with the low bit the head;
  - the board L1 record (board.borg): the compiled norm of
    (todo-item state title who when) - plain forks, no reader sugar.

Never hand-decode ternary; this module only ENCODES, the same way the
OCaml rim does.  Parity (`pytwin.py parity [BASE]`) cross-checks every
builder against the live REPL on literal-expressible values and exits
non-zero on the first mismatch - run it before trusting a migration.
"""

import json
import os
import sys
import urllib.request

# -- ternary constructors ------------------------------------------------

LEAF = "0"


def stem(t):
    return "1" + t


def fork(a, b):
    return "2" + a + b


def bool_t(b):
    return stem(LEAF) if b else LEAF


# -- cstr ----------------------------------------------------------------

def char_tree(v):
    if v == 0:
        return LEAF
    return fork(bool_t(v & 1), char_tree(v >> 1))


def cstr_bytes(bs):
    """bytes -> cstr string tree ternary (Tuna.Cstr.encode)."""
    t = LEAF
    for i in range(len(bs) - 1, -1, -1):
        t = fork(char_tree(bs[i]), t)
    return t


def cstr(s):
    return cstr_bytes(s.encode("utf-8"))


# -- law-5 int ------------------------------------------------------------

def mag_bits(n):
    bits = []
    x = n
    while x:
        bits.append(x & 1)
        x >>= 1  # LSB-first, high zeros already absent
    return bits


def bits_tree(bits):
    t = LEAF
    for b in reversed(bits):
        t = fork(bool_t(b), t)
    return t


def int_pos(n):
    if n <= 0:
        raise ValueError("int_pos is for positives; zero is the canonical zero")
    return fork(LEAF, bits_tree(mag_bits(n)))


INT_ZERO = fork(LEAF, LEAF)


def int_of(n):
    if n == 0:
        return INT_ZERO
    if n > 0:
        return int_pos(n)
    raise ValueError("negative when-values are not a board field")


# -- L1 board record (board.borg) ------------------------------------------

KEY_STATE = "10"   # %10
KEY_TITLE = "110"  # %110
KEY_WHO = "1110"   # %1110
KEY_WHEN = "11110"  # %11110
STATE_OPEN = "10"  # %10
STATE_DONE = "0"   # %0


def kv(key, val):
    """(pair key (pair val %0)) as ternary."""
    return fork(key, fork(val, LEAF))


def record(state_b, title_b, who_b, when):
    """the compiled norm of (todo-item state title who when)."""
    return (fork(kv(KEY_STATE, STATE_OPEN if state_b == "open" else STATE_DONE),
                 fork(kv(KEY_TITLE, cstr_bytes(title_b)),
                      fork(kv(KEY_WHO, cstr_bytes(who_b)),
                           fork(kv(KEY_WHEN, int_of(when)), LEAF)))))


# -- parity battery ---------------------------------------------------------


def parity(base):
    """cross-check the builders against the live REPL; exit 1 on gap."""
    token = os.environ.get("TUNA_SMOKE_TOKEN") or open(
        "/tmp/tuna-dev/bootstrap.token").read().strip()
    if token.startswith("TUNA_BOOTSTRAP_TOKEN="):
        token = token.split("=", 1)[1]
    auth = {"Authorization": "Bearer " + token,
            "Content-Type": "application/json"}

    def tern(term):
        req = urllib.request.Request(
            base + "/api/repl",
            data=json.dumps({"command": "eval " + term}).encode(), headers=auth)
        return json.load(urllib.request.urlopen(req))["round"]["ternary"]

    checked = 0

    def eq(what, mine, term):
        nonlocal checked
        got = tern(term)
        if got != mine:
            print(f"pytwin parity FAIL {what}: py {mine} != repl {got}", file=sys.stderr)
            sys.exit(1)
        checked += 1

    for s in ["", "a", "hello board", "call the dentist",
              "slash / inside", "back\\slash", "line\nbreak",
              "día Ünicode ☃", "walk 1727780000", "x" * 300, "\x01"]:
        eq("cstr", cstr(s), '"' + s + '"')
    for n in [1, 2, 10, 127, 128, 255, 256, 1023,
              1727780000, 2 ** 65, 2 ** 70 + 3]:
        eq("int", int_of(n), str(n))
    # zero has TWO shapes (12.2 compat: the reader's bare 0 is Leaf);
    # the codec's canonical zero is what the stdlib builds.
    eq("int-zero", INT_ZERO, "(int-canonical 0)")
    # record parity for literal-expressible fields; quote-bearing titles
    # are exercised end-to-end by verify-13 through the page rim.
    eq("record", record("open", b"call the dentist", b"board-probe", 1727780000),
       '(todo-item todo-open "call the dentist" "board-probe" 1727780000)')
    eq("record-done", record("done", b"x", b"y", 1),
       '(todo-item todo-done "x" "y" 1)')
    print(f"pytwin parity: {checked} twins agree")
    return 0


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "parity":
        base = (sys.argv[2] if len(sys.argv) > 2
                else "http://127.0.0.1:" + os.environ.get("TUNA_HTTP_PORT", "18091"))
        sys.exit(parity(base))
    print(__doc__)
