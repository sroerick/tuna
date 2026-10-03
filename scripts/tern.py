#!/usr/bin/env python3
"""tern.py — the ternary inspection tool. NEVER diff ternary by eye.

The canonical tree encoding (common/lib/canon.ml) is O(tree)-length,
self-similar, and structurally invisible; this session's debugging lost
real turns to eyeball diffs of long strings (a `first [7 9]` that IS 7
was read as garbage with the codebook in the same output).  Rule: pin a
REPL-computed twin and compare with `cmp`, or decode with `str`/`int`.
This tool makes that the path of least resistance.

Conventions (house codecs, mirrored exactly):
  canon     0 = Leaf, 1<child> = Stem, 2<left><right> = Fork
  cstr      string = list of chars; char = LSB-first bit list (high
            zeros stripped, nil = Leaf); bit false = Leaf, true = Stem
            Leaf  (common/lib/cstr.ml)
  law-5 int Fork(sign, magnitude) ; magnitude = LSB-first bool list,
            high zeros stripped; sign false = Leaf, true = Stem Leaf;
            canonical zero = Fork(Leaf, Leaf)  (common/lib/int_enc.ml)
  bool      false = Leaf (0), true = Stem Leaf (10)   (stdlib law 1)

AMBIGUITY (by design, the book's one-form law): Fork(Leaf, Leaf) is
int-zero, the 2-list [false false], AND a pair of leaves.  `str`/`int`
are best-effort decoders and say so; `pretty`/`sexpr` are the
unambiguous views.  For ground truth, compare against a REPL twin.

usage: tern.py <cmd> [arg]      (T = ternary arg, @file, or - stdin)
  pretty T    indented tree with node kinds
  sexpr T     one line: 0 | (s X) | (f X Y)
  str T       decode as a cstr string
  int T       decode as a law-5 int
  bool T      decode as a bool
  cmp A B     equality verdict; exit 0 equal, 1 differ, 2 error
  enc SEXPR   s-expr back to ternary (round-trip)
  selftest    offline codec checks + corpus round-trip
"""
import os
import sys


# ---------- canon parse / encode ----------

class Bad(Exception):
    pass


def parse(s, i=0):
    if i >= len(s):
        raise Bad(f"unexpected end of input at {i}")
    c = s[i]
    if c == "0":
        return ("leaf",), i + 1
    if c == "1":
        child, j = parse(s, i + 1)
        return ("stem", child), j
    if c == "2":
        left, j = parse(s, i + 1)
        right, k = parse(s, j)
        return ("fork", left, right), k
    raise Bad(f"unexpected character {c!r} at {i}")


def parse_exn(s):
    t, i = parse(s)
    if i != len(s):
        raise Bad(f"trailing characters after tree at {i}")
    return t


def enc(t):
    k = t[0]
    if k == "leaf":
        return "0"
    if k == "stem":
        return "1" + enc(t[1])
    return "2" + enc(t[1]) + enc(t[2])


# ---------- views ----------

def sexpr(t):
    k = t[0]
    if k == "leaf":
        return "0"
    if k == "stem":
        return "(s " + sexpr(t[1]) + ")"
    return "(f " + sexpr(t[1]) + " " + sexpr(t[2]) + ")"


def pretty(t, indent=0, out=None):
    if out is None:
        out = []
    pad = "  " * indent
    k = t[0]
    if k == "leaf":
        out.append(pad + "leaf")
    elif k == "stem":
        out.append(pad + "stem")
        pretty(t[1], indent + 1, out)
    else:
        out.append(pad + "fork")
        pretty(t[1], indent + 1, out)
        pretty(t[2], indent + 1, out)
    return out


# ---------- house codec decoders (best effort) ----------

def bits_of_list(t):
    """LSB-first bool list from a nil-terminated list; None if malformed."""
    out = []
    while True:
        if t == ("leaf",):
            return out
        if t[0] != "fork":
            return None
        b = t[1]
        if b == ("leaf",):
            out.append(0)
        elif b == ("stem", ("leaf",)):
            out.append(1)
        else:
            return None
        t = t[2]


def decode_int(t):
    """law-5 int: Fork(sign, LSB-first magnitude). None if not an int."""
    if t[0] != "fork":
        return None
    sign, mag = t[1], t[2]
    bits = bits_of_list(mag)
    if bits is None:
        return None
    v = 0
    for i, b in enumerate(bits):
        v |= b << i
    if sign == ("leaf",):
        return v
    if sign == ("stem", ("leaf",)):
        return -v
    return None


def decode_bool(t):
    if t == ("leaf",):
        return False
    if t == ("stem", ("leaf",)):
        return True
    return None


def decode_str(t):
    """cstr: list of chars; each char an LSB-first bit list. None if not."""
    out = []
    while True:
        if t == ("leaf",):
            return "".join(out)
        if t[0] != "fork":
            return None
        bits = bits_of_list(t[1])
        if bits is None:
            return None
        v = 0
        for i, b in enumerate(bits):
            v |= b << i
        if v > 255:
            return None
        out.append(chr(v))
        t = t[2]


# ---------- input handling ----------

def read_input(arg):
    if arg == "-":
        return sys.stdin.read().strip()
    if arg.startswith("@"):
        with open(arg[1:]) as f:
            return f.read().strip()
    return arg.strip()


def die(msg, code=2):
    print(f"tern: {msg}", file=sys.stderr)
    sys.exit(code)


# ---------- selftest ----------

def selftest():
    # law-5 vectors pinned from the live engine this session
    # (verified against the server AND the CL reference).
    for dec, t in [(0, "200"), (5, "20210202100"), (7, "202102102100"),
                   (8, "202020202100"), (9, "2021020202100")]:
        got = decode_int(parse_exn(t))
        assert got == dec, f"int {t}: got {got}, want {dec}"
        assert enc(parse_exn(t)) == t, f"round-trip {t}"
    assert decode_bool(parse_exn("0")) is False
    assert decode_bool(parse_exn("10")) is True
    # cstr: encode a few strings with the house shape and decode back
    def char_tree(v):
        if v == 0:
            return ("leaf",)
        bit = ("stem", ("leaf",)) if v & 1 else ("leaf",)
        return ("fork", bit, char_tree(v >> 1))

    def str_tree(s):
        t = ("leaf",)
        for ch in reversed(s):
            t = ("fork", char_tree(ord(ch)), t)
        return t

    for s in ["", "x", "write", "call the dentist", "todo-cal-v02e/d"]:
        st = str_tree(s)
        assert decode_str(st) == s, f"str round-trip {s!r}"
        assert enc(parse_exn(enc(st))) == enc(st)
    # ambiguity is real: law-5 zero == the 2-list [false false]
    assert decode_int(parse_exn("200")) == 0
    # corpus round-trip: every committed program field re-parses
    here = os.path.dirname(os.path.abspath(__file__))
    corpus = os.path.join(here, "diff-corpus")
    n = 0
    if os.path.isdir(corpus):
        for name in sorted(os.listdir(corpus)):
            if not name.endswith(".corpus"):
                continue
            for line in open(os.path.join(corpus, name)):
                if line.startswith("program ") or line.startswith("arg "):
                    body = line.split(None, 1)[1].strip()
                    assert enc(parse_exn(body)) == body, f"{name}: {body[:40]}"
                    n += 1
    print(f"selftest ok: law-5 vectors, bool, cstr round-trips, "
          f"{n} corpus fields re-parse clean")


# ---------- main ----------

def main():
    if len(sys.argv) < 2:
        die(__doc__)
    cmd = sys.argv[1]
    if cmd == "selftest":
        selftest()
        return
    if cmd == "enc":
        if len(sys.argv) != 3:
            die("enc needs an s-expr")
        print(enc(_sexpr_parse(sys.argv[2])))
        return
    if cmd == "cmp":
        if len(sys.argv) != 4:
            die("cmp needs two ternary inputs")
        a, b = read_input(sys.argv[2]), read_input(sys.argv[3])
        try:
            ta, tb = parse_exn(a), parse_exn(b)
        except Bad as e:
            die(f"parse: {e}")
        if ta == tb:
            print(f"EQUAL ({len(a)} ternary chars)")
            sys.exit(0)
        print(f"DIFFER (lens {len(a)} vs {len(b)})")
        sa, sb = sexpr(ta), sexpr(tb)
        # first structural divergence, on the s-expr view
        i = 0
        while i < min(len(sa), len(sb)) and sa[i] == sb[i]:
            i += 1
        print(f"  first divergence at s-expr char {i}:")
        print(f"    A: ...{sa[max(0, i - 20):i + 40]}")
        print(f"    B: ...{sb[max(0, i - 20):i + 40]}")
        sys.exit(1)
    if len(sys.argv) != 3:
        die(f"{cmd} needs one input (ternary, @file, or -)")
    src = read_input(sys.argv[2])
    try:
        t = parse_exn(src)
    except Bad as e:
        die(f"parse: {e}")
    if cmd == "pretty":
        print("\n".join(pretty(t)))
    elif cmd == "sexpr":
        print(sexpr(t))
    elif cmd == "str":
        s = decode_str(t)
        if s is None:
            die("not a cstr string under the house codec", 1)
        print(repr(s))
    elif cmd == "int":
        v = decode_int(t)
        if v is None:
            die("not a law-5 int under the house codec", 1)
        print(v)
    elif cmd == "bool":
        b = decode_bool(t)
        if b is None:
            die("not a bool (leaf/stem-leaf)", 1)
        print(b)
    else:
        die(f"unknown command {cmd!r}")


def _sexpr_parse(s):
    """s-expr back to tree: 0 | (s X) | (f X Y)."""
    toks = s.replace("(", " ( ").replace(")", " ) ").split()

    def go(it):
        tok = next(it)
        if tok == "0":
            return ("leaf",)
        if tok != "(":
            raise Bad(f"expected ( or 0, got {tok!r}")
        head = next(it)
        if head == "s":
            child = go(it)
            assert next(it) == ")"
            return ("stem", child)
        if head == "f":
            l = go(it)
            r = go(it)
            assert next(it) == ")"
            return ("fork", l, r)
        raise Bad(f"expected s or f, got {head!r}")

    t = go(iter(toks))
    return t


if __name__ == "__main__":
    main()
