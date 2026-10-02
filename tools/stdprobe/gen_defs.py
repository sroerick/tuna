#!/usr/bin/env python3
"""Generate the sabra stdlib v1 defs (JSON for the probe; the .defs
transcript + manifest are derived from the same term trees).

DSL: python lists are applications, strings are atoms/%literals.
lam(p, body) = (lambda (p) body); dispatch(la, sa, fa, v) =
((tree-case la sa fa) v). Everything serializes with balanced parens.
"""
import json


def ser(x):
    if isinstance(x, str):
        return x
    return "(" + " ".join(ser(y) for y in x) + ")"


def L(*xs):
    return list(xs)


def P(*ps):
    return list(ps)


def lam(p, body):
    return L("lambda", P(p), body)


def lam2(p, q, body):
    return L("lambda", P(p), L("lambda", P(q), body))


def dispatch(leaf_arm, stem_arm, fork_arm, var):
    return L(L("tree-case", leaf_arm, stem_arm, fork_arm), var)


defs = []


def d(name, term):
    src = ser(term)
    assert src.count("(") == src.count(")"), (name, src)
    defs.append((name, src))


# ---- L0 pins (verbatim-port rule) ----
d("true", "%10")
d("false", "%0")
d("not", "%22102000")

# ---- L1 structure ----
d("tree-case", lam("la", lam("lb", lam("lf", L("pair", L("pair", "la", "lb"), "lf")))))

d("is-leaf", lam("t", dispatch("%10", lam("c", "%0"), lam("l", lam("r", "%0")), "t")))
d("is-stem", lam("t", dispatch("%0", lam("c", "%10"), lam("l", lam("r", "%0")), "t")))
d("is-fork", lam("t", dispatch("%0", lam("c", "%0"), lam("l", lam("r", "%10")), "t")))

d("if", lam("t", lam("f", L("pair", L("pair", "f", L("pair", "%0", "t")), "%0"))))

d("bool-and", lam("a", lam("b", dispatch("%0", lam("c", "b"), lam("l", lam("r", "%0")), "a"))))
d("bool-or", lam("a", lam("b", dispatch("b", lam("c", "%10"), lam("l", lam("r", "%0")), "a"))))
d("bool-xor", lam("x", lam("y", L(L("if", L("not", "y"), "y"), "x"))))

d("first", lam("p", dispatch("%0", lam("c", "%0"), lam("l", lam("r", "l")), "p")))
d("second", lam("p", dispatch("%0", lam("c", "%0"), lam("l", lam("r", "r")), "p")))
d("is-zero", lam("n", dispatch("%10", lam("c", "%0"), lam("l", lam("r", "%0")), "n")))
d("nat-pred", lam("n", dispatch("%0", lam("c", "c"), lam("l", lam("r", "l")), "n")))

# ---- runtime-recursion machinery (upstream wait/k1 barriers) ----
d("sa-k", lam("x", L("x", L("true", "x"))))
d("wait", lam("a", lam("b", lam("c",
  L(L(L("%0", L("%0", "a")), L("true", "c")), "b")))))
d("wait1", lam("a", L("%0", L("%0",
  L(L("%0", L("%0", L("true", L("%0", L("%0", "a"))))), "true")))))
d("rec-fix", lam("functional", L(L("wait", "sa-k"),
  lam("x", L("functional", L("wait1", "sa-k", "x"))))))

# ---- L2 lists (fold-right; recursion via the functional's self) ----
d("fold-fn", lam("self", lam("xs", lam("f", lam("z",
  dispatch("z",
           lam("c", "z"),
           lam("hd", lam("tl", L("f", "hd", L("self", "tl", "f", "z")))),
           "xs"))))))
d("list-fold", lam("f", lam("z", lam("xs",
  L(L(L(L("rec-fix", "fold-fn"), "xs"), "f"), "z")))))

d("list-length", lam("xs",
  L("list-fold", lam("h", lam("acc", L("%0", "acc"))), "%0", "xs")))
d("list-append", lam("xs", lam("ys",
  L("list-fold", lam("h", lam("acc", L("pair", "h", "acc"))), "ys", "xs"))))
d("list-reverse", lam("xs",
  L("list-fold", lam("h", lam("acc",
    L("list-append", "acc", L("pair", "h", "%0")))), "%0", "xs")))
d("list-map", lam("g", lam("xs",
  L("list-fold", lam("h", lam("acc", L("pair", L("g", "h"), "acc"))), "%0", "xs"))))

# list-ref: countdown recursion in tree-case ARM BODIES only (compile
# reduction is eager in application ARGUMENT positions); dispatch on
# the index itself so a branch is selected before any recursion fires.
REF_FN = lam("self", lam("i", lam("xs",
  dispatch("%0",
           lam("c", "%0"),
           lam("hd", lam("tl",
             dispatch(L("%0", "hd"),
                      lam("c", L("self", "c", "tl")),
                      lam("u", lam("v", "%0")),
                      "i"))),
           "xs"))))
d("ref-fn", REF_FN)
d("list-ref", lam("i", lam("xs", L(L("rec-fix", "ref-fn"), "i", "xs"))))

# nat recursion sanity engine (also the F6 canary)
d("sum-fn", lam("self", lam("n",
  dispatch("%0", lam("c", L("%0", L("self", "c"))), lam("l", lam("r", "%0")), "n"))))
d("nat-sum", L("rec-fix", "sum-fn"))

# ---- structural equality ----
EQ_LEAF = L("is-leaf", "b")
EQ_STEM = lam("c", dispatch("%0",
  lam("c2", L("self", "c", "c2")), lam("u", lam("v", "%0")), "b"))
EQ_FORK = lam("l", lam("r", dispatch("%0",
  lam("c", "%0"),
  lam("l2", lam("r2", L("bool-and", L("self", "l", "l2"), L("self", "r", "r2")))),
  "b")))
d("eq-fn", lam("self", lam("a", lam("b", dispatch(EQ_LEAF, EQ_STEM, EQ_FORK, "a")))))
d("tree-eq", L("rec-fix", "eq-fn"))
d("list-eq", lam("xs", lam("ys", L("tree-eq", "xs", "ys"))))

# ---- L3 ints (law 5: sign-magnitude, fork(sign-bool, mag-bits)) ----
# magnitude = bit list, LSB first; canonical = high-order false bits stripped.
# mag-canonical: fold-right, acc = the canonical TAIL (fold is right, so acc
# is the MORE significant side). acc nil = "no significant bits seen yet":
# hd true -> [true]; hd false or junk -> nil (this strips top false bits).
MAG_F = lam2("hd", "acc",
  dispatch(
    dispatch("%0", lam("c", L("pair", "%10", "%0")), lam2("u", "v", "%0"), "hd"),
    lam("c2", L("pair", "hd", "acc")),
    lam2("ua", "va", L("pair", "hd", "acc")),
    "acc"))
d("mag-canonical", lam("bits", L("list-fold", MAG_F, "%0", "bits")))

# int-canonical: fork(sign-bool, m) with m = mag-canonical mag; m nil
# -> both zeros one form (fork false nil), sign dropped. canonical sign
# = is-stem (bool collision law: fork junk reads positive). junk int
# (leaf/stem at top) -> canonical zero.
d("int-canonical", lam("n",
  dispatch(L("pair", "%0", "%0"),
           lam("c", L("pair", "%0", "%0")),
           lam2("sign", "mag",
             L(lam("m",
               dispatch(L("pair", "%0", "%0"),
                        lam("c2", L("pair", L("is-stem", "sign"), "m")),
                        lam2("u2", "v2", L("pair", L("is-stem", "sign"), "m")),
                        "m")),
               L("mag-canonical", "mag"))),
           "n")))

# int constants (alias defs, same discipline as true/false); both
# canonical from birth.
d("int-zero", "%200")
d("int-one", "%202100")

# int-neg: canonicalize, then flip a nonzero sign. zero in -> %200 out
# (never mints -0). junk -> canonicalize first, so junk -> +0 -> +0.
d("int-neg", lam("n",
  L(lam("c",
    dispatch(L("pair", "%0", "%0"),
             lam("j", L("pair", "%0", "%0")),
             lam2("s", "m",
               dispatch(L("pair", "%0", "%0"),
                        lam("j2", L("pair", L("not", "s"), "m")),
                        lam2("u", "v", L("pair", L("not", "s"), "m")),
                        "m")),
             "c")),
    L("int-canonical", "n"))))

# mag-ripple: add one carry bit into an LSB-first magnitude. c junk ->
# 0, xs junk -> nil+delta. Recursion only in dispatch arm bodies.
RIP_LEAF = dispatch("%0", lam("jc", L("pair", "%10", "%0")),
                    lam2("u1", "v1", "%0"), "c")
RIP_FORK = lam2("hd", "tl",
  dispatch(L("pair", "hd", "tl"),
    lam("jc", dispatch(L("pair", "%10", "tl"),
                       lam("jh", L("pair", "%0", L("self", "tl", "%10"))),
                       lam2("u2", "v2", L("pair", "%10", "tl")),
                       "hd")),
    lam2("u3", "v3", L("pair", "hd", "tl")),
    "c"))
d("mag-ripple-fn", lam("self", lam("xs", lam("c",
  dispatch(RIP_LEAF, lam("jx", RIP_LEAF), RIP_FORK, "xs")))))
d("mag-ripple", lam("xs", lam("c",
  L(L("rec-fix", "mag-ripple-fn"), "xs", "c"))))

# mag-add: full adder. per position: out = xor3(ahd,bhd,c),
# carry' = majority(ahd,bhd,c) = (a&b) | (c & (a^b)). Result through
# mag-canonical so a zero sum is nil, not [f...].
MA_SUMBIT = L("bool-xor", L("bool-xor", "ahd", "bhd"), "c")
MA_CARRY = L("bool-or", L("bool-and", "ahd", "bhd"),
             L("bool-and", "c", L("bool-xor", "ahd", "bhd")))
MA_BFORK = lam2("bhd", "btl",
  L("pair", MA_SUMBIT, L("self", "atl", "btl", MA_CARRY)))
MA_AFORK = lam2("ahd", "atl",
  dispatch(L("mag-ripple", L("pair", "ahd", "atl"), "c"),
           lam("jb", L("mag-ripple", L("pair", "ahd", "atl"), "c")),
           MA_BFORK,
           "b"))
d("mag-add-fn", lam("self", lam("a", lam("b", lam("c",
  dispatch(L("mag-ripple", "b", "c"),
           lam("ja", L("mag-ripple", "b", "c")),
           MA_AFORK,
           "a"))))))
d("mag-add", lam("a", lam("b",
  L("mag-canonical",
    L(L(L("rec-fix", "mag-add-fn"), "a"), "b", "%0")))))

# mag-cmp: three-way over LSB-first lists -> small nat (0 lt, 1 eq, 2
# gt). Recursion reaches the MSB end first; the deeper verdict wins
# unless eq, then the current position decides. One list exhausted: a
# true bit anywhere in the other's rest decides, else eq (trailing false
# bits inert). Junk stems read as 0.
MC_ANY = lambda v: L("list-fold", lam2("h", "acc", L("bool-or", "h", "acc")), "%0", v)
MC_A_LEAF = L(L("if", "%0", "%10"), MC_ANY("b"))
# tree-case law at work: only the LEAF arm may be a bare value (it is
# returned plain); stem/fork arms are APPLIED to the parts, so a value
# there gets applied to the child - must be a lambda absorbing it.
MC_B0 = dispatch("%10", lam("jb1", "%0"), lam2("j1", "j2", "%10"), "bhd")
MC_B1 = dispatch("%110", lam("jb2", "%10"), lam2("j3", "j4", "%110"), "bhd")
MC_BIT = dispatch(MC_B0, lam("jb0", MC_B1), lam2("j5", "j6", "%10"), "ahd")
MC_BFORK = lam2("bhd", "btl",
  dispatch("%0",
    lam("c", dispatch(MC_BIT, lam("j7", "%110"), lam2("j8", "j9", MC_BIT), "c")),
    lam2("j10", "j11", MC_BIT),
    L("self", "atl", "btl")))
MC_AFORK = lam2("ahd", "atl",
  dispatch(L(L("if", "%110", "%10"), MC_ANY("a")),
           lam("jb3", L(L("if", "%110", "%10"), MC_ANY("a"))),
           MC_BFORK,
           "b"))
d("mag-cmp-fn", lam("self", lam("a", lam("b",
  dispatch(MC_A_LEAF, lam("j12", MC_A_LEAF), MC_AFORK, "a")))))
d("mag-cmp", lam("a", lam("b", L(L("rec-fix", "mag-cmp-fn"), "a", "b"))))

# mag-ripsub: subtract a single 1 from an LSB-first magnitude (borrow
# ripple; the mag-ripple twin). 0-borrow case is handled in mag-sub
# directly (the result passes mag-canonical, so interim non-canonical
# forms are fine).
RS_FORK = lam2("hd", "tl",
  dispatch(L("pair", "%10", L("self", "tl")),
           lam("jh", L("pair", "%0", "tl")),
           lam2("rj1", "rj2", L("pair", "%0", "tl")),
           "hd"))
d("mag-ripsub-fn", lam("self", lam("xs",
  dispatch("%0", lam("rj3", "%0"), RS_FORK, "xs"))))
d("mag-ripsub", lam("xs",
  L("mag-canonical", L(L("rec-fix", "mag-ripsub-fn"), "xs"))))

# mag-sub: full subtractor. per position: diff = a^(b^w),
# borrow' = (b&w) | (~a & (b|w)). CALLER CONTRACT: a >= b (via mag-cmp);
# violating it reads as 0 past the point a runs out. Result through
# mag-canonical (law 5).
MS_DIFF = L("bool-xor", "ahd", L("bool-xor", "bhd", "w"))
MS_BORROW = L("bool-or", L("bool-and", "bhd", "w"),
              L("bool-and", L("not", "ahd"), L("bool-or", "bhd", "w")))
MS_BNIL = dispatch("a", lam("rj4", L("mag-ripsub", "a")),
                   lam2("rj5", "rj6", L("mag-ripsub", "a")), "w")
MS_BFORK = lam2("bhd", "btl",
  L("pair", MS_DIFF, L("self", "atl", "btl", MS_BORROW)))
MS_AFORK = lam2("ahd", "atl",
  dispatch(MS_BNIL, lam("rj7", MS_BNIL), MS_BFORK, "b"))
d("mag-sub-fn", lam("self", lam("a", lam("b", lam("w",
  dispatch("%0", lam("rj8", "%0"), MS_AFORK, "a"))))))
d("mag-sub", lam("a", lam("b",
  L("mag-canonical",
    L(L(L("rec-fix", "mag-sub-fn"), "a"), "b", "%0")))))

# int-add: canonicalize both, then signs. same sign -> mag-add, keep it;
# opposite signs -> mag-cmp picks subtrahend direction + result sign, eq
# lands on the single zero form. Signs are canonical bools post
# int-canonical, so same-sign = not (bool-xor sa sb).
IA_ADDVAL = L("pair", "sa", L("mag-add", "ma", "mb"))
IA_GTVAL = L("pair", "sa", L("mag-sub", "ma", "mb"))
IA_LTVAL = L("pair", "sb", L("mag-sub", "mb", "ma"))
IA_EQGT = lam("c", dispatch(L("pair", "%0", "%0"),
                            lam("j3", IA_GTVAL),
                            lam2("j4", "j5", IA_GTVAL),
                            "c"))
IA_SUBVAL = dispatch(IA_LTVAL, IA_EQGT, lam2("j6", "j7", IA_GTVAL),
                     L("mag-cmp", "ma", "mb"))
IA_BFORK = lam2("sb", "mb",
  L(L("if", IA_ADDVAL, IA_SUBVAL), L("not", L("bool-xor", "sa", "sb"))))
IA_FORKA = lam2("sa", "ma",
  dispatch("ca", lam("j8", "ca"), IA_BFORK, "cb"))
IA_CAWORK = dispatch("cb", lam("j0", "cb"), IA_FORKA, "ca")
d("int-add", lam("a", lam("b",
  L(lam("ca", L(lam("cb", IA_CAWORK), L("int-canonical", "b"))),
    L("int-canonical", "a")))))

# int-sub: add + neg (the chapter's own words). Canonicity inherited.
d("int-sub", lam("a", lam("b",
  L("int-add", "a", L("int-neg", "b")))))

# int-cmp: small-nat three-way (0 lt, 1 eq, 2 gt), signs first. diff
# signs -> sa decides (negative loses); same sign -> mag-cmp, magnitude
# args flipped when both negative.
IC_FORKB = lam2("sb", "mb",
  L(L("if", L(L("if", L("mag-cmp", "mb", "ma"), L("mag-cmp", "ma", "mb")), "sa"),
       L(L("if", "%0", "%110"), "sa")),
    L("not", L("bool-xor", "sa", "sb"))))
IC_FORKA = lam2("sa", "ma",
  dispatch("%10", lam("jc", "%10"), IC_FORKB, "cb"))
IC_CAWORK = dispatch("%10", lam("jc2", "%10"), IC_FORKA, "ca")
d("int-cmp", lam("a", lam("b",
  L(lam("ca", L(lam("cb", IC_CAWORK), L("int-canonical", "b"))),
    L("int-canonical", "a")))))

json.dump(defs, open("/tmp/stdlib_defs.json", "w"))
print(len(defs), "defs ok")
