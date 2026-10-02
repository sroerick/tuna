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

json.dump(defs, open("/tmp/stdlib_defs.json", "w"))
print(len(defs), "defs ok")
