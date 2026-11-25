#!/usr/bin/env python3
"""Turn the Rocq export output (export/*.v -> *.out) into two files:

  trees    one line per distinct tree: "<id> <ternary>" (0 leaf, 1 stem,
           2 fork - tuna's canonical encoding), id = first 16 hex chars
           of sha256 of the ternary string
  vectors  one line per vector: "<suite> <label> <program-id>
           <arg-id>[,<arg-id>...] <expected-id>"

Usage: convert.py OUTDIR SUITE=FILE.out [SUITE=FILE.out ...]"""
import hashlib, re, sys

out = sys.argv[1]
trees, vectors = {}, []

def tid(t):
    h = hashlib.sha256(t.encode()).hexdigest()[:16]
    assert trees.setdefault(h, t) == t, "id collision"
    return h

for arg in sys.argv[2:]:
    suite, path = arg.split("=", 1)
    for line in re.findall(r'"([^"]*)"', open(path).read()):
        label, prog, args, exp = line.split(" ")
        ids = ",".join(tid(a) for a in args.split(",")) if args else "-"
        vectors.append(f"{suite} {label} {tid(prog)} {ids} {tid(exp)}")

with open(f"{out}/trees", "w") as f:
    for h in sorted(trees):
        f.write(f"{h} {trees[h]}\n")
with open(f"{out}/vectors", "w") as f:
    f.write("\n".join(vectors) + "\n")
print(f"{len(trees)} trees, {len(vectors)} vectors")
