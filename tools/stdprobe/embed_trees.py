#!/usr/bin/env python3
"""stdlib/v1/trees -> OCaml module (interpreter/lib/accel_trees.ml via a
dune rule): the frozen stdlib trees by hash, for the accelerator
registry (borg/eval-modes.borg E2).  The interpreter cannot depend on
the compiler, so the trees arrive as canonical ternary, frozen by
tools/stdprobe/freeze.ml.  Usage: embed_trees.py <trees>
"""
import sys

out = ["(* GENERATED from stdlib/v1/trees by tools/stdprobe/embed_trees.py",
       "   via a dune rule - do not edit. (hash, ternary) per stdlib def. *)",
       "", "let all = ["]
for line in open(sys.argv[1]):
    name, h, ternary = line.split()
    assert all(c in "012" for c in ternary), name
    out.append('  ("%s", "%s"); (* %s *)' % (h, ternary, name))
out.append("]")
print("\n".join(out))
