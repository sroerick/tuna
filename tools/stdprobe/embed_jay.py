#!/usr/bin/env python3
"""reference/jay-typing-rules/natives -> OCaml module (interpreter/lib/
jay_trees.ml via a dune rule): the trees of Barry Jay's checker that
fast3 registers (borg/eval-modes.borg E3), as canonical ternary.
Usage: embed_jay.py <natives>"""
import sys

out = ["(* GENERATED from reference/jay-typing-rules/natives by",
       "   tools/stdprobe/embed_jay.py via a dune rule - do not edit. *)",
       "", "let all = ["]
for line in open(sys.argv[1]):
    name, ternary = line.split()
    assert all(c in "012" for c in ternary), name
    out.append('  ("%s", "%s");' % (name, ternary))
out.append("]")
print("\n".join(out))
