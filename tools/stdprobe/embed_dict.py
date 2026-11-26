#!/usr/bin/env python3
"""stdlib/v1/trees -> OCaml module (stdlib/lib/std_trees.ml via a dune
rule): the frozen stdlib as (name, hash, ternary) rows, so offline front
ends resolve std names exactly as the manifest pins them
(borg/local.borg L1).  Usage: embed_dict.py <trees>
"""
import sys

out = ["(* GENERATED from stdlib/v1/trees by tools/stdprobe/embed_dict.py",
       "   via a dune rule - do not edit. (name, sha256, ternary) per def. *)",
       "", "let rows = ["]
for line in open(sys.argv[1]):
    name, h, ternary = line.split()
    assert all(c in "012" for c in ternary), name
    assert all(c in "0123456789abcdef" for c in h) and len(h) == 64, name
    out.append('  ("%s", "%s", "%s");' % (name, h, ternary))
out.append("]")
print("\n".join(out))
