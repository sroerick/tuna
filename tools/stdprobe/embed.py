#!/usr/bin/env python3
"""stdlib/v1/core.defs -> OCaml module (server/lib/stdlib_embed.ml via
dune rule). Reads the REPL transcript grammar only (def NAME TERM
records, blank-line separated, # comments) - no second grammar, ever.
Usage: embed.py <core.defs>  (module on stdout)
"""
import sys


def esc(s):
    return s.replace("\\", "\\\\").replace('"', '\\"')


def main():
    path = sys.argv[1]
    records = []
    for block in open(path).read().split("\n\n"):
        lines = [l.strip() for l in block.splitlines()
                 if l.strip() and not l.strip().startswith("#")]
        if not lines:
            continue
        rec = " ".join(lines)
        assert rec.startswith("def "), rec
        _, name, term = rec.split(" ", 2)
        records.append((name, term))
    out = []
    out.append("(* GENERATED from stdlib/v1/core.defs by")
    out.append("   tools/stdprobe/embed.py via a dune rule - do not edit. *)")
    out.append("")
    out.append('let identity_name = "sabralib"')
    out.append("")
    out.append("let defs = [")
    for name, term in records:
        out.append('  ("%s", "%s");' % (esc(name), esc(term)))
    out.append("]")
    print("\n".join(out))


main()
