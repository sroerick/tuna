# Jay's typing rules as programs (vendored trees)

Source: Barry Jay, *Typing Rules Become Programs in Tree Calculus*,
draft of 2026-11-25, <https://github.com/barry-jay-personal/typing-rules-as-programs>
at commit `ddbab88b57cd1a010188dafd5666fd8f710671c2`. MIT licensed;
`LICENSE` is his. Everything in `trees` is his code, as trees.

In that development types are trees and the type checker is a tree
program: `infer O @ t` evaluates to `Sm T` (t has type T) or `Lf`
(failure), for an ontology `O` of declared abstract types. His values
are plain tree-calculus trees under the same five rules tuna runs, so
tuna runs his checker unchanged.

Files:

- `export/ExportBool.v`, `export/ExportLists.v` — ours. They import his
  modules and print his programs, arguments and expected results in
  tuna's ternary encoding (0 leaf, 1 stem, 2 fork). Each vector is one
  of his proved `Example`s (booleans.v, lists.v, self_typing.v): the
  expected result is what his proof establishes, not something we
  computed.
- `export/TyInt.v` — ours: a binary `Int` declared in his framework
  (wrapped type over `Bool * List Bool`), with `O_int_sound` proved
  from his `wrap_decl_sound`; `export/ExportInt.v` prints its proved
  Examples. `export/ExportConcrete.v` prints his `is_concrete`.
  `export/TyBin.v` — ours: binary naturals `Bin` (two successors, one
  tag per bit) with our soundness proof `bin_interp_sound`, and `Int`
  wrapped over `Bool * Bin`; `export/ExportBin.v` prints its Examples.
- `trees` — `<id> <ternary>`, each distinct tree once (id = first 16 hex
  of sha256 of the ternary string; a label, not tuna's tree hash).
- `vectors` — `<suite> <label> <program> <args,...> <expected>` by id.
  Applying `program` to `args` in tuna must give `expected`.
- `convert.py`, `regen.sh` — provenance: rebuild from the pinned commit
  (needs Rocq; ~40 min cold). Not part of the build.

Ontologies used: `O_ex` (booleans.v: Bool, Nat), `O_list` (lists.v:
adds List[A], updates) our `O_int` (O_list plus a wrapped Int) and `O_bin` (O_list plus Bin
and Int). His full `paper_ontology` is not exported yet.

Checked by `tests/jay_tests.ml` (see the tuna metatheory chapter, rung 4).

fast3 (borg/eval-modes.borg E3) registers some of these trees natively:
`natives` lists them as `<name> <ternary>` (his helpers by hash; infer,
tp and row at base parameter Lf and with each parameter set to K, from
which the engine derives their templates), written by
`export/ExportNatives.v`.  `export/ExportHelpers.v` prints suite
"helpers": his helper programs (equal, is_concrete, get_tagged,
get_label, resv, row_prog, rows) against the values his spec lemmas
state.
