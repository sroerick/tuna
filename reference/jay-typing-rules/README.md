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
- `trees` — `<id> <ternary>`, each distinct tree once (id = first 16 hex
  of sha256 of the ternary string; a label, not tuna's tree hash).
- `vectors` — `<suite> <label> <program> <args,...> <expected>` by id.
  Applying `program` to `args` in tuna must give `expected`.
- `convert.py`, `regen.sh` — provenance: rebuild from the pinned commit
  (needs Rocq; ~40 min cold). Not part of the build.

Ontologies used: `O_ex` (booleans.v: Bool, Nat), `O_list` (lists.v:
adds List[A], updates) and our `O_int` (O_list plus Int). His full `paper_ontology` is not exported yet.

Checked by `tests/jay_tests.ml` (see the tuna metatheory chapter, rung 4).
