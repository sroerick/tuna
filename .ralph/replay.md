# Tuna — replay completion (loop workspace, master)

Goal: finish `borg/replay.borg`'s two open stanzas — `§live` (live
replay, planned) and `§prim-versioning` (partial). Everything else in
that chapter is implemented. Read `AGENTS.md` + `SPEC.md` + the book
first (the book is the source of truth; code converges to it), then
read `borg/replay.borg`, `server/lib/replay.ml`, `server/lib/run.ml`,
`server/lib/api.ml`, `server/lib/prims.ml`, `store/lib/store.ml`.

## Ground rules

- `eval $(opam env --switch=poohstack --set-switch)` before anything.
- Gates: `dune build @all && dune runtest` green, `borge lint` clean,
  battery green (smoke-api, smoke-ui, verify-1..10), small commits,
  `git push origin master` after each green-gate commit.
- **CORE UNTOUCHED**: interpreter/, compiler/, common/ stay
  byte-identical (same law stdlib/forensics/accounts carry).
- README's battery line + the chapter stanza, nothing else in shared
  docs.

## CONCURRENT LOOP — isolation rules (load-bearing)

A sibling ralph loop works in the worktree `../tuna-eio` on branch
`rim-eio`: the Lwt -> eio rim port (store/, server/, bin). Rules:

- **Master is yours.** The sibling commits only to `rim-eio`.
- Files you may touch: `server/lib/replay.ml`, `server/lib/run.ml`,
  `server/lib/api.ml`, `server/lib/prims.ml`, `store/lib/store.ml`
  (only additive accessors), `tests/*` (additive), `borg/replay.borg`,
  `scripts/` (new script or a new verify-11), README battery line.
- Do NOT touch: `borg/rim-eio.borg`, `borg/federation.borg`,
  `store/lib/dune`, `store/lib/db.ml`, `store/lib/pgx_io.ml`,
  `server/bin/*`, `server/lib/dune` (the sibling owns the rim deps),
  `borg/accounts.borg`, `FINDINGS.md` (unless you OBSERVE a
  finding — then record it, that file is yours to add to).
- Stay LWT-STYLE on master: do not start the eio conversion; do not
  refactor db.ml/pool. Your delta is feature work in the Lwt rim.
- Ports/DB: the operator's dev server is on :18091 (token
  `/tmp/tuna-dev/bootstrap.token`, PG /tmp :5434 db tuna). Do NOT stop
  or clean PG. If you need a fresh server, boot on TUNA_HTTP_PORT=18096
  against the same DB and kill only what you started.

## Tasks (one or two per loop; tick + annotate)

## T1 Live replay [x]
- DONE. `Replay.live_replay` (server/lib/replay.ml) re-executes
  parent program+inputs through the SAME `Run.execute` boundary against
  the live host under caller `grants`; mints a NEW run row with
  `parent_run_id` = parent and NEVER writes `verify_status` (row is
  unverified). World diff is journal-versus-journal
  (`diff_journals`: align by seq on result_hash | error | prim | extra
  | missing; tree-shaped changes carry `first_diff_path`). Denied grant
  journals the denial. Surface: `POST /api/runs/:id/live-replay
  {"grants":[ids],"fuel","size_cap"}` -> {run, live_replayed_from,
  world_diff, contract_mismatches, journal} (api.ml `live_replay_run`,
  route registered before `/api/runs/:id`). Tests:
  tests/store_tests.ml m7 "live replay mints a linked unverified row +
  world diff" (incl. denied-grant journals). Acceptance:
  scripts/verify-11-replay-live.sh (green).

## T2 Prim-versioning: contract mismatch as first-class [x]
- DONE. `Replay.contract_mismatches` compares each row's pinned
  `j_prim_contract` to `Prims.contract` (single source, "2"; boundary
  `Run` pins it; no scattered literals — grep clean). `divergence`
  gained `recorded_contract` / `current_contract` / `contract_mismatch`;
  `verdict_json` surfaces them as first-class fields. Live replay
  surfaces `contract_mismatches[]` (seq, prim, recorded_contract,
  current_contract, recorded_build, summary). Faithful replay stays
  unconditional (recorded answers are data) — asserted in tests. Test:
  tests/store_tests.ml m7 "contract mismatch is first-class; faithful
  replay stays unconditional".

## T3 Receipts + book flip [x]
- DONE. `scripts/verify-11-replay-live.sh` written (4 sections: linked
  unverified row, changed-world diff, denied-grant journal, contract
  fields first-class) and run GREEN against a scratch server on :18096
  (same PG). README battery line now `verify-1..11`. borg/replay.borg
  flipped: project planned->implemented, §live planned->implemented,
  §prim-versioning partial->implemented; agent note rewritten.
  `borge lint` clean; `borge report` replay.borg 5 implemented / 0
  open. Full battery (verify-1..10, smoke-api, smoke-ui) + `dune build
  @all && dune runtest` green. Committed + pushed.

## Deviations
- (record reality divergences from this plan/chapter here)
- verify-11 uses `psql` to move the `prim_kv` world between the
  recorded run and the live replay (key_hash = sha256 of key ternary
  "10"); this is test scaffolding against the live dev DB, not a
  production path.
- The contract-mismatch test lives at the unit level (fabricated row);
  verify-11 asserts the field is present on the divergence surface and
  zero for current-contract rows (no way to fabricate an old-contract
  row through the HTTP boundary without cheating).
