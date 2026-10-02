# Tuna — rim-eio (loop workspace, branch rim-eio, worktree ../tuna-eio)

Goal: implement `borg/rim-eio.borg` end to end — migrate tuna1's rim
from Lwt to eio, in place, as the purity thesis's declared stage.
This is a **mechanical port with byte-identical behavior**, refereed by
the EXISTING battery — the migration invents no new acceptance
machinery, and the CORE IS UNTOUCHED BY CONSTRUCTION (only the rim
re-binds).

Read `AGENTS.md` + `SPEC.md` + the book first, then the chapter
`borg/rim-eio.borg` (follow it verbatim), then:
- PP's playbook: `/home/roerick/dev/wyo.tech/pricklypear/archive/plans/eio-floor-checklist.md`
- PP's shapes: `/home/roerick/dev/wyo.tech/pricklypear/image/lib/io_eio.ml`
  (pure pgx over Eio.Net), `.../image/lib/http_kit.ml`, `.../image/lib/http_core.ml`
  (same owner, ISC — port utilities from these; do not invent).

## Ground rules

- `eval $(opam env --switch=poohstack --set-switch)` before anything.
- You are in a GIT WORKTREE at `/home/roerick/dev/wyo.tech/tuna-eio` on
  branch `rim-eio` (forked from master `ee80a63`). Commit ALL work to
  `rim-eio`; NEVER checkout/merge/rebase/push master. Push the branch
  when green: `git push -u origin rim-eio` (the host merges).
- Gates per stage: `dune build @all && dune runtest` green (each stage
  leaves this true); the chapter's acceptance battery at the end:
  pure suites + PG suites + differential 24/24 + smoke-api 14/14 +
  smoke-ui 17/17 + verify-1..10 10/10, AND every step count unchanged.
- **FAMILY LAW (chapter, non-negotiable): direct-style everywhere, NO
  Lwt_eio bridge, NO dual rims, NO runtime flag — ever.** PP Phase 5
  deleted the bridge and the flag ON EVIDENCE; do not resurrect them.
- **Lwt DEPARTS THE DEPS**: store/ and server/ dune deps lose lwt.
  `Prim_eval.Make` stays untouched as the corpus referee (its monad
  parameter goes to `Id` in tests — nothing there binds Lwt).
- CORE UNTOUCHED: interpreter/, compiler/, common/ diff-empty across
  the whole series.

## ISOLATION (load-bearing)

The replay sibling loop finished BEFORE you (live replay +
prim-versioning landed on master `5ee8323`; your branch is based on
it), so there is no concurrent writer. Still:

- **Work on branch `rim-eio` ONLY.** Never checkout/merge/rebase/push
  master; the host merges. Push the branch when green.
- You own the RIM: dune deps, `store/lib/db.ml`, `store/lib/pgx_io.ml`,
  `server/bin/*`, `server/lib/dune`, `borg/rim-eio.borg`. You WILL
  touch `api.ml`/`run.ml`/`replay.ml` for the port (Lwt types ->
  direct style) — keep those diffs STYLE-ONLY (mechanical Lwt
  removal), never semantic: replay.ml now carries fresh live-replay +
  contract-mismatch logic that must survive the port byte-for-byte in
  behavior. If a semantic change is unavoidable, record it under
  Deviations loudly.
- Ports/DB: TAKEN on this machine — PG 5433/5434/5435/5436; HTTP 18080
  (PP), 18090 (glochid), 18091 (master dev), 18092 (pp-slice demo).
  **YOUR slots: PG :5437, HTTP :18097.**
  Your env (source it in every shell; write `scripts/eio-env.sh`):
  `TUNA_DB_HOST=/tmp TUNA_DB_PORT=5437 TUNA_DB_NAME=tuna
   TUNA_DB_USER=tuna TUNA_DATA_DIR=/tmp/tuna-eio-pg
   TUNA_DEV_DIR=/tmp/tuna-eio-dev TUNA_HTTP_PORT=18097`
  `scripts/dev.sh` + `scripts/test-store.sh` honor all TUNA_* knobs.
- First `dune build @all` here is COLD; expect minutes.
- Kill only processes YOU spawned.

## Stages (the chapter's three, risk-ordered; each independently shippable)

## S1 driver [ ]
- Replace `server/lib/run.ml`'s `Tuna_interp.Flat_drive.Make (Lwt)`
  with a DIRECT-STYLE eio driver over `Tuna_interp.Flat`: a loop of
  `step` / `answer` where a prim suspension parks the machine as a
  VALUE; the host resolves grant checks + journals the call through the
  (now eio) store, then resumes via `answer`.
- Awaiting a prim answer becomes a plain fiber operation inside a
  cancellation domain; `TUNA_RUN_MAX_SECONDS` stops being a watchdog
  BESIDE the loop and becomes the loop's OWN cancellation discipline.
- **SEMANTIC PINS (non-negotiable):** a cancelled run finalizes as
  status `deadline_exceeded` journaled like any exhaustion — NEVER an
  exception at the boundary (AGENTS rule 5). `TUNA_COMPILE_MAX_SECONDS`
  keeps its 400 compile-failed behavior, never a pinner. Replay and the
  fork counterfactual carry the same cap. `deadline_exceeded` rows stay
  unverifiable (the clock is not a calculus fact).
- Referee: differential 24/24 + verify-3 (replay identity) + verify-5
  (divergence) + the prim suites, on YOUR instance.
- Note: if a driver-level eio conversion forces the store to be eio
  first, do S2's pgx_io/db.ml change in the same commit — that
  dependency is expected; keep the commit build-green.

## S2 store [ ]
- `pgx_lwt` -> pure `pgx` over `Eio.Net`, porting PP's `Io_eio` pattern
  (ambient switch/net/clock ctx, direct-style SQL flows, a pool under
  eio instead of the Lwt mvar in `db.ml`). Read PP `io_eio.ml` for the
  connection/pool shape.
- Socket story UNCHANGED: /tmp dir, :5434, trust auth, scripts/dev.sh
  orchestration identical. `store.ml` re-threads Lwt types to direct
  style — mechanical lets where `>>=` chains stood. **Every SQL string
  and row shape stays byte-identical.**
- Gate: the PG suites (`scripts/test-store.sh` / PG-gated tests under
  YOUR :5437 env) + `store/lib/dune` drops lwt/pgx_lwt.

## S3 http [ ]
- `Dream` -> `httpun_eio`. The Dream surface is bounded and
  enumerated: `server/lib/pages/*` (Dream.form decoding, Dream.html
  fragments, htmx partials), vendored static (`htmx.min.js`),
  login/logout session cookies, bearer auth on `/api`.
- Port utilities from PP's `http_kit` / `http_core` family. NOTE:
  `routes.ml` response-records are ALREADY Dream-free by design
  (routes.borg: handlers return plain records; `Api.dispatch_route`
  adapts them) — that seam was built for this day; only the adaptation
  layer enters this stage. Preserve it.
- The htmx UI must render BYTE-EQUIVALENTLY: smoke-ui 17/17 is the
  referee, not a redesigned corpus. `server/lib/dune` + `server/bin/dune`
  drop dream/lwt; `main.ml` builds an eio mainloop.
- Cookie/session + bearer behavior must be identical (accounts chapter).

## S4 Battery + flip + push [ ]
- Full chapter acceptance on YOUR instance: pure suites + PG suites +
  differential 24/24 + smoke-api 14/14 + smoke-ui 17/17 + verify-1..10
  10/10, and EVERY STEP COUNT unchanged across the corpus (the corpus
  is the purity referee; step counts are engine facts, not rim facts).
- CLOCK PINS reproduce: deadline_exceeded rows + compile-failed 400s
  exactly; omega/crown workloads still finalize under the wall clock
  with journaled status values.
- ONE FLIP, CLUB-SAFE: no staged dual rim, no flag; the whole series
  landed on the dev stack (this worktree) first — the club promote is
  the operator's step. Rollback is git revert.
- `borge lint && borge report` clean IN THE WORKTREE; flip
  `borg/rim-eio.borg` statuses to implemented with agent notes + the
  measured facts (F8 RIM REGRESSION NOT TRIGGERED, or the F8 entry in
  your chapter + this file if it triggers — record, never hide).
- Commit throughout; push `rim-eio` when the whole battery is green.

## F8 regime (pre-registered)
If the eio rim cannot reproduce the run-boundary clock policies, OR the
battery goes red in a non-straight-port way, OR stage cost balloons
without a named cause: the swap STALLS. The Lwt rim remains a LEGAL
STAGE per the purity thesis (it never made Lwt mandatory, only honest
as a labeled stage). Record in FINDINGS.md-equivalent (your chapter
stanza + this file; the host files it at merge), never a bridge
rescue, never a flag. A stalled stage just keeps the label honest.

## Deviations
- (record reality divergences here as you go)
