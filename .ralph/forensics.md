# Tuna — forensics chapter (loop workspace)

You are implementing tuna's divergence-forensics chapter: the
"extremely debuggable" research pitch made falsifiable.end to a
deliberate-fault corpus and frozen numeric gates.

Read `AGENTS.md` + `SPEC.md` + the book first (`tuna.borg` + `borg/*.borg`;
the book is the source of truth and code converges to it). The chapter
implement is `borg/forensics.borg` — read it FIRST and follow it
verbatim. `.ralph/plan.md` is the archive of house conventions
and finished milestones; treat it as reference, your tasks are here.

## Ground rules

- `eval $(opam env --switch=poohstack --set-switch)` before anything.
- Gates: `dune build @all && dune runtest` green, `borge lint` clean,
  the battery green (smoke-api, smoke-ui, verify-1..9), commit small,
  `git push origin master` after each green-gate commit.
- Chapter acceptance: **CORE UNTOUCHED** — interpreter/, compiler/,
  common/ stay byte-identical; delta confined to scripts/ (server/
  cli/ tests/ only if a finding legitimately forces protocol surface,
  and then say why under Deviations).
- **VERIFY NUMBERING COLLISION**: the chapter text names
  `scripts/verify-9-forensics.sh`, but `scripts/verify-9-math-prims.sh`
  shipped BEFORE you (math-prims landed). Use
  **scripts/verify-10-forensics.sh** and fix the chapter's numbers in
  the same commit that flips the book.
- **FAILURE-NUMBER COLLISION**: the chapter pre-registers "F7
  SUPERLATIVE OVERREACH", but F7/F8 are already taken in FINDINGS.md
  (PRIM DRIFT / PRIM CREEP, from math-prims). Renumber the chapter's
  failure to **F9 SUPERLATIVE OVERREACH** in the same book-fix commit;
  FINDINGS.md gains an F9 placeholder alongside its siblings.

## CONCURRENT LOOP — isolation rules (load-bearing)

A sibling ralph loop is running on branch `pp-slice` in the worktree
`../tuna-pp` (sessions/credentials, public face, serving kit). Rules:

- Files the SIBLING owns — you must NOT edit: `migrations/*`,
  `server/lib/*` (esp. pages/routes/api), `scripts/smoke-api.sh` /
  `scripts/smoke-ui.sh`, its new scripts under any name it invents from
  `scripts/serve.sh`/`scripts/deploy/`/`scripts/pp-env.sh`. Yours:
  `borg/forensics.borg`, `scripts/forensics-corpus/**`,
  `scripts/verify-10-forensics.sh`, `FINDINGS.md`, README's
  "Tests & acceptance" section only (the sibling won't touch it).
- Do NOT edit `tuna.borg` (your book work lives in the chapter file);
  find all verdicts in FINDINGS.md (the sibling has been ordered never
  to touch it this session).
- Do NOT take migration numbers; the forensics chapter needs none (the
  protocol is a loop over existing objects).
- Never `scripts/dev.sh stop`, `stop-pg`, or `clean`. The PG cluster
  (socket /tmp :5434, db tuna) and the operator's dev server
  (http://127.0.0.1:18091, token at `/tmp/tuna-dev/bootstrap.token`) must
  stay up. If the live 18091 server predates any endpoint you need,
  boot your own fresh copy on **TUNA_HTTP_PORT=18093** against the same
  DB, `export TUNA_HTTP_PORT=18093` for the verify scripts, and kill a
  process only if you started it.
- The verify scripts + verify-lib.sh honor `TUNA_HTTP_PORT`,
  `TUNA_SMOKE_TOKEN`, `TUNA_DB_*`; never hardcode colliding ports.
- Your corpus adds runs/programs/values rows into the shared dev db —
  fine, it is the evidence db; your scripts must stay idempotent /
  re-runnable ("corpus invariant under rerun" is part of your gate 5).

## Tasks (one or two per loop; tick + annotate as you go)

## T1 Protocol inventory [x]
- SEAL: deriv records (server/lib/deriv.ml — sealed record + offline
  deriv-check with exit semantics 0/1/2).
- DIFF: first-diff — the PATCH API's 409 payload carries the
  first-divergent path + both subtree hashes; REPL has a first-diff
  too. Confirm + exercise both.
- CAUSE: journal fork (`POST /api/journals/:run_id/fork` edits re-run
  suffix). Trace summaries (run with `trace`/`trace_cap` — the per-
  firing digest view). The divergent run's Loop closure pair
  (a run that closes as `loop` names the offending (fun, arg) pair —
  verify this exists in the run row/journal as the chapter assumes,
  record under Deviations if reality differs).
- LOCATE: callsite path -> provenance -> IR node + span
  (`GET /api/programs/:hash` ir provenance).
- FIX: CAS patch at the divergent path (expected old-hash = the diff's
  failing subtree) -> fresh program hash; PROVE: replay-verify the fix
  against the frozen journal, then a fresh live run where called for;
  HANDOFF: the transcript is queries over hash-addressed artifacts.
- Write the protocol as a documented curl sequence (corpus README is
  fine) and sanity-run every step by hand against the live server with
  a known stdlib program before building the corpus gate.

## T2 Fault corpus [x]
- `scripts/forensics-corpus/`: N>=12 deliberate faults over
  stdlib-v1-bound programs (vocabulary: `stdlib/v1/core.defs` +
  `scripts/diff-corpus`; the sequencing gate is satisfied — stdlib v1
  shipped). Four classes per the chapter: structural (flipped digit /
  swapped arm), semantic (wrong dict def consumed by unchanged caller),
  world-via-journal (wrong prim answer, trees identical, only the fork
  localizes), divergence (omega-shaped fault; expected answer = Loop
  closure pair, never a fuel burn).
- Faults produced by SCRIPTED MUTATION (never hand-edited): a
  deterministic mutation script (python3) + per-fault expected-answer
  annotations (exact mutation path / def boundary / fork verdict /
  loop pair).
- Every injection lands as a journaled CAS patch
  (`POST /api/programs/:hash/patch`) — the corpus is auditable history.

## T3 scripts/verify-10-forensics.sh [x]
- Full loop SEAL->HANDOFF unattended against the live server; exit
  semantics mirror deriv-check: 0 green, 1 any-miss, 2 malformed.
- Prints the MEASURED number beside each gate:
  1. LOCALIZATION: >=90% class-1 exact mutation path by first-diff
     alone; >=80% class-2 at the mutated def's boundary.
  2. QUERY BUDGET: class-1/2 localized within <=8 API queries;
     class-3 within <=12 including the fork.
  3. COLLABORATION: two transcript runs from identical starting
     hashes agree on every localization answer exactly (the re-run is
     part of the script).
  4. COUNTERFACTUAL: every class-3 fork reaches a different terminal
     status; replay-verify passes both records.
  5. UNATTENDED: the script itself (exit semantics above), zero human
     state.
- A gate miss is NEVER a quietly relabeled test — it is F9, recorded.

## T4 Measurements + F9 regime [x]
- Record measured numbers (corpus README + chapter agent note).
- F9 SUPERLATIVE OVERREACH: any frozen gate miss => the pitch drops
  "EXTREMELY", retains "a debugging discipline over hash-addressed
  artifacts — receipts all the way down"; FINDINGS.md entry with
  evidence; artifacts stay load-bearing. Same regime F5/F6. MEASURE,
  do not massage — a localization percentage that comes in at 75% is a
  finding, not a rounding call.
- Scope discipline: gates passing proves stdlib-v1-scale programs and
  agent debuggers ONLY; write that scope into the flip note. The
  count-preservation counterexample (constant swap preserves count,
  changes behavior) is pinned in the corpus per the chapter.

## T5 Book flip + battery [ ] (borge lint clean; README battery + FINDINGS F9 + corpus README landed; commit/push pending)
- Flip `borg/forensics.borg` planned -> implemented with agent notes +
  measured numbers (same commit as the F9 + verify-10 renumbering).
- Add verify-10-forensics.sh to README's Tests & acceptance battery.
- `borge lint && borge report` clean; full battery green; commit;
  push origin master.

## Deviations
- verify number: chapter said verify-9-forensics.sh; shipped as
  verify-10-forensics.sh (verify-9 is math-prims) — book updated.
- failure number: chapter pre-registered F7; F7/F8 taken by math-prims,
  so it shipped as F9 SUPERLATIVE OVERREACH — FINDINGS.md + book updated.
- class-1 swap-arms: first-diff descends into the swapped arm
  (expect = path+"1") because the fork keeps its shape; this is the
  localization answer and is recorded in the corpus table + book agent
  note. Not a miss.
- journal fork status: class-3 uses `/api/journals/:id/fork` with a
  `{seq,result_ternary}` edit; the fork row is fetched from the fork
  response (no separate GET needed). Confirmed live.
- Loop closure pair: surfaced on the trace endpoint as a `loop` event
  carrying `fun`/`arg`; the run row itself does not store the pair, so
  the corpus reads it from `/api/runs/:id/trace`. Recorded here —
  reality matched the chapter's assumption, via trace not run row.

## Progress (loop #5, 2026-10-04)
- Corpus driver + tree primitives written (scripts/forensics-corpus/),
  15 faults, all four classes, counterexample pinned.
- scripts/verify-10-forensics.sh green against the live 18091 server:
  gate 1 100%/100%, gate 2 max 2/1 queries, gate 3 agree exactly,
  gate 4 2/2 + 2/2, gate 5 unattended. MEASURED, not massaged.
- borg/forensics.borg flipped planned -> implemented (book diff already
  in working tree) with measured agent note + scope.
- FINDINGS.md F9 placeholder added alongside F7/F8.
- scripts/forensics-corpus/README.md written: protocol curl sequence
  (SEAL->HANDOFF) + full fault table + measured gate verdicts.
- README "Tests & acceptance" battery now names verify-1..10.
- OPEN: run `dune build @all && dune runtest`, re-run borge lint/report,
  commit small, push origin master. (Book flip + F9 + verify-10
  renumbering all land in the same commit as required.)
