# tuna

Standalone **tree-calculus evaluator habitat**: a pure tree-calculus
core, an auditable effect boundary, and a journal that makes every run
faithfully replayable. Trees are exactly

```
type t = Leaf | Stem of t | Fork of t * t
```

Everything else — programs, effects, grants, REPL sessions — is built
on top of that one datatype, with a hash-chained journal as the system
of record. See `SPEC.md` and the `tuna.borg` book (the book is the
source of truth; code converges to it).

## Quickstart

```
eval $(opam env --switch=poohstack --set-switch)
dune build @all && dune runtest          # build + unit/differential tests
scripts/dev.sh start                     # PG (:5434, db tuna) + server (:18090)
curl -s http://127.0.0.1:18090/health    # {"status":"ok","db":true}
```

The dev server boots an admin identity (`root`) from
`TUNA_BOOTSTRAP_TOKEN`, or generates one and prints it once (persisted
at `/tmp/tuna-dev/bootstrap.token`). Every `/api/*` route and every
UI page (except login) requires that bearer token:

```
TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' /tmp/tuna-dev/bootstrap.token)
curl -s -H "Authorization: Bearer $TOKEN" http://127.0.0.1:18090/api/runs | head
```

Point a browser at `http://127.0.0.1:18090/login` (same token) for the
htmx UI: dashboard, program pages with provenance + patching, run
journal views with replay-verify buttons, grants admin, REPL.

## The calculus

- **Trees**: `Leaf | Stem t | Fork (t, t)`. Canonical serialization is
  the ternary string format (`0` = leaf, `1`+child = stem,
  `2`+left+right = fork); hash = sha256 of the ternary, lowercase hex.
  - **Reduction**: the olydis 2024 triage rules, verbatim port of the
    vendored reference (`reference/tree-calculus/`). A "step" is one
    triage-rule firing; wrapper applications are free. Runs are bounded
    by fuel (max firings) and a live-size cap — exhaustion is a normal
    result, never an exception.
  - **Strings**: the binary convention of the tree-calculus reference
    (`marshal.ml`): a string is a list of chars, a char is a
    little-endian bit list over bools (false = `Leaf`, true = `Stem
    Leaf`), so a byte costs O(bits) nodes instead of a unary chain.
    `"..."` literals and every prim string payload (paths, `now`,
    `uuid`, http bodies, error messages) use it; `prim_contract "2"`
    marks the codec change.
    - **Compile IS reduction**: the surface s-expr language
      (`(lambda (x) ...)`, application, `%<ternary>` tree literals, `"..."`
      string literals, and the `pair`/`cons` aliases) goes through bracket
      abstraction with eta and is fully evaluated at compile time. Every
      compiled-tree node carries the id of the IR node responsible for it —
      the provenance map that diagnostics resolve through. `pair`/`cons`
      compile to the leaf (extensionally the fork constructor), so
      `(pair a b)` = `Fork (a, b)` at zero triage cost.
  - **Demand-memo** (`borg/sharing.borg` §demand-memo): opt-in
    cross-run sharing of CLEAN firings ("demand": true under v1).
    Own-garden trust model (the `demand_memo` table is keyed by caller;
    a run only consumes answers its own identity produced), hits are
    free (no step, no fuel; `demand_hits` on the run row), and the
    dirty rule hardens to a boundary law — a firing whose evaluation
    touched a prim is never cached, so grant liveness and the journal
    audit are untouched by construction. Replay re-executes without the
    garden: a demand run's step count is an environment fact, but its
    result must reproduce bit-for-bit.
  - **Pure-step core** (`interpreter/lib/flat.ml`, `borg/purity.borg`):
    the RUNTIME PURITY THESIS end state — an explicit-stack (CEK-style)
    abstract machine whose state is a value, with no monad parameter and
    no scheduler/Lwt types. `step` performs one elementary reduction;
    a prim call parks the machine in a `pending` value the host answers
    out of band (`answer`), so suspension is a value. Its step counts are
    **bit-for-bit identical** to the recursive monad engine across the
    whole differential corpus under both semantics (v0 and v1), refereed
    by `tests/flat_tests.ml`; `tools/flatbench` measures the two engines
    at no step-cost premium. The server run boundary *drives the pure
    core* through `Flat_drive.Make(Lwt)` (the monad lives at the rim,
    never in the core); the recursive engine is kept as the test referee.

      Effects stay run-time-only with the `(runtime (prim "name" ...))`
      form: it threads the enclosing lambda parameter through constant
      arguments (a K combinator) so compile-IS leaves the call alone —
      without it, a prim call whose arguments are all constants would
      fire during compilation (prims only run inside a run).

      The prim family includes the journaled math prims
      (`math/add`, `math/sub`, `math/mul`, `math/cmp`, `math/neg`;
      borg/math-prims.borg — the F6 consequence): host-mirrored law-5
      sign-magnitude int ops answered at O(bits) per call under the
      same grant/journal/replay law as every other prim. The pure
      sabra defs (sabralib dictionary) remain the reference semantics
      and the replay-recomputable check layer; tests gate the prims
      against them (tests/math_prim_tests.ml).

## Surfaces

### CLI (`cli/bin/main.exe`)

```
tuna compile <source-file>       # hash, ternary, size, steps; compile errors carry IR paths
tuna eval <corpus-file>          # run a diff-corpus entry (program+args+fuel+cap)
tuna eval-compiled <ternary> [args...] [--fuel N] [--cap N]
```

### JSON API (bearer auth on everything; 401/400/403/404/409 by case)

```
POST   /api/programs               {"ternary": "..."} | {"source": "..."}  → 201 {hash}
GET    /api/programs/:hash         → row (+ ir provenance when compiled from source)
POST   /api/programs/:hash/patch   {"path","expected_old_hash","new_ternary"} — structural CAS; 409 carries both subtree hashes + first-diff path
POST   /api/runs                   {"program_hash","inputs":[ternary...],"grants":[id...],"fuel","size_cap","semantics","trace"/"trace_cap","demand"} → run row (synchronous, journaled)
GET    /api/runs?caller=&program=  → list
GET    /api/runs/:id               → row + journal (auto-verifies an unverified finished run)
GET    /api/runs/verify?all=1      → replay-verification sweep over all runs
POST   /api/runs/:id/gc            → retention GC: cited tombstone, verifier reports GONE never VERIFIED
GET    /api/journals/:run_id       → full journal
POST   /api/journals/:run_id/fork  {"edits":[{seq, result_ternary|error, ...}]} → counterfactual run (chain rebuilt, edited suffix re-executed)
POST   /api/grants                 {"prim","args_attenuation"?} → grant id
POST   /api/grants/:id/attenuate   {"prim"?,"args_attenuation"?,"path_prefix"?} → narrower derived grant (holder-only; lineage recorded; widening 400, dead lineage 409)
GET    /api/grants/:id             → row + lineage (ancestors) + descendants
POST   /api/grants/:id/revoke      → revoke (live at every prim call, and through the parent_grant chain: revoking a root kills its subtree's future use)
POST   /api/repl                   {"command": "..."} or {"term": ...} (+ inputs/grants/fuel/size_cap for eval rounds)
GET    /health                     → no auth, db ping
```

REPL command grammar (`eval`/`def`/`undef`/`get`/`patch`/`first-diff`/
`dict`; a bare term evaluates): see `server/lib/repl_cmd.ml`. Eval and
def rounds are first-class journaled runs, chained per identity via
`parent_run_id`; structural commands are store queries. Name resolution
at parse time (borg/stdlib.borg): lambda param > identity dictionary >
sabralib dictionary (the seeded sabra stdlib v1, 61 defs: v1
vocabulary + the dialect chapter's records; borg/stdlib.borg +
borg/dialect.borg) > reader
builtins — identity def shadow wins, `undef` reveals std.

The surface reader (borg/dialect.borg v0.1) adds construction sugar
that folds into the same grammar: `[a b c]` brackets build the cons
chain (pair a (pair b (pair c 0))), decimal/negative atoms are
canonical law-5 int literals, and `(let ((x a)) B)` is nested lambda
application.  A sugar form and its hand-written twin compile to the
identical tree; `letrec` stays gated (unused by the port).  Records are
the v1.1 vocabulary: a record is a list of [key value] two-lists
(`rec-get`, `rec-val`, `rec-has`, `rec-upd`).  The chapter's dogfood is
the todo board in-calculus — read fold, state transition, filter query
— in `scripts/dialect/` (acceptance `scripts/verify-12-dialect.sh`).

### Federation (M12, `borg/federation.borg`)

Two tuna instances exchange values by hash and converge by shipping
verified ops windows. `TUNA_FED_PEERS` boots one non-admin identity per
peer (`fed-peer-<name>`); a supplied `TUNA_FED_PEER_TOKEN_<NAME>` is the
same bearer on every instance.

```
GET  /api/fed/value/:hash           → {hash, kind, payload}: tree = canonical
                                      ternary (a program hash resolves as its
                                      own ternary), bytes = base64. The peer
                                      rehashes on receipt; reads are
                                      hash-gated (any bearer), journaled
                                      op fed-value.
GET  /api/fed/ops?prefix=&after_seq=&limit=
                                    → contiguous op window {ops[], head,
                                      last_seq, verified}; each row carries
                                      its true prev_hash so a prefix-filtered
                                      (non-contiguous) window verifies per row.
POST /api/fed/ops/apply             {"src_prefix","dst_prefix","ops",
                                      "values":[{hash,kind,payload}]}
                                      re-verifies the chain, rehashes inline
                                      values, checks per-peer authz (peer
                                      <name> writes only ns/<name>/...;
                                      admin any), folds atomically into the
                                      destination. Append-only, no merge:
                                      a re-apply shadows under a fresh
                                      version, divergence is two namespaces.
```

`scripts/fed-sync.sh <peer> <local> <token> <src> <dst> [after-seq]`
drives a full value-pull + ops-pull + apply round with client-side
rehash verification (`scripts/fed-pull.sh` remains the single-value
client).

### Effects (prims) and the boundary

Programs call out at a prim boundary: `(prim "name" args...)` compiles
to an inert gate-shaped tree that only fires inside a run boundary.
The v1 registry: `echo`, `now`, `uuid`, `store/get`, `store/put`,
`http/get` (egress allowlisted via env). Every call is checked LIVE
against a grant row (mid-run revocation bites) and journaled — denial
is a journaled error answer, not an exception. **Faithful replay** =
re-executing program+inputs with prim answers consumed sequentially
from the run's journal; verification checks the chain, per-seq
answers, result hash and step count. No replay ever touches the live
host or network.

## Repository layout

```
common/       tree type, ternary serialize/parse, hashing, string codec
interpreter/  pure core: apply + fuel/size-bounded stepper; monadic twin
compiler/     surface s-expr → IR (spans) → bracket abstraction w/ eta → tree + provenance
store/        pgx_lwt access layer over the migrations schema
server/       Dream app: JSON API + htmx UI + REPL; bin/main.exe entry
cli/          terminal clients (compile / eval / eval-compiled)
migrations/   additive SQL (each file self-guards against re-application)
scripts/      dev.sh orchestration + smoke/verify/acceptance scripts
reference/    vendored upstream tree-calculus (normative) + CL twin
tests/        alcotest suites (unit / compiler / prim / store / differential)
```

## Public face (pp-slice)

Tuna serves a pricklypear-shaped public rim.  The anonymous surfaces
(reserved in `borg/routes.borg` law 2; the routes chapter's list grew
by that book change):

| path | what |
|---|---|
| `/welcome` | front page: what tuna is, the calculus in two lines, links |
| `/code` | read-only browser for recent programs + runs |
| `/agent.txt` | prose agent manifest (auth model, first calls, rules) |
| `/.well-known/agent.json` | machine contract (endpoints, auth, discovery) |
| `/src.tgz` | tarball of the server's own source at HEAD |
| `/health` | tokenless liveness (`{"status":"ok","db":true}`) |

`/agent.txt` and `/.well-known/agent.json` are generated in OCaml
(`server/lib/agent.ml`) and derive their base URL from the request's
forwarded host headers, so they are correct behind a reverse proxy.
`/src.tgz` is served from `TUNA_PUBLIC_DIR` (default
`/tmp/tuna-pp-public`) and produced by
`scripts/deploy/build-public.sh`, which runs `git archive HEAD` — no
github dependency at serve time.

## Accounts: two tiers

Tuna has one identity store (`identities`), used by two credential
tiers:

- **Bearer tokens** (the agent tier) — unchanged.  Every `/api/*`
  route accepts `Authorization: Bearer <token>`.  Boot prints root's
  once; an admin mints more.
- **Browser sessions** (the human tier) — `POST /login` takes
  username+password (or a bearer token) and mints an opaque session row
  (`auth_sessions`, migration 0013); the `tuna_session` cookie is
  HttpOnly + SameSite=Lax and carries only the session token (14-day
  TTL, revoked on logout).

Admin-only: minting identities (`POST /api/identities` or the
`/identities` page).  A minted identity is **non-admin** by default and
may run/compile, keep its own REPL dictionary, mint/attenuate/revoke
its own grants, and read public pages.  It cannot mint identities,
revoke others' grants, or touch other identities' namespaces (path-
prefix grants garden the tree names).  `TUNA_BOOTSTRAP_PASSWORD`
(optional) ensures root's password credential each boot; unset means
root signs in with its bearer token at the login page's token form.

## Deploy (pp-slice)

One operator command on the serving host:

```
scripts/serve.sh start     # PG + migrations + build + src.tgz + serve
scripts/serve.sh status    # :TUNA_HTTP_PORT health
scripts/serve.sh stop
```

`scripts/serve.sh` honors every `TUNA_*` knob and is distinct from the
dev loop (`scripts/dev.sh`).  A systemd **user** unit exemplar lives at
`scripts/deploy/tuna.service` (copy to `~/.config/systemd/user/`, edit
paths, `systemctl --user enable --now tuna.service`).  The TLS/edge
fragment and the flip procedure are in
`scripts/deploy/reverse-proxy.md`: terminate TLS at Caddy/nginx and
forward to `127.0.0.1:$TUNA_HTTP_PORT`; tuna never sees a private key.
Per-instance secrets live in an env file (mode 600), never in the tree.

## Honest limitations (pp-slice)

Recorded, not hidden:

- **Interim password hashing.** Credentials use the PP-compatible
  interim format `sha256$salt$digest` (`digest = sha256(salt ^ ":" ^
  password)`), not a memory-hard KDF.  Argon2id is named as planned;
  the format is string-compatible so a future upgrade verifies legacy
  rows and transparently rehashes on success — no flag day.  There is
  no server-side pepper yet.
- **No CSRF token.**  The session cookie is SameSite=Lax only; browser
  clients may drive `/api/*` with the session cookie, so same-site
  cross-origin requests are the residual risk.  Bearer-only agents are
  unaffected.
- **No OAuth/JWT**, no password reset, no login throttling.  `auth_log`
  rows exist so a throttle/forensics pass has data instead of guesses.
- **TLS is at the reverse proxy**; tuna speaks plain HTTP on loopback.
- **`/src.tgz` reflects HEAD at the last `build-public.sh` run**, not
  necessarily the running binary; re-run `serve.sh start` after a pull.

## Tests & acceptance

```
dune runtest                     # 60+ alcotest suites + 24-entry differential corpus (3 engines agree)
scripts/smoke-api.sh             # 14 sections over a live server
scripts/smoke-ui.sh              # 17 sections over the htmx UI
scripts/smoke-public.sh          # 12 sections over the public rim + sessions
scripts/verify-1..11-*.sh        # acceptance criteria 1–11 (callable in any order, exit 0 on green)
borge lint && borge report       # book/reality consistency
```

Failure definitions F1–F4 (`borg/acceptance.borg`) are pre-registered
findings, not bugs — observed evidence lives in `FINDINGS.md`.
