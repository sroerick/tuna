# httpun hardening plan (persisted 10-06 after the 4th child wall-death)

Branch `httpun-hardening`, cut from master 0b5e18c (fix 3430f9b + error-path
regression tests; 3430f9b deployed to town 10-05 18:35Z). This file persists
the recon so no session re-derives it. Five detached children have died at
the 1h wall on this workstream (papercut 1U5MHFI; the 4th burned ~4.8M tokens
for zero code; the 5th was S3's child, whose slice was rescued 10-07 - see
the S3 entry). Successor protocol: land ONE slice per session, commit by
minute 40 no matter what.

## Recon (from child tuna-httpun-hardening-4, verified on clean master)
- server/lib/web.ml (516 lines) is the whole HTTP floor; per-connection
  switches exist from the 10-04 fix; no chunked/streaming code in the server
  tree (nothing that depends on chunked responses in-tree).
- Test harness boots real `serve` instances on loopback with raw sockets
  (tests/web_errorpath_tests.ml, 366 lines, already on master); dune 3.24.2
  (opam switch poohstack); clean-master `dune build` exit 0.
- httpun supports 413/431 status codes natively; Eio 1.5 Pi interface
  understood; all call sites mapped.
  - API facts pinned 10-06 (S2): Eio.Fiber.first : ?combine:('a -> 'a ->
    'a) -> (unit -> 'a) -> (unit -> 'a) -> 'a (two thunks; first finisher
    wins, its exception propagates); Eio.Time.with_timeout_exn : _ clock ->
    float -> (unit -> 'a) -> 'a raises Eio.Time.Timeout (the clean
      body-deadline race); clock = Eio.Stdenv.clock env. Still to pin for
      S4: Flow.copy_string. (accept_fork socket-close semantics pinned by
      S3: slot release = connection-fiber finish = both sides closed.)

## Slices (small loops; one per session)
  - S1 DONE 10-06 on this branch: declared Content-Length past max_body_bytes
    OR bytes actually read past it -> 413 + Connection: close (web.ml request
    handler; read_body's Body_too_large no longer swallowed into an empty
    body). Cap = the existing 1 MiB max_body_bytes (+TUNA_MAX_BODY_BYTES).
    Suite 7/7: new cases = declared CL past cap + chunked stream past cap
    (CL framing never delivers more than it declares; chunked is the read
    path).
  - S2 DONE 10-06 on this branch: whole-body read deadline, default 30s
    (web.ml default_body_timeout; per-deployment override = serve's
    optional ~body_timeout, the knob S5 wires into run.ml). Race =
    Eio.Time.with_timeout_exn around read_body in the request handler; on
    expiry the awaiting fiber is cancelled and the reader is dropped with
    the connection. Stall answers 408 + Connection: close - same
    containment shape as S1's 413 (tell the client why, stop waiting on
    the unread remainder). Suite 9/9: new cases = partial body + stall ->
    408 within deadline; slow-but-moving body inside deadline still 200.
    - S3 DONE 10-07 on this branch (slice rescued from the 5th wall-dead
      child, tuna-httpun-s3-1-7): live-connection cap. The accept loop hands
      out conn_max slots (Atomic compare-and-set; default 256 =
      default_conn_max, serve's optional ~conn_max overrides - the knob S5
      wires into run.ml). A connection accepted while saturated holds no
      slot; its requests are answered 503 + Connection: close on the first
      request, through the same contained safe_respond path as the 413/408
      answers. A served connection releases its slot when its connection
      fiber finishes - which is after the CLIENT closes its side too
      (httpun half-closes on Connection: close and lingers on the read
      side). Suite 11/11 (three consecutive green runs): new cases =
      saturated refusal + gate-release frees a slot -> served again.
      TEST GOTCHAS for S4/S5: wait_for_listener's probe takes a slot whose
      release is async, so holds must retry on an immediate 503 (hold_gate
      does; still refused after the window = real slot leak, fails loudly);
      and after a released connection's response, close the client side
      before probing capacity again.
  - S4 DONE 10-07 on this branch: connection LIFETIME cap (total age
    of one connection; not per-request, not the S3 concurrency count).
    serve races each connection's whole life against conn_lifetime
    (Eio.Time.with_timeout_exn around the connection handler inside
    the S3 per-connection Switch.run; default 300s =
    default_conn_lifetime, serve's optional ~conn_lifetime overrides -
    the knob S5 wires into run.ml). A connection still open past the
    cap (idle keep-alive or mid-request) has its switch cancelled and
    its socket closed by accept_fork; the Timeout is caught as a
    recycle (logged "connection lifetime expired", never on_error) and
    the S3 slot frees in the same Fun.protect finally. Suite 13/13
    (three consecutive green runs): new cases = an aged keep-alive
    connection is recycled (server closes its side unprompted; a fresh
    connection is served after) + a connection inside the cap keeps
    working (two keep-alive requests, one socket, no early close).
    TEST GOTCHAS for S5: keep-alive probes need their own request line
    (no Connection: close; keepalive_get in the suite) and a PLAIN
    Unix.sleepf inside drive (nested run_in_systhread raises
    Effect.Unhandled). Gate counts at this commit: tuna 38, prim 12,
    repl 4, store 17, tree_substrate 7 (repl/store above the old 3/11
    baseline; not touched by this slice). Env gotcha re-hit during
    gates: /tmp cleaner gutted the dev pg cluster ("checkpoint request
    failed") -> stop-pg, rm -rf /tmp/tuna-pgsup /tmp/tuna-dev,
    start-pg.
  - S5 DONE 10-07 on this branch: the three serve knobs are tunable per
    deployment without code edits.  web.ml reads TUNA_CONN_MAX (S3 live
    cap, default 256 = default_conn_max), TUNA_BODY_TIMEOUT (S2
    deadline, default 30s) and TUNA_CONN_LIFETIME (S4 cap, default
    300s) per serve call - same env pattern as TUNA_MAX_BODY_BYTES:
    unset, empty or malformed falls back to the compiled-in default,
    never a boot error; an explicit optional argument still wins.
    Production rides serve's defaults (bin/main.ml -> Api.serve ->
    Web.serve passes no knob args).  Repo note for successors: this
    tree has no server/bin/run.ml (that path is the stale sibling's
    layout) - the TUNA_MAX_BODY_BYTES precedent itself lives in
    web.ml, so the env readers went there too, per serve CALL rather
    than module init so tests can set one var per case.  Suite 15/15
    (two consecutive green runs; was 13): new cases =
    TUNA_CONN_LIFETIME=0.5 recycles a keep-alive connection early
    (case-12 shape via env) + TUNA_CONN_MAX=1 refuses the second
    connection (case-10 shape via env); with_env sets one var per case
    and clears it after (an empty value parses as absent).  TEST
    GOTCHAS: Unix.putenv per case works BECAUSE the env read is per
    call; Option.filter is unbound under this toolchain's dune build
    (5.4.1 toplevel has it - unexplained) -> nested match with
    when-guards, same style as the max_body_bytes reader.  Gate counts
    at this commit: web 15 x2 green, tuna 38, prim 12, repl 4,
    store 17, tree_substrate 7 (store/tsub via `bash
    scripts/test-store.sh <exe> migrations [db]`; PG was up, no
    cluster remedy needed).  httpun-hardening pass complete; merge +
    deploy = roerick per protocol.

## LANDED 10-07 (roerick: 'You can always push. Always push.')
The pass is COMPLETE: S1-S5 at branch tip 7b0072f, all gates green (web
15/15 x2 + full suites), pushed origin+wyo. Landing rule superseded:
green gated work fast-forwards master and pushes it - the wyo master
hook deploys to club. Branch kept for history; this file is the record.

## LANDED 10-09 (token rotation, self-serve)

98733b5 on master, pushed origin+wyo; wyo hook deployed to club green
(probe: POST /api/identities/<other>/rotate -> 403 first try).  The
slice: POST /api/identities/:id/rotate - any identity rotates its OWN
bearer token (hygiene, not a privilege); ANOTHER identity's requires
the admin gate, same as minting (board ff54443b, operator-approved
2026-10-09).  New raw token returned ONCE (only sha256 stored);
previous token stops verifying immediately; auth_log kind=bearer rows.
Supersedes hand-UPDATE as the only rotate path.  Live-verified on
club: non-admin rotating another id -> 403; own -> 200; old token 401
immediately after.  gregor's token (the one DM'd through the wedged
bridge) rotated in place the same hour.  Gates: dune build @all +
dune runtest 15 suites green (store suite incl. rotation test) on a
FRESH /tmp pg cluster - the /tmp cleaner had gutted the old one
('checkpoint request failed'; remedy stop-pg + rm -rf
/tmp/tuna-pgsup /tmp/tuna-dev + start-pg, per the 09-17 gotchas).
## LANDED 10-09 (grants list, admin)

5addf1b on master, pushed origin+wyo; wyo hook deployed to club green
(probes: GET /api/grants no token -> 401; gregor token -> 403 first
try, with ?author= -> 403 same).  The slice: GET /api/grants - admin-
only roster of the grants table (the JSON surface of the /grants page
list).  ?author=<uuid> scopes to one author identity (minted_by; the
caller is the fallback for pre-0003 rows that predate the author
column), ?limit= caps the page (default 100, max 1000, newest first).
Store.list_grants gained a required ~author; list call sites pass it
explicitly (the /grants page passes ~author:None).  OCaml gotcha for
the successor: 'f ?author ()' shorthand with an in-scope option
variable double-wraps the argument - pass a required labeled arg
instead.
## Successor protocol
1. Read this file + git log --oneline -3 on this branch. No re-recon.
2. Implement one slice, dune build, run the web tests.
3. COMMIT by minute 40 regardless of state; never leave the tree dirty.
  4. PUSH the branch AND land green gated work (roerick 10-07: 'You can
     always push. Always push.' - supersedes the 10-06 never-merge line):
     git push origin <branch> && git push wyo <branch>; when a slice is
     complete and gates green, ff master onto it and push master - the
     wyo master hook deploys to club. Judgment still applies: risky or
     half-done work stays on the branch.
