# Tuna — pricklypear-like slice (loop workspace, branch pp-slice)

Goal: turn tuna from a dev-loopback evaluator into a
**pricklypear-like live habitat slice**: cookie-session login with
hashed credentials + non-admin identities, the PP-shaped public face
(welcome page, /code, /src.tgz, agent.txt, .well-known/agent.json), and
a serving/deploy kit so the eventual public flip is one operator
command. Read `AGENTS.md` + `SPEC.md` + the book first (tuna.borg +
borg/*.borg — the book is the source of truth and code converges to it).

## Where you are (load-bearing)

- You are in a **GIT WORKTREE** at `/home/roerick/dev/wyo.tech/tuna-pp`
  on branch `pp-slice` (forked from master d5c4a94). Commit ALL work to
  this branch, small commits, house style. NEVER checkout/merge/rebase
  master; NEVER push master. When the whole battery is green at the
  end: `git push -u origin pp-slice` (the host/operator merges).
- A sibling ralph loop is running on MASTER in the main tree on the
  forensics chapter. Files the sibling OWNS — do NOT edit:
  `borg/forensics.borg`, `scripts/verify-10-forensics.sh`,
  `scripts/forensics-corpus/`, `FINDINGS.md` (record anything
  finding-shaped in your own chapter stanza + this file; the host files
  findings at merge), README's "Tests & acceptance" section.
- Migration numbers **0013+ are YOURS** (the sibling takes none).
  Smokes: add `scripts/smoke-public.sh`; do NOT edit smoke-api.sh /
  smoke-ui.sh / verify-*.sh.
- `tuna.borg`: append EXACTLY ONE `(inline "borg/<your-chapter>.borg")`
  line; all your other book work (docstring, statuses, agent notes)
  lives inside your own chapter file. Do NOT un-flip implemented
  chapters; your slice gets a NEW chapter (read the house chapter style
  first: docstring framing a research question, statuses per stanza,
  acceptance subsection).

## Environment isolation (load-bearing; this machine runs other things)

- `eval $(opam env --switch=poohstack --set-switch)` before anything.
- TAKEN on this machine: PG 5433/5434/5435; HTTP 17878, 18080
  (pricklypear image), 18090 (glochid), 18091 (tuna master dev).
  **YOUR slots: PG :5436, HTTP :18092.**
- Your instance env (export in every shell; suggestion: write
  `scripts/pp-env.sh` to source):
  `TUNA_DB_HOST=/tmp TUNA_DB_PORT=5436 TUNA_DB_NAME=tuna
   TUNA_DB_USER=tuna TUNA_DATA_DIR=/tmp/tuna-pp-pg
   TUNA_DEV_DIR=/tmp/tuna-pp-dev TUNA_HTTP_PORT=18092`
- `scripts/dev.sh` honors all TUNA_* knobs (cluster init, migrations
  auto-apply). `scripts/test-store.sh` + the PG-gated alcotest suites
  honor `TUNA_DB_*` — full isolation for `dune runtest`.
- First `dune build @all` here is a COLD build; expect minutes.
- Kill only processes YOU spawned. Never touch 18091 / 5434 / 18080 /
  18090 — that is the master loop's and the operator's ground.

## PP patterns to mirror (read them, then implement tuna-native —
the tuna book's law wins wherever book and pattern disagree)

- Auth: `/home/roerick/dev/wyo.tech/pricklypear/migrations/002_auth.sql`
  (users/credentials/auth_sessions/auth_log shapes) + PP's image/lib
  auth middle layer + PP README's "Honest limitations" tone (interim
  `sha256$salt$digest` hashing — argon2 named as planned; sessions
  HttpOnly SameSite=Lax; CSRF not yet; TLS at the reverse proxy).
  Tuna ALREADY has identities (migrations 0001 + 0003: identities,
  is_admin; server/lib/tokens.ml maps bearer tokens; fed peers boot
  non-admin identities). CHECK the real shapes FIRST; additive SQL
  only; every migration self-guards against re-application (AGENTS
  law — 0001 is the pattern).
- Public face: PP `image/lib/agent_discovery.ml` + `image/lib/http_core.ml`
  carry welcome / agent.txt / .well-known/agent.json / src.tgz shapes.
  Tuna's routes chapter (`borg/routes.borg` — IMPLEMENTED, read it;
  server/lib/routes.ml + the tree_paths `route/*` namespace) is the
  LAW: endpoints become data where that chapter says they do. Follow
  it; do not bolt on a second routing scheme.

## Tasks (one or two per loop; tick + annotate as you go)

## T0 Recon [x] — landed design (loop 1; reality as found)

**Current auth reality (differs from PP; shapes recorded before
writing SQL):**
- tuna has ONE credential store today: `identities(id, name UNIQUE,
  token_hash = sha256(bearer), is_admin)` (migration 0001;
  0003 fixed grants FK). Identity minting is BOOT-ONLY — root via
  `TUNA_BOOTSTRAP_TOKEN` (`Api.serve`), fed peers from fed env (both
  `Store.bootstrap_identity`); there is NO API/UI minting surface.
- The login page (server/lib/pages/auth.ml) currently accepts a raw
  bearer token and the cookie `tuna_session` carries THAT TOKEN; pages
  auth = cookie→`Store.verify_token`. There are no sessions/passwords.
- `Api.authenticate` = bearer-only for /api/*; `Pages.Auth` = cookie
  for pages. Both resolve to `Store.identity`.
- Admin-ness (`is_admin`) only bypasses grant-coverage checks (mint/
  revoke others' grants, route publish, repl dict admin view). A
  non-admin identity otherwise ALREADY behaves as T2 wants (per-
  identity dest via repl_dict + runs.caller; path-prefix grants garden
  the tree names and page-level shields; `Run.execute` requires grants
  only at prim boundary). The missing piece is MINTING + a password
  tier, not a new permission matrix.
- routes.borg law 2 pins the reserved list (api health login logout
  frag grants programs runs repl value route static view todo) and
  says it "may only grow by book change" — our PP-slice chapter IS
  that book change (welcome code agent.txt src.tgz .well-known added
  to the pinned list in routes.borg + noted in our chapter).
- PP patterns as landed in PP's tree TODAY: 002_auth.sql (users /
  credentials(kind, public_data, secret_data jsonb) / auth_sessions
  (token_hash, expires, revoked) / auth_log) and auth_store.ml's
  opaque session-token + sha256-stored + 14d TTL + revoke-on-logout;
  PP has since UPGRADED to argon2id with transparent rehash of legacy
  `sha256$salt$digest` rows (digest = sha256(salt_hex ^ ":" ^
  password)). We adopt THE INTERIM format verbatim (same string shape
  and same digest recipe, argon2id named as planned in README/chapter
  honest-limitations) so a future transparent-rehash upgrade is
drop-in.

**Landed design decisions (T1–T3 concretely):**
- **T1 (0013):** flat additive tables `credentials(kind,
  secret_hash, UNIQUE(identity_id, kind))`, `auth_sessions(token_hash,
  expires_at, revoked_at)`, `auth_log` — flat relational rows, no
  jsonb blobs (0001's flat-table law; the storage watch note says
  nothing here prefers a path). Salt-hex strings from /dev/urandom;
  session token = 32 random bytes hex stored as sha256. Store layer:
  `set_password`, `verify_password` (constant-time; timing-flattened
  on unknown username against a fixed dummy hash), `mint_session`,
  `verify_session`, `revoke_session`, `log_auth`.
- **Cookie contract changes:** the `tuna_session` cookie now carries
  the OPAQUE SESSION TOKEN (verified via auth_sessions), never a
  bearer; typed-in bearer tokens at /login MINT a session so the
  cookie contract is single. Login form = username+password (+ a
  "bearer token" alternate form for identities without passwords).
  Logout revokes the session row. Old cookie sessions (bearer-in-
  cookie) stop verifying — re-login; acceptable for a dev-facing
  dx upgrade, recorded in the chapter.
- **API rim:** `Api.authenticate` accepts bearer FIRST, then session
  cookie (PP's "browser clients may use the session cookie" stance;
  CSRF honestly recorded as not-yet, SameSite=Lax since only).
- **Boot:** `TUNA_BOOTSTRAP_PASSWORD` (optional) ensures root's
  password credential each boot (rehash = rotation; PP_BOOTSTRAP
  analog, no insecure default — unset = root logs in by bearer form).
- Store helpers live in a new `store/lib/credentials.ml` (random hex,
  interim hash format, constant-time verify) — store layer self-
  contained, no server dep.
- **T2:** admin-only `POST /api/identities` minting non-admin
  identity + initial password (response carries the bearer token
  ONCE) + admin page under /grants-adjacent UI; bearer API auth
  unchanged. Boundary audit: non-admin gets mint_member/route
  publish only with covering grants (already enforced), cannot mint
  identities/revoke others' grants (already enforced via is_admin
  checks); also check repl/tree/value surfaces for identity-scoping.
- **T3:** /welcome + /code are server pages (dynamic row listings);
  /agent.txt + /.well-known/agent.json generated in OCaml
  (module `server/lib/agent.ml`) shaped after PP's agent_discovery;
  /src.tgz generated at BOOT time by the deploy script from the
  worktree (`git archive HEAD` → tar.gz) into TUNA_PUBLIC_DIR,
  served from disk — no github dep, document in README.

## T0 Recon [x]
- Read borg/routes.borg + server/lib/routes.ml (how route entries pin
  programs/byte-value templates); migrations 0001/0003/0004 + tokens.ml
  (identity/bearer/dict shapes); the login page flow; PP 002_auth.sql
  + PP image/lib auth + agent_discovery paths. Write the concrete
  landed design into THIS file (adjust T1–T4 as reality dictates),
  then implement.

## T1 Sessions + credentials (migration 0013) [x] — landed loop 1
- `credentials` + `auth_sessions` + `auth_log` landed (0013,
  self-guarding); interim `sha256$salt$digest` recipe pinned; argon2id
  named as drop-in. Store layer (set/verify password, mint/verify/
  revoke session, log_auth) in store/lib/store.ml +
  store/lib/credentials.ml.
- Login page: username+password OR bearer-token alternate; BOTH mint an
  opaque session; cookie `tuna_session` carries only the session token.
  Api.authenticate = bearer first, session-cookie fallback.
  TUNA_BOOTSTRAP_PASSWORD ensures root's password each boot.
- PG-gated suite tests/session_tests.ml (4/4 green under own scratch db
  `tuna_test_sessions`).  Full `dune runtest tests` green.
- NB: outer-match parenthesization bug in Api.authenticate fixed;
  test harness migration path must be absolute (rule passes `../migrations`
  relative to _build, resolved under dune runtest by workspace_root, but
  direct invocation needs an absolute path).

## T2 Identity at the rim [x] — landed loop 2
- Caller identity = session for browser requests, bearer otherwise.
- Admin mints non-admin identity + initial password (API + admin UI
  page; bootstrapped root is the only identity that can mint).
- Non-admin identities can: run/compile, REPL def (their own dict
  chains, chained via parent_run_id), mint/attenuate/revoke their own
  grants, read public pages. They cannot: mint identities, revoke
  others' grants, touch other identities' namespaces (path-prefix
  grants already enforce the garden; boundary audit found no hole -
  authority is the grant row, not the identity kind).
- Smoke coverage: smoke-public [8][9][11].

## T3 Public face [x] — landed loop 2 (commit c91acc3)
- /welcome, /code, /agent.txt, /.well-known/agent.json, /src.tgz all
  landed; agent manifest derives base URL from forwarded headers.
- /code is a server page (no capability, dynamic rows), not a route
  record; recorded in borg/accounts.borg.
- /src.tgz generated by scripts/deploy/build-public.sh (git archive
  HEAD) into TUNA_PUBLIC_DIR, served via Dream.from_filesystem.
- Smoke coverage: smoke-public [1]-[6][12].

## T4 Serving/deploy kit [x] — landed loop 2
- scripts/serve.sh (start/stop/restart/status; pidfile; migrations
  auto-apply; builds src.tgz; honors all TUNA_* knobs) — distinct from
  dev.sh, which stays untouched.
- scripts/deploy/tuna.service (systemd user unit exemplar),
  scripts/deploy/reverse-proxy.md (TLS-at-edge flip doc),
  scripts/deploy/build-public.sh.
- README: "Public face" + "Deploy" + "Honest limitations" sections
  added (PP tone); "Tests & acceptance" section untouched.

## T5 Book + battery + push [x] — landed loop 2
- borg/accounts.borg landed (7 stanzas implemented; F10 session
  confusion + F11 rim shadowing pre-registered). Exactly one inline
  line appended to tuna.borg.
- routes.borg law 2 list grew by book change with a dated note; the
  transcribed list in server/lib/routes.ml now matches (this was the
  loop-1 failed edit: identities/welcome/code/agent.txt/src.tgz/
  .well-known were never transcribed into code — fixed loop 2).
  tests/repl_tests.ml pins the reserved rim (book/reality pin).
- smoke-public.sh 302->303 fixed (Dream.redirect answers 303) and the
  loop-1 sed damage to the login line repaired; all 12 sections green.
- Battery green in the isolated instance (:18092): `dune build @all`,
  `dune runtest`, `borge lint && borge report`, smoke-public,
  smoke-api (14), smoke-ui (17).
- Pushed pp-slice (below, loop 2).

## Deviations
- (record reality divergence from this plan / the PP patterns here)
