# Tuna on town: what runs where

This worktree (/home/pp/tuna) is FROZEN at the b098a18 (tuna2-substrate)
checkout and is NOT updated by deploys. Deploys swap artifacts only:

  _build/default/server/bin/main.exe   live binary, swapped by
                                       /usr/local/sbin/tuna-promote
                                       (previous kept as main.exe.prev)
  migrations/*.sql, server/static,
  scripts/*.sh                         synced each deploy

/etc/rc.d/tuna -> run-server.ksh sources /etc/tuna/tuna.env and execs
the _build binary, so git state in this tree says nothing about what
is live. Current deployed rev:

  - vultr:~/tuna-deploy/deploy.log      append-only relay+promote log
  - /home/pp/tuna-stage/tuna-<rev12>/   staged source of the current
                                        deploy (kept for debug, cleaned
                                        on next promote)

Deploy flow (monorepo): push `refs/heads/main` to `wyo.town:/srv/git/wyo.git`
-> path-filtered `post-receive` (`tuna/**` or `midden/**` changed) -> relay to
`gregor:/home/al/tuna-deploy/hook-entry.sh <rev>` (archives `midden/` + `tuna/`
from `wyo.git`) -> `town:/usr/local/sbin/tuna-promote` (stages `midden` into
`tuna/_build/midden-prefix`, builds `tuna` + runs `TUNA_DB_PORT=5432 dune
runtest`, applies `tuna/migrations/*.sql`, swaps `main.exe`, `rcctl restart
tuna`, health `GET /` 200|303 with auto-rollback to `.prev`).

## Server rev observability (10-27)

tuna-promote writes `TUNA_BUILD_REV=<rev12>` into /etc/tuna/tuna.env
before the restart, so /health (open) answers `rev:` and every
claims.sh mint records `server_rev=` in its projection - a citation
names the server rev it observed. An unset value answers "unknown":
the binary predates the convention. The manual hook-entry path must
set the line too, or the next /health says unknown.

## Deploy receipts (10-27)

tuna-promote mints an observation claim after a green health check
(`claims.sh deploy <rev12> "battery summary"`, TUNA_BOOTSTRAP_TOKEN,
non-fatal on failure): it lands at
u.<claimant>.claim.<date>.observed.deploy.<rev12>.audit-<p12>, so the
registry lists every deploy with its battery receipt and a peer can
check the floor against the rev it names.
