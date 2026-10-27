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

Deploy flow: push refs/heads/master to wyo tuna.git -> post-receive
relay -> vultr -> tuna-promote here (build + battery + migrations +
swap + rcctl restart + health GET / 303, auto-rollback to .prev).

## Server rev observability (10-27)

tuna-promote writes `TUNA_BUILD_REV=<rev12>` into /etc/tuna/tuna.env
before the restart, so /health (open) answers `rev:` and every
claims.sh mint records `server_rev=` in its projection - a citation
names the server rev it observed. An unset value answers "unknown":
the binary predates the convention. The manual hook-entry path must
set the line too, or the next /health says unknown.
