#!/usr/bin/env bash
# pp-slice T4: build the public artifacts served from TUNA_PUBLIC_DIR.
#
#   scripts/deploy/build-public.sh [output-dir]
#
# Produces src.tgz — a tarball of the server's own source at HEAD, so the
# live habitat can serve its own source with NO github dependency at
# serve time.  Uses `git archive HEAD` (tracked files only, no _build,
# no secrets); falls back to a plain tar of the tree if this is not a
# git checkout.
#
# Output dir defaults to $TUNA_PUBLIC_DIR, else /tmp/tuna-pp-public.
# Called by scripts/serve.sh on start; safe to re-run (atomic replace).
set -e

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT="${1:-${TUNA_PUBLIC_DIR:-/tmp/tuna-pp-public}}"

mkdir -p "$OUT"
tmp="$(mktemp "$OUT/.src.tgz.XXXXXX")"
trap 'rm -f "$tmp"' EXIT

if git -C "$ROOT" rev-parse --git-dir >/dev/null 2>&1; then
  git -C "$ROOT" archive --format=tar.gz -o "$tmp" HEAD
else
  echo "[build-public] not a git checkout; tarring the worktree" >&2
  ( cd "$ROOT" && tar --exclude=_build --exclude=.git --exclude=.ralph \
      --exclude='*.tar.gz' -czf "$tmp" . )
fi

mv "$tmp" "$OUT/src.tgz"
chmod 0644 "$OUT/src.tgz"
trap - EXIT
echo "[build-public] wrote $OUT/src.tgz ($(du -h "$OUT/src.tgz" | cut -f1))"
