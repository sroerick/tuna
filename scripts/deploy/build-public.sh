#!/usr/bin/env bash
# pp-slice T4: build the public artifacts served from TUNA_PUBLIC_DIR.
#
#   scripts/deploy/build-public.sh [output-dir]
#
# Produces src.tgz — a tarball of the server's own source at HEAD, so the
# live habitat can serve its own source with NO github dependency at
# serve time.  Uses `git archive HEAD` (tracked files only, no _build,
# no secrets); falls back to a tar of the worktree if this is not a
# git checkout.
#
# PORTABILITY (2026-10-27, observed on club/OpenBSD 7.9): the fallback
# used GNU tar spelling (`tar --exclude=... -czf`), and OpenBSD's
# /bin/tar rejects long options outright ("unknown option -- -").  That
# matters more than it looks: the DEPLOYED source on club is a staged
# export with no .git, so it ALWAYS takes the fallback -- meaning
# /src.tgz could never be built on the box that serves it, and the
# public surface 404'd while the book promised it.  Detect a tar that
# understands the GNU flags and use it when present (club ships
# /usr/local/bin/gtar); otherwise use the portable -c/-z/-f + -X
# exclude-file form that both BSD tar and pax accept.
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
  # prefer a GNU tar: its --exclude needs no temp file and handles globs
  GNU_TAR=""
  for t in gtar /usr/local/bin/gtar; do
    if command -v "$t" >/dev/null 2>&1 &&
       "$t" --version 2>/dev/null | grep -q "GNU tar"; then
      GNU_TAR="$t"; break
    fi
  done
  if [ -n "$GNU_TAR" ]; then
    ( cd "$ROOT" && "$GNU_TAR" --exclude=_build --exclude=.git \
        --exclude=.ralph --exclude='*.tar.gz' -czf "$tmp" . )
  else
    # portable path: an exclude FILE (-X) works on BSD tar and pax alike
    xf="$(mktemp "$OUT/.src.exclude.XXXXXX")"
    trap 'rm -f "$tmp" "$xf"' EXIT
    printf '%s\n' ./_build ./_build/* ./.git ./.git/* ./.ralph ./.ralph/* \
      ./*.tar.gz >"$xf"
    ( cd "$ROOT" && tar -c -z -f "$tmp" -X "$xf" . )
    rm -f "$xf"
  fi
fi

mv "$tmp" "$OUT/src.tgz"
chmod 0644 "$OUT/src.tgz"
trap - EXIT
echo "[build-public] wrote $OUT/src.tgz ($(du -h "$OUT/src.tgz" | cut -f1))"
