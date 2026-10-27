#!/usr/bin/env bash
# S5 acceptance (UI-SPEC.md 2029077): link discipline over the public face.
#
# Consumer-agent policy forbids iterating guessed URL variants - unlinked
# surfaces do not exist.  This crawl PROVES the guarantee rather than
# asserting it: starting from / and /login, anonymous (no cookies, no
# tokens), following only <a href> links:
#   1. every expected public surface is reachable (final code 200);
#   2. every public HTML page carries the same shared nav
#      (Pages.Layout.nav renders one nav for all pages);
#   3. no same-origin <a href> on any crawled page ends in a dead page.
#
# Env: TUNA_HTTP_PORT (default 18090).  Runs against a live server
# (scripts/dev.sh start).  Exit 0 on green.
set -u

BASE="http://127.0.0.1:${TUNA_HTTP_PORT:-18090}"
fail() { echo "FAIL: $*" >&2; exit 1; }

# public surfaces that must be reachable by links alone
EXPECTED="/ /login /welcome /agent /agent.txt /claims /prims /code /health /src.tgz /static/repl.html /.well-known/agent.json"

# the shared nav (Pages.Layout.nav): every public HTML page carries all of these
NAV_LINKS="/ /repl /grants /identities /todo /prims /claims /welcome /code"

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT
QUEUE="$WORK/queue"; SEEN="$WORK/seen"; LINKS="$WORK/links"; HTML="$WORK/html"; GOT="$WORK/fetched"
: > "$SEEN"; : > "$LINKS"; : > "$GOT"; mkdir -p "$HTML"
printf '%s\n' / /login > "$QUEUE"

seen() { grep -qxF "$1" "$SEEN"; }
fetch() { curl -s -L --max-time 10 -o "$WORK/body" -w '%{http_code}' "$BASE$1"; }

while IFS= read -r p; do
  seen "$p" && continue
  echo "$p" >> "$SEEN"
  code=$(fetch "$p") || continue
  echo "$p $code" >> "$GOT"
  [ "$code" = 200 ] || continue
  ct=$(curl -s -o /dev/null -w '%{content_type}' "$BASE$p")
  case "$ct" in text/html*) ;; *) continue ;; esac
  cp "$WORK/body" "$HTML/$(printf '%s' "$p" | tr '/?&=' '____')"
  # same-origin <a href> links; strip fragments; unescape &amp;
  grep -o 'href="[^"]*"' "$WORK/body" \
    | sed 's/^href="//; s/"$//; s/#.*//; s/\&amp;/\&/g' \
    | grep -E '^/([^/]|$)' | grep -v '^/logout$' | grep -v '^$' | sort -u > "$WORK/.l" \
    || true
  cat "$WORK/.l" >> "$LINKS"
  cat "$WORK/.l" >> "$QUEUE"
done < "$QUEUE"

echo "[1] crawled $(wc -l < "$SEEN") urls ($(grep -c ' 200$' "$GOT" || true) answered 200)"

echo "[2] every expected public surface reachable by links alone"
for e in $EXPECTED; do
  grep -qxF "$e 200" "$GOT" || fail "$e not reachable from / and /login by links"
done

echo "[3] shared nav on every public HTML page"
for f in "$HTML"/*; do
  for n in $NAV_LINKS; do
    grep -q "href=\"$n\"" "$f" \
      || fail "$(basename "$f") missing nav link $n (nav must be identical everywhere)"
  done
done

echo "[4] no dead same-origin links"
for l in $(sort -u "$LINKS"); do
  c=$(curl -s -L --max-time 10 -o /dev/null -w '%{http_code}' "$BASE$l")
  [ "$c" = 200 ] || fail "dead link $l (final code $c)"
done

echo "crawl-public: all sections green"