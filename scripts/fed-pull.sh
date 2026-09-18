#!/usr/bin/env bash
# fed-pull.sh - pull one value from a peer tuna instance (M12 F1 value
# exchange, borg/federation.borg) and verify the rehash contract: the
# peer trusts nothing but the hash, so the payload must rehash to the
# address it was fetched under.
#
#   usage: fed-pull.sh <base-url> <bearer-token> <hash>
#
# kind tree carries the canonical ternary text (a program hash resolves
# as its own ternary); kind bytes carries base64.  Exits 0 iff the
# payload rehashes to <hash> (lowercased); a 404 or any mismatch is a
# nonzero exit, never an absorbed error.
set -e
base=$1
token=$2
hash=$(printf %s "$3" | tr 'A-F' 'a-f')
if ! curl -sf -m 30 -X GET "$base/api/fed/value/$hash" \
     -H "Authorization: Bearer $token" -o /tmp/fed-pull.$$ ; then
  echo "fed-pull: peer $base has no value $hash (or unreachable)" >&2
  rm -f /tmp/fed-pull.$$
  exit 1
fi
resp=$(cat /tmp/fed-pull.$$); rm -f /tmp/fed-pull.$$
kind=$(printf %s "$resp" | sed -n 's/.*"kind":"\([a-z]*\)".*/\1/p')
payload=$(printf %s "$resp" | sed -n 's/.*"payload":"\([^"]*\)".*/\1/p')
if [ -z "$kind" ] || [ -z "$payload" ]; then
  echo "fed-pull: unparseable response from $base" >&2
  exit 1
fi
case $kind in
  tree)  bytes=$(printf %s "$payload") ;;
  bytes) bytes=$(printf %s "$payload" | openssl base64 -d -A) ;;
  *) echo "fed-pull: unknown kind $kind" >&2; exit 1 ;;
esac
got=$(printf %s "$bytes" | openssl dgst -sha256 -r | cut -d' ' -f1)
len=$(printf %s "$bytes" | wc -c | tr -d ' ')
if [ "$got" = "$hash" ]; then
  echo "fed-pull: verified kind=$kind len=$len hash=$hash"
else
  echo "fed-pull: REHASH MISMATCH got=$got want=$hash" >&2
  exit 1
fi
