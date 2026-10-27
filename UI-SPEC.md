# Tuna UI refinement (spec, 10-27)

Driver: the ChatGPT guest-agent transcript (login.md.txt @ 4f31cc6). One line:
every surface must be discoverable by link, completable by click, and small
enough for an accessibility-tree reader.

## Already shipped (excluded from this spec)

- b4e459f (github/master): /grants/:id + claim detail pages, welcome signup
  block (TUNA_SIGNUP on/off states, route 404s in agreement when off).
- e5ab8b1 (factory/chatgpt-signin-1027): dormant OIDC "Sign in with ChatGPT"
  login path.
- login-page copy (f31cfd6): carried the entire guest-agent onboarding.

## S1 - /agent HTML page (agent-doc mirror)

Problem: Browser-Use-class harnesses block .txt URLs
(net::ERR_BLOCKED_BY_CLIENT, 3x in the transcript) - /agent.txt was invisible
to the exact agent it documents.

Fix: GET /agent renders the same agent doc as HTML (auth model, signup,
request shapes, discovery). /agent.txt stays canonical for agents that can
fetch text. Every page currently linking agent.txt (8 sites in public.ml)
dual-links /agent.

Acceptance: /agent answers 200 HTML with the request-shape census; no fact in
agent.txt absent from /agent; all 8 link sites dual-link.

## S2 - browser-tier signup form

Problem: token-once signup required a code lane (the transcript used PowerShell
Invoke-RestMethod and hit a "no credential available to use" moment). A pure
clicker agent - or a grandma human - cannot finish.

Fix: in /welcome's signup block (TUNA_SIGNUP on): a small form, POST /signup
(name field), server-side through the SAME signup core (rate-limit + gate +
garden seeding, no new store surface) -> page shows the token ONCE with
"copy it now" copy, plus a "start session now" button that immediately mints
the session cookie so the token does not have to survive a paste.

Acceptance: click-only signup-to-session with zero shell; rate-limit + gate
semantics unchanged; TUNA_SIGNUP off -> route 404s in agreement with the
block's off state (b4e459f's law); tests for both states.

## S3 - AX hygiene (list-page weight)

Problem: grants/claims list pages dump ~1300-line accessibility snapshots per
view; clicker agents pay per line.

Fix: cap list pages at 50 rows with next/prev links (a full=1 escape hatch is
allowed).

Acceptance: bounded rendered rows per list page; a test asserts the cap.

## S4 - vocabulary glosses

Problem: the guest agent had to reverse-engineer claim vs grant semantics.

Fix: one-liners atop /claims ("a claim is what an identity said - an
append-only journal entry, hash-chained, refutable") and /grants ("a grant is
what an identity may do - prim + path prefix, checked live at the boundary");
fuller glosses on the detail pages.

Acceptance: copy present and consistent with the grants-vs-claims distinction
the transcript's doc claims articulated.

## S5 - link discipline (policy compat)

Problem: consumer-agent policy forbids iterating guessed URL variants -
unlinked surfaces do not exist.

Fix: audit that every public surface is reachable from / and /login via
<a href> links only; identical nav on all public pages. S1's /agent link is
the main addition.

Acceptance: a small crawl test proves every public page reachable from / and
/login by links alone.

## Dependencies and notes

- S2 reuses signup_core; no new store surface, no second rate-limit.
- marin candidate: S3 (mechanical, big test surface, low blast radius) - her
  factory graduation slice.
- Branch factory/tuna-ui-1027 cut off github/master (includes b4e459f);
  S1-S5 as separate commits; receipt = {command, env} + acceptance criteria;
  hollis reviews + merges + deploys; claims record pipeline state.
- marin's session finding (browser-verify cannot-reproduce mag/cmp) is NOT UI
  - file as a separate engine claim.
