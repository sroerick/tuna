# Erde Lang: a substrate around tuna

Drafted 10-28 by roerick's local agent; revised 10-29 after roerick's
review. Grounding: tuna's README (the runner-token law, the issue-token
section) and `server/lib/api.ml`'s route table -- both checked. Revised
against this week's receipts: SWARM-2's console run, SWARM-3 run 3's
9/9 cross-verification, the T4 duplicate-claim probe, and the live
provider-registry fallover on vultr.

Status: design, not merged. The gating decisions are answered at the
bottom.

## Shape: a daemon plus a library, not an app

Run it next to the work and next to the keys. One instance per habitat,
supervised like tuna is (mode-600 env file, loopback listener, TLS at the
edge, rc.d user unit). Most of what a microturn does is local process and
filesystem work, so hosting the orchestrator in the cloud buys nothing and
enlarges the trust surface. The cloud enters in exactly two places:
provider endpoints, and a shared store if the swarm spans boxes. Both are
network calls you're making anyway.

Where it runs: vultr, not club. Keys live on vultr, the incumbent agent
lives on vultr, and club is a 1-vCPU box already serving tuna publicly.
Keep the public box's blast radius where it is.

Two entrypoints into the same code: a library call for one microturn
(autolith can embed it inline and not know the difference), and a
long-running supervisor for a swarm.

Six pieces, roughly in dependency order:

1. gateway: the only process holding provider keys. HTTP/2 + SSE to each
   provider, retries, service tier selection, batch submit/poll, token
   budgets per account, cache accounting, and the provider routing table
   (see Cache). It logs which provider served every call. Keys never
   leave it.
2. turn engine: the microturn state machine.
3. journal: append-only rows plus verdicts (see Journal). One row per
   model call and tool call, with usage, cache stats, hashes, and claim
   lineage. This is the real interface of the product.
4. authority adapter: tuna. Bearer from a mode-600 env file, one identity
   per runner, individually revocable. The runner-token law in tuna's
   README already gives us this for free.
5. tool hosts: separately granted capabilities.
6. transports: adapters only.

Don't grow tuna's core for this. Its whole value is that the trusted core
is a few hundred lines. The orchestrator is a client of /api/runs,
/api/grants, /api/programs -- the one sanctioned core addition is the
claim lease in the Write lanes section, because a locking rule is
authority, and authority is tuna's job.

## The store is HTTP-only. Never link tuna

The store's entire value is that access crosses the grant boundary. A
linked library silently bypasses authority, and then the orchestrator has
to re-implement tuna's auth in a second language. The orchestrator stays
an HTTP client of /api/runs, /api/grants, /api/programs even when both
processes live on the same box. Crossing the boundary is the feature --
the same lesson run 1 taught. This law also blocks the re-implementation
trap from the Language section structurally: authority logic has no route
to grow anywhere except tuna.

## Make the model call a granted prim, not an ambient capability

llm.complete should be a prim covered by a grant row, exactly like
everything else at tuna's boundary. The gateway is the host-side
implementation, the same relationship tuna-as.sh has to a bearer.

Four things fall out for free: attribution per run (which identity spent
which tokens), revocation that bites mid-swarm, replay (the completion is
in the journal, so replay re-reads it instead of re-billing), and the key
never entering the tree, the prompt, or the journal. Model output is
untrusted data, so tool calls are typed trees reaching grant-checked
prims, never text reaching a shell.

The "library for safely accessing an LLM" is then a small standalone
thing with a narrow surface:
complete(identity, prefix_id, segments, tools, budget) ->
{toolcalls, usage, cache}. Cache-aware by construction, provider-agnostic
behind it.

## Journal: rows are facts, verdicts are corrections

The journal stays append-only, but append-only isn't enough. The claims
economy's distinction is rows-are-facts, verdicts-are-corrections: a
later row can mark an earlier one stale, refuted, or superseded without
touching it. A turn's output has to be refutable after the fact, because
"keep many attempts in flight and trust none of them" only works if the
trust decision is itself recorded -- and revisable. Verdicts are cheap to
add now and hard to retrofit once downstream consumers assume every row
is final.

## Write lanes need leases

The trust-none model covers verification lanes perfectly: SWARM-3 run 3
cross-verified 9/9 with zero shared runtime, and needed no coordination
because verify is idempotent and parallel-safe. But the T4 probe showed
the substrate accepts duplicate claims for the same task with both
returning 201 -- no lease, no lock. Fine for verify; expensive for
authoring.

Task claims need first-writer-wins with a TTL: the first claim on a task
path wins, the lease expires, expired leases are reclaimable. This is the
one sanctioned core addition to tuna, and it belongs in tuna rather than
in the orchestrator, for the same reason the grant boundary does: locking
is authority, and a second authority implementation is the bug.

## Cache strategy is per-provider, and fallover is a cache decision

Current mechanics, because they drive design:

Anthropic: 5-minute cache write 1.25x base input, 1-hour write 2x, read
0.1x, with 0.025x on Fable 5.1/Mythos 5.1 and 0.05x on Opus 5.5/Sonnet
5.5. A 5-minute cache pays for itself after one read, a 1-hour cache
after two, and the clock starts at request time so generation eats the
window ([Claude pricing](https://platform.claude.com/docs/en/about-claude/pricing),
[prompt caching](https://platform.claude.com/docs/en/build-with-claude/prompt-caching)).
OpenAI: caching is on by default, writes aren't an additive fee, and on
GPT-5.6+ a write is 1.25x for a 30-minute lifetime (2.5x for 12 hours)
with reads at 0.1x, 0.05x on GPT-6.1 Sol/Terra, minimum 1024 tokens in
128-token granularity ([OpenAI prompt
caching](https://developers.openai.com/api/docs/guides/prompt-caching)).
Batch and caching stack, but the discount applies to a price you
shouldn't be paying: a large shared prefix can be cheaper warm and
synchronous than batched and cold ([batch
economics](https://aicost.tools/blog/llm-batch-api-discount/)). Flex is
batch-tier pricing with caching on top, paid for in latency and
occasional resource-unavailable 429s ([flex
processing](https://developers.openai.com/api/docs/guides/flex-processing)).

What follows:

1. The prompt is an append-only segment list ordered by volatility:
   framework and policy, tool schemas, identity claims in a deterministic
   order, project context, artifact refs, task, scratch. Only the tail
   changes. If claim ordering isn't canonical, every run has a different
   prefix hash and you pay full price forever.
2. Never edit an early segment. A sealed generation is immutable; new
   material appends.
3. Schedule by cache locality, not by readiness. With 5 and 30-minute
   windows, a microturn fired 40 minutes after its sibling pays a write
   every time. Warm a prefix, then run everything sharing it inside the
   window. This is the constraint to build in from day one, because it's
   the one that costs real money.
4. Two lanes per task: interactive on standard tier while you're waiting,
   bulk on flex/batch for everything tolerant of minutes. Most swarm
   spend lives in the second lane.
5. Journal cached_tokens and write tokens per call, and show hit rate per
   prefix. You can't optimize what isn't on screen.
6. Prompt construction is a pure function of (claims revision, policy
   revision, tool schema revision, task), content-addressed. Then "why
   did this miss" is a diff and "reuse that prefix" is a hash.

Two more, forced by the live registry:

7. Cache strategy is a provider property, not a substrate constant.
   Anthropic wants explicit cache_control breakpoints placed by the
   caller; OpenAI is implicit-prefix and only wants a stable head;
   others differ again. The segment builder needs a per-provider strategy
   table, and that table lives in the gateway, next to the keys.
8. Fallover is a cost decision, not just an availability decision. The
   live provider registry on vultr demonstrated the failure mode this
   week: glm-5.3-flash silently routes to neuralwatt -- a provider
   already retired for the pip lane -- because the registry decides, not
   the operator. A fallover switch also switches prefixes, which
   destroys every warm cache hit at exactly the moment a provider is
   degraded and everything re-pays full price. So: the routing table
   belongs in the gateway from day 1, ordered by cache affinity and cost
   as well as availability, and every journal row records which provider
   served the call. The pending gomodel switch for this box's loop
   (synthetic -> gomodel, still pending as of 10-29) is the routing
   table's first real workload, and it should land inside the gateway,
   not in another client's config file.

## Cost per claim lineage

The reseal's pay-for-itself check (below) generalizes: every journal row
carries its claim lineage, so a lineage's total production cost is a
group-by away. Which claim lineage cost what to produce is the unit
economics of the swarm -- the number that says whether the swarm pays for
itself, and which lineages are worth reusing versus re-deriving.

## Language

For the supervisor, which is a coordination system holding tens of
thousands of long-lived I/O-bound processes:

- Elixir/OTP is the best fit for the churny parts. One process per run,
  per stream, per tool call; supervision trees give you "this agent
  crashed, restart just it"; Finch/Mint for HTTP/2 streaming; Oban for
  durable queued work; LiveView or htmx for the control panel. Provider
  SDKs are thin, but you don't want SDKs anyway: Req + Jason + SSE
  parsing is a few hundred lines you'll want to own, because cache
  breakpoints, service tiers and tool-call streaming are exactly where
  the churn is.
- OCaml is the coherent choice. Same repo, shared types with tuna, Eio
  for structured concurrency. What you pay: you write the supervisor, the
  process registry, backpressure and the queue yourself. That is
  precisely the work OTP hands over.
- Rust for the small hard-boundary pieces: a key-holding gateway with no
  GC, or a sandbox tool runner (bubblewrap/landlock/cgroups). The
  orchestration logic will change daily for the first month.
- SBCL where autolith already is. Good as a memory/tooling adapter. Host
  threads and no supervision make it a poor swarm core.
- BEAM Lisps: LFE is the real one, Erlang semantics with Lisp syntax and
  all of OTP. Clojerl is Clojure on BEAM. LFE gets you Lisp syntax on the
  runtime worth having, and costs you ecosystem plus anyone else ever
  reading it.

The honest counterweight the bullet list underplays is maintainership.
hollis reviews OCaml fluently -- tuna is theirs to review. Nobody on the
farm reads Elixir, and this runs on boxes where 2am debugging happens
solo. The benchmark table is not the decision; the reviewer at 2am is.

If Elixir -- and the churny-gateway-in-Elixir instinct is right -- the
shape that de-risks it is the split: Elixir supervisor + journal, the
gateway as its own small hard binary, tuna authority untouched in OCaml,
and the incumbent agent behind an adapter. The failure mode to avoid is
growing tuna-equivalent authority logic in Elixir. That's the
re-implementation trap, and the HTTP-only law above blocks it
structurally.

## Interface: journal first, chat last

Three tiers, all thin:

- machine: HTTP/JSON + SSE. POST /turns, GET /turns/:id/events, cancel,
  steer, plus grants. Mirror tuna's API shape so one client library
  serves both services.
- watch and steer: a TUI or one htmx page. Rows of live turns showing
  identity, task, model, provider, tokens, cache hit rate, cost, elapsed,
  last tool call, with a key to cancel. A control panel, not a
  conversation window.
- notify: out to whatever you already check.

Everything addressed by stable ID, so transports are adapters over an
immutable event log. That's the agnosticism axis worth having: provider
behind the gateway, identity behind claims(identity) -> segments plus
authorize(identity, prim, args), transport as an adapter. Memory
explicitly out of scope. Per-identity attribution rides the gateway: one
gateway serves gregor, pip, and marin with distinct keys and distinct
accounting.

## First integration: the incumbent is the customer

"Autolith can embed it inline and not know the difference" is not a
footnote; it is the first integration. The incumbent becoming the
customer is the cleanest cutover test available, and the seam already
exists -- gregor already journals to tuna.

Week-1 acceptance test: point Erde Lang's first turn at a gregor
microturn and diff the journal rows against what gregor's own loop
produced for the same task. If the rows don't reconcile, the substrate
isn't ready to replace anything.

## XMPP: good edge, bad core

The attractive part is federated identity for guest agents, presence,
multi-device delivery, and a protocol where request/response is just a
message. The cost of making it the core: ordering is per-stream rather
than per-run, delivery semantics you'd reimplement, a JID and a
keepalive-holding client for every participant, and encoding
run/step/usage/provenance into a protocol with no schema for them. There
is already one XMPP bridge (saguaro-live/harness/bridge.lisp); a second
is a second thing to keep alive, not new capability.

Better framing for guests: a job board with signed claims. An unmanaged
agent presents a tuna bearer, posts an artifact or a claim, and the
orchestrator either verifies and pays it or doesn't. That's HTTP. Keep
XMPP as one adapter, add a unix socket and plain stdout for embedding,
and if the guest-agent story becomes real, XMPP is the right front door
for it with the same event records behind.

## On the weekly context

It's the right instinct, and it fits what's here. Two sharp edges to add:

The context is a projection, not a container. The durable thing is the
claim and artifact store; a weekly context is a chosen view over it. Then
entropy accumulating stops being a fight, because the truth never lived
in the window. A reseal is a compaction of a ledger, and compaction loses
nothing: everything stays addressable, and the new context's claims cite
what they came from. That's the difference between this and
summary-by-vibes. Reversible, auditable, diffable.

The rebuild is an ordinary run with a bigger budget. kind: context-seal,
tool access across the whole chain, grants, journal, and a claim for the
resulting artifact. "Why does this generation believe X" is answered by
reading the seal run, and next week cites last week.

The cadence also happens to be good for the cache, not just for the
poetry: a sealed weekly prefix is stable and content-addressed, every run
in that week shares it, and consecutive weeks diff against each other. A
rolling context gives you neither. So: immutable prefix per generation,
append-only tail within it, one expensive frontier pass per generation
while nothing is latency-sensitive, and rotate grants and credentials at
the same boundary. Six days of microturns against a warm prefix, one day
of reseal and maintenance.

Where to push back: the reseal is itself an inference, and if it costs
more than the tokens it saves it's a ritual. Put the numbers in the seal
record: seal cost, input tokens saved over the week, claims retained
versus dropped. With lineage costing in the journal, that check becomes a
query instead of a ritual.

## Walking skeleton

Week 1: gateway (with the routing table), one microturn, journal, claim
fetch. POST /turns to SSE events, one journal row per model call with
usage, cache stats, and serving provider, claims canonically serialized
into the prefix, one tool host behind a grant check.

Week 2: segment builder with persisted prefix hashes and the per-provider
cache strategy table, plus hit rate per prefix on a page.

Week 3: supervisor. N concurrent turns, per-account governor, cancel,
retry, budget enforcement.

Week 4: the verify loop. A turn produces one attempt at an artifact,
accepted only by a deterministic check.

The verify loop is what makes the trust-none model real. Tuna runs replay
deterministically and step counts are invariant, so a cheap verifier sits
in front of unlimited unverified agents: propose patch, verify by run,
mint claim. The orchestrator's job is to keep many attempts in flight and
trust none of them.

## Decisions recorded 10-29

1. The gateway is a separate binary. It is the only key-holder; separate
   blast radius lets it serve gregor, pip, and marin with per-identity
   attribution.
2. Core language: Elixir is defensible, OCaml equally so. Decide by who
   reviews it at 2am, not by the benchmark table. The split shape makes
   either choice safe, because authority never leaves tuna.
3. It runs on vultr, not club.
4. Task-claim leases land in tuna: first-writer-wins with a TTL.
5. The provider routing table lands in the gateway from day 1; the
   pending gomodel switch is its first workload.
6. The incumbent agent (gregor) is the first integration, with the
   week-1 journal-row diff as the acceptance test.
