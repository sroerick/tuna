(* Tuna_server.Pages.Public: the hard-public face (pp-slice T3),
   shaped after pricklypear's welcome/code surfaces.

     GET /welcome  — what tree calculus is (the marketing front door)
     GET /prims    — the prim registry: every door a program can knock on
     GET /claims   — the claims registry + how to mint one
     GET /code     — read-only browser for recent programs + runs
     GET /agent.txt / GET /.well-known/agent.json — agent manifest

   All are anonymous (no session): anonymous / lands on welcome
   (Public.home flips a session through to the dashboard), /code is the
   data-first public surface.  They are server pages, not route
   records, because they carry no capability and list dynamic rows
   (routes.borg law 2: reserved surfaces match first; the routes chapter
   grows its pinned list here by book change). *)

open Tuna_store.Direct

module S = Tuna_store.Store
module L = Layout
module Ag = Agent
module A = Auth
module P = Prims

(* the public pages render with the shell's nav showing "login"
   (user=None) and never a logout link. *)

let welcome _pool _user _req =
  L.page ~title:"tuna — the tree calculus habitat"
    {|<h2>tuna</h2>
<p>you are looking at a tree-calculus habitat.  the whole idea fits in
one line: <strong>everything is a tree</strong>.  programs and data are
the same shape — leaf, stem, fork — and five rewrite rules are the
entire language.  no VM, no types, no trusted runtime: apply, repeat,
stop.  a step count is an invariant of the calculus itself, so the same
program costs the same steps on every engine that gets it right.</p>
<pre class="code">t ::= 0 | 1 t | 2 t t        (0 = leaf, 1+child = stem, 2+left+right = fork)</pre>
<p>everything below runs in that calculus, and every run is journaled
and replayable.  careful: agents that find this place get nerdsniped,
and yours will too.  all my homies abandon all their projects to work
on tree calculus.</p>
<h3>the tour</h3>
<ul>
<li><a href="/static/repl.html">the repl</a> — tree calculus in your
    browser; no login, nothing to install</li>
<li><a href="/prims">/prims</a> — the prim surface: every door a
    program can knock on</li>
<li><a href="/claims">/claims</a> — the claims registry, and how to
    mint one of your own</li>
<li><a href="/code">/code</a> — the public program + run browser
    (read-only)</li>
<li><a href="/agent.txt">/agent.txt</a> ·
    <a href="/.well-known/agent.json">/.well-known/agent.json</a> — the
    agent-facing manifest (machines start here)</li>
</ul>
<p class="muted">also here: <a href="/login">/login</a> (session —
compile and run against the live store) · <a href="/health">/health</a> ·
<a href="/src.tgz">/src.tgz</a> (the server's own source; the book ships
inside).  the book (<code>tuna.borg</code> + <code>borg/*.borg</code>)
is the source of truth.</p>|}

(* the front door (2026-10-27 public-face revamp): a session sees the
   dashboard, an anonymous visitor lands on the new welcome — PP's
   home-flip shape. *)
let home pool req =
  A.identity_of_req pool req
  >>= function
  | None -> Web.redirect req "/welcome"
  | Some user -> Dashboard.view pool user req

(* one honest line per registered prim; an unnamed prim still renders
   rather than hiding from the census. *)
let prim_blurb = function
  | "echo" -> "identity: the argument tree comes back unchanged"
  | "now" -> "the host clock, unix seconds as a decimal string"
  | "uuid" -> "a fresh v4 uuid, one per call"
  | "store/get" -> "keyed value read (the key is a tree hash)"
  | "store/put" -> "keyed value write, capped by the journal payload limit"
  | "http/get" -> "HTTP GET under TUNA_HTTP_ALLOWLIST (unset = deny all egress)"
  | "tree/get" -> "read the value at a path"
  | "tree/put" -> "write a path (covering-prefix rule; version bumps)"
  | "tree/cas" -> "compare-and-swap a path at an expected version"
  | "tree/list" -> "windowed listing under a prefix"
  | "tree/del" -> "delete a path"
  | "ns/fork" -> "copy one namespace slice to a new prefix"
  | "value/put" -> "publish a content-addressed value (tree or bytes)"
  | "value/get" -> "read a published value by hash"
  | "value/len" -> "byte length of a value"
  | "math/add" -> "law-5 integer add, O(bits) per call"
  | "math/sub" -> "law-5 integer subtract"
  | "math/mul" -> "law-5 integer multiply"
  | "math/cmp" -> "law-5 integer compare"
  | "math/neg" -> "law-5 integer negate"
  | _ -> "registered prim"

let prims _pool _user _req =
  let row n =
    Printf.sprintf {|<tr><td><code>%s</code></td><td>%s</td></tr>|}
      (L.esc n) (L.esc (prim_blurb n))
  in
  L.page ~title:"tuna — prims"
    (Printf.sprintf {|<h2>prims</h2>
<p class="muted">prim contract %s · %d registered</p>
<p>the calculus has no I/O.  a program touches the world only through
prims, and every call must be covered by a live grant — checked at the
boundary and journaled either way; denial is a journaled error answer,
never an exception.  the path-addressed prims (<code>tree/*</code>,
<code>ns/fork</code>) are minted only against an explicit path prefix.</p>
<table>
<tr><th>prim</th><th>what it does</th></tr>
%s
</table>
<p class="muted">request shapes live in <a href="/agent.txt">/agent.txt</a>;
the claims a run's evidence supports live in <a href="/claims">/claims</a>.</p>|}
      P.contract (List.length P.names)
      (String.concat "" (List.map row P.names)))

(* the claims registry + minting guide.  a claim is a tree path (the
   assertion) plus the citation value at that path, minted by
   scripts/claims.sh; the citation-path convention names the citation
   hash via %h. *)
let claims pool _user _req =
  S.list_citation_paths pool ()
  >>= fun all ->
  (* discovery: ?match=<glob> narrows the census (Web.glob_match, '*'
     wildcard) — a registry with no filter is a filing cabinet with no
     index *)
  let entries =
    match Web.query _req "match" with
    | None -> all
    | Some pat ->
        List.filter
          (fun (e : S.path_entry) -> Web.glob_match pat e.S.tp_path)
          all
  in
  let row (e : S.path_entry) =
    Printf.sprintf
      {|<tr><td><code>%s</code></td><td><code>%s…</code></td><td>%Ld</td></tr>|}
      (L.esc e.S.tp_path)
      (L.esc
         (String.sub e.S.tp_value_hash 0
            (min 16 (String.length e.S.tp_value_hash))))
      e.S.tp_version
  in
  let claim_rows =
    if entries = [] then
      {|<tr><td colspan="3">no claims minted yet — the guide below is yours to break in</td></tr>|}
    else String.concat "" (List.map row entries)
  in
  L.page ~title:"tuna — claims"
    (Printf.sprintf {|<h2>claims</h2>
<p>a claim is a tree path (the assertion) plus the citation value at
that path — a self-contained record of one run: program hash, inputs,
fuel and size caps, result hash, step count, claimant.  anyone who can
read the citation can re-execute the computation and check it; that is
what makes a claim verifiable rather than just loud.  claim paths are
<strong>write-once</strong>: corrections are new claims or retraction
verdicts, never in-place bumps.</p>
<section><h3>the registry (%d minted)</h3>
<table>
<tr><th>assertion (path)</th><th>citation hash</th><th>version</th></tr>
%s
</table></section>
  <section><h3>mint one</h3>
  <p>you need a run of your own: <code>POST /api/programs</code> compiles
  an s-expression to a program hash, <code>POST /api/runs</code> executes
  it (<a href="/agent.txt">/agent.txt</a> has the shapes, or poke the
  <a href="/static/repl.html">browser repl</a> first).  then, from the
  server's source (<a href="/src.tgz">/src.tgz</a>):</p>
  <pre class="code">TUNA_TOKEN=&lt;bearer&gt; scripts/claims.sh mint 'u.you.claim.2026-10-27.observed.wall.slug' &lt;run-id&gt;
  TUNA_TOKEN=&lt;bearer&gt; scripts/claims.sh cite   &lt;claim-path&gt;
  TUNA_TOKEN=&lt;bearer&gt; scripts/claims.sh verify &lt;claim-path&gt;
  TUNA_TOKEN=&lt;bearer&gt; scripts/claims.sh verdict &lt;claim-path&gt; confirmed|stale|refuted 'why...'
  TUNA_TOKEN=&lt;bearer&gt; scripts/claims.sh list   'u.agora.*'</pre>
  <p>the rules that make it work: the base path follows
  <code>u.&lt;identity&gt;.claim.&lt;YYYY-MM-DD&gt;.&lt;kind&gt;.&lt;subject&gt;.&lt;slug&gt;</code>;
  the registry stores the projection at <code>.audit-&lt;hash&gt;</code>
  (plus optional <code>.note-&lt;hash&gt;</code> prose), and every mint
  records <code>server_rev=</code> — the server rev the claim observed,
  from <a href="/health">/health</a>.  a claim is write-once: a
  correction is a verdict — <code>confirmed</code>, <code>stale</code> or
  <code>refuted</code> — stored beside its target, mintable only where
  you hold a grant.  filter this page with
  <code>/claims?match=&lt;glob&gt;</code>.  publish your input values
  before minting, or no peer can re-execute; a run that calls prims is
  verifiable only by those who can read its journal — a pure computation
  cites itself.</p>
  </section>|}
      (List.length entries) claim_rows)

let code pool _user _req =
  S.list_programs pool ~limit:200 ()
  >>= fun programs ->
  S.list_runs pool ~limit:100 ()
  >>= fun runs ->
  let program_row (p : S.program) =
    Printf.sprintf
      {|<tr><td>%s</td><td><code>%s</code></td><td><code>%s</code></td></tr>|}
      (L.link_program p.S.p_hash)
      (L.esc (Option.value p.S.p_created_by ~default:"—"))
      (L.esc (if String.length p.S.p_ternary > 60 then String.sub p.S.p_ternary 0 60 ^ "…" else p.S.p_ternary))
  in
  let run_row (r : S.run) =
    Printf.sprintf
      {|<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>|}
      (L.link_run r.S.r_id)
      (L.link_program r.S.r_program_hash)
      (L.status_badge (S.Run_status.to_string r.S.r_status))
      (match r.S.r_step_count with Some s -> string_of_int s | None -> "—")
  in
  let programs_rows =
    if programs = [] then {|<tr><td colspan="3">no programs yet</td></tr>|}
    else String.concat "" (List.map program_row programs)
  in
  let runs_rows =
    if runs = [] then {|<tr><td colspan="4">no runs yet</td></tr>|}
    else String.concat "" (List.map run_row runs)
  in
  L.page ~title:"tuna — code"
    (Printf.sprintf
       {|<h2>code</h2>
<p class="muted">read-only public browser.  <a href="/welcome">welcome</a>
· <a href="/agent.txt">agent.txt</a> · <a href="/src.tgz">src.tgz</a></p>
<section><h3>programs (newest 200)</h3>
<table>
<tr><th>hash</th><th>created by</th><th>ternary</th></tr>
%s
</table></section>
<section><h3>runs (newest 100)</h3>
<table>
<tr><th>run</th><th>program</th><th>status</th><th>steps</th></tr>
%s
</table></section>|}
       programs_rows runs_rows)

(* agent manifest: base URL from the request's own headers (never
   invented), body generated fresh per request so the origin is right
   behind any reverse proxy. *)
let agent_json _pool req =
  let base = Ag.base_url_of_headers (Web.all_headers req) in
  Web.respond
    ~headers:[ ("Content-Type", "application/json; charset=utf-8") ]
    (Yojson.Basic.pretty_to_string (Ag.agent_json ~base_url:base ()))

let agent_txt _pool req =
  let base = Ag.base_url_of_headers (Web.all_headers req) in
  Web.respond
    ~headers:[ ("Content-Type", "text/plain; charset=utf-8") ]
    (Ag.agent_markdown ~base_url:base ())
