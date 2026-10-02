(* Tuna_server.Pages.Public: the hard-public face (pp-slice T3),
   shaped after pricklypear's welcome/code surfaces.

     GET /welcome  — what tuna is, the calculus in two lines, links
     GET /code     — read-only browser for recent programs + runs
     GET /agent.txt / GET /.well-known/agent.json — agent manifest

   Both are anonymous (no session): welcome is the front door, /code is
   the data-first public surface.  They are server pages, not route
   records, because they carry no capability and list dynamic rows
   (routes.borg law 2: reserved surfaces match first; the routes chapter
   grows its pinned list here by book change). *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout
module Ag = Agent

(* the public pages render with the shell's nav showing "login"
   (user=None) and never a logout link. *)

let welcome _pool _user _req =
  L.page ~title:"tuna — welcome"
    {|<h2>tuna</h2>
<p>a standalone tree-calculus evaluator habitat.  programs are trees;
the calculus in two lines:</p>
<pre class="code">Leaf | Stem of t | Fork of t * t
0 = leaf, 1+child = stem, 2+left+right = fork (canonical ternary)</pre>
<p>A <em>step</em> is one triage-rule firing; the two wrapper
applications are application, not steps — so step counts are an invariant
of the calculus, not of the engine.  Every run is journaled and
replayable.</p>
<ul>
<li><a href="/code">/code</a> — read-only program + run browser</li>
<li><a href="/agent.txt">/agent.txt</a> · <a href="/.well-known/agent.json">/.well-known/agent.json</a>
    — agent-facing manifest</li>
<li><a href="/src.tgz">/src.tgz</a> — the server's own source tarball</li>
<li><a href="/login">/login</a> — sign in (session) to run and compile</li>
<li><a href="/health">/health</a> — liveness</li>
</ul>
<p class="muted">The book (<code>tuna.borg</code> + <code>borg/*.borg</code>)
is the source of truth; it ships inside the source tarball.</p>|}

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
  let base = Ag.base_url_of_headers (Dream.all_headers req) in
  Dream.respond
    ~headers:[ ("Content-Type", "application/json; charset=utf-8") ]
    (Yojson.Basic.pretty_to_string (Ag.agent_json ~base_url:base ()))

let agent_txt _pool req =
  let base = Ag.base_url_of_headers (Dream.all_headers req) in
  Dream.respond
    ~headers:[ ("Content-Type", "text/plain; charset=utf-8") ]
    (Ag.agent_markdown ~base_url:base ())
