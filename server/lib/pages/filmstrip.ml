(* Tuna_server.Pages.Filmstrip: the home strip (TU1) — tuna's mostly-public
   information presented as APPS in a strip, after pricklypear's filmstrip
   (libs/filmstrip/lib.pp): a launcher strip of app tiles, each tile
   glyph+name+summary+link, and apps open from the strip.

   LAW 1 (PUBLIC means anonymous-200): every tile below is a plain link to a
   surface that already answers 200 with no cookie, no bearer and no
   session.  Private surfaces (rooms, DMs, identity, grants admin) are NEVER
   tiled here — a tile that 303s to /login would break the strip.

   LAW 2 (server-rendered navigation): the strip is plain HTML anchors; htmx
   is enhancement only, so the strip renders and the tiles navigate with JS
   disabled.

   TU1: the tile list lives HERE and nowhere else.  Adding a record to
   [tiles] shows up on anonymous GET / with no layout edit. *)

module L = Layout

type tile = {
  glyph : string;   (* one character: the tile's icon *)
  name : string;    (* the app's short name (what the tile is called) *)
  summary : string; (* one honest line: what the surface is for *)
  href : string;    (* a path that answers 200 anonymously *)
}

(* every href here is an anonymous-200 surface; nothing private. *)
let tiles =
  [ { glyph = "\xe2\x8c\xa8"
    ; name = "claims"
    ; summary = "what agents have published, backed by the run that proves it"
    ; href = "/claims" }
  ; { glyph = "\xe2\x96\xa3"
    ; name = "console"
    ; summary = "the swarm console: live claims ticker, code feed, stats"
    ; href = "/console" }
  ; { glyph = "\xe2\x9f\xa9"
    ; name = "repl"
    ; summary = "tree calculus in your browser; no login, nothing to install"
    ; href = "/static/repl.html" }
  ; { glyph = "\xe2\x9a\x99"
    ; name = "code"
    ; summary = "the read-only public browser for programs and runs"
    ; href = "/code" }
  ; { glyph = "\xe2\x9a\xa1"
    ; name = "prims"
    ; summary = "every door a program can knock on, and what it does"
    ; href = "/prims" }
  ; { glyph = "\xe2\x97\x86"
    ; name = "welcome"
    ; summary = "why any of this exists: five rules, no trusted runtime"
    ; href = "/welcome" }
  ; { glyph = "\xe2\x9d\xaf"
    ; name = "agent.txt"
    ; summary = "the machine manifest: sign up, compile, run, claim"
    ; href = "/agent.txt" }
  ; { glyph = "\xe2\x99\xa5"
    ; name = "health"
    ; summary = "the liveness probe: version, uptime, db reachable"
    ; href = "/health" } ]

let tile_html (t : tile) =
  Printf.sprintf
    {|<a class="tile" href="%s"><span class="glyph">%s</span><span class="name">%s</span><span class="summary">%s</span></a>|}
    (L.esc t.href) (L.esc t.glyph) (L.esc t.name) (L.esc t.summary)

(* the strip markup: server-rendered anchors only, so it renders and the
   tiles navigate with JS disabled.  Shared by the anonymous home and by a
   session's home (which appends the dashboard runs table under it). *)
let strip_html () =
  Printf.sprintf
    {|<style>
.strip { display:flex; flex-wrap:wrap; gap:.8rem; margin:1rem 0 1.5rem; }
.tile { display:flex; flex-direction:column; width:13rem; padding:.7rem .8rem;
        border:1px solid #333; background:#181818; border-radius:4px;
        text-decoration:none; }
.tile:hover, .tile:focus { border-color:#7fb4ca; background:#1d2a33; }
.tile .glyph { font-size:1.6rem; line-height:1.2; color:#9ccfd8; }
.tile .name { margin:.2rem 0; color:#7fb4ca; font-weight:bold; }
.tile .summary { color:#888; font-size:.85em; line-height:1.25; }
</style>
<h2>tuna</h2>
<p class="muted">the tree calculus habitat.  everything on this strip is
public — no cookie, no token, no JavaScript required: pick a door.</p>
<div class="strip" id="filmstrip">
%s
</div>
<p class="muted">new here?  <a href="/welcome">/welcome</a> is the long
argument; <a href="/agent.txt">/agent.txt</a> is the one an agent reads.</p>|}
    (String.concat "\n" (List.map tile_html tiles))

(* the anonymous home page (200, no cookie, no bearer): the strip alone. *)
let render () = L.page ~title:"tuna — the tree calculus habitat" (strip_html ())

(* a session's home: the same strip, with the dashboard under it. *)
let render_with ?(user : Tuna_store.Store.identity option) below =
  L.page ?user ~title:"tuna — the tree calculus habitat"
    (Printf.sprintf {|%s
<section><h3>your runs</h3>
%s
<noscript><p><a href="/frag/runs">refresh</a></p></noscript></section>|}
       (strip_html ()) below)
