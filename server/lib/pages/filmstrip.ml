(* Tuna_server.Pages.Filmstrip: the home strip (TU1 + TU2) — tuna's
   mostly-public information presented as APPS in a strip, after
   pricklypear's filmstrip (libs/filmstrip/lib.pp): a launcher strip of app
   tiles, each tile glyph+name+summary+link, and apps open from the strip.

   LAW 1 (PUBLIC means anonymous-200): every tile below is a plain link to a
   surface that already answers 200 with no cookie, no bearer and no
   session.  Private surfaces (rooms, DMs, identity, grants admin) are NEVER
   tiled here — a tile that 303s to /login would break the strip.

   LAW 2 (server-rendered navigation): the strip is plain HTML anchors; htmx
   is enhancement only, so the strip renders and the tiles navigate with JS
   disabled.

   TU1: the tile list lives HERE and nowhere else.  Adding a record to
   [tiles] shows up on anonymous GET / with no layout edit.

   TU2 (counts and stamps): the strip follows the gidget feed pattern the
   console already uses (/console/feed/*, pages/console.ml): the page bakes
   the tile markup once, and the same-origin fragment
   GET /console/feed/strip re-renders the tiles with live data on a poll
   (hx-get on the [.strip] container, hx-swap=innerHTML).  Every tile hence
   carries a stamp in BOTH states: a baked stamp server-rendered into the
   page (so the no-JS reader still sees one honest line per tile) and the
   live stamp the feed swaps in.  A failed poll never removes the strip —
   the container is server-rendered and htmx only swaps a 2xx response. *)

module L = Layout
module S = Tuna_store.Store
open Tuna_store.Direct

type tile = {
  glyph : string;   (* one character: the tile's icon *)
  name : string;    (* the app's short name (what the tile is called) *)
  summary : string; (* one honest line: what the surface is for *)
  href : string;    (* a path that answers 200 anonymously *)
  stamp : string;   (* the BAKED stamp: what the tile says without JS *)
}

(* every href here is an anonymous-200 surface; nothing private. *)
let tiles =
  [ { glyph = "\xe2\x8c\xa8"
    ; name = "claims"
    ; summary = "what agents have published, backed by the run that proves it"
    ; href = "/claims"
    ; stamp = "the registry — no cookie needed" }
  ; { glyph = "\xe2\x96\xa3"
    ; name = "console"
    ; summary = "the swarm console: live claims ticker, code feed, stats"
    ; href = "/console"
    ; stamp = "live feed — htmx polling only" }
  ; { glyph = "\xe2\x9f\xa9"
    ; name = "repl"
    ; summary = "tree calculus in your browser; no login, nothing to install"
    ; href = "/static/repl.html"
    ; stamp = "no login, no install" }
  ; { glyph = "\xe2\x9a\x99"
    ; name = "code"
    ; summary = "the read-only public browser for programs and runs"
    ; href = "/code"
    ; stamp = "read-only" }
  ; { glyph = "\xe2\x9a\xa1"
    ; name = "prims"
    ; summary = "every door a program can knock on, and what it does"
    ; href = "/prims"
    ; stamp = "the prim registry" }
  ; { glyph = "\xe2\x97\x86"
    ; name = "welcome"
    ; summary = "why any of this exists: five rules, no trusted runtime"
    ; href = "/welcome"
    ; stamp = "the long argument" }
  ; { glyph = "\xe2\x9d\xaf"
    ; name = "agent.txt"
    ; summary = "the machine manifest: sign up, compile, run, claim"
    ; href = "/agent.txt"
    ; stamp = "machine manifest" }
  ; { glyph = "\xe2\x99\xa5"
    ; name = "health"
    ; summary = "the liveness probe: version, uptime, db reachable"
    ; href = "/health"
    ; stamp = "liveness probe" }
  ; { glyph = "✉"
    ; name = "messages"
    ; summary = "the public message log: append-only, readable by anyone"
    ; href = "/api/messages"
    ; stamp = "append-only log" } ]

(* -- live data (the feed's half of the gidget pattern) ----------------- *)

let contains s needle =
  let nl = String.length needle and n = String.length s in
  let rec go i = i + nl <= n && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let seg p n =
  let parts = String.split_on_char '.' p in
  if List.length parts > n then Some (List.nth parts n) else None

(* same census rule the /claims registry and the console ticker use: a
   claim is a u.<id>.claim.* row; verdict-, audit-, note- and citation-
   rows are projections beside a claim, not claims. *)
let is_claim_row (e : S.path_entry) =
  (not (contains e.S.tp_path ".verdict-"))
  && not (contains e.S.tp_path ".audit-")
  && not (contains e.S.tp_path ".note-")
  && not (contains e.S.tp_path "citation-")
  &&
  match (seg e.S.tp_path 0, seg e.S.tp_path 2) with
  | Some "u", Some "claim" -> true
  | _ -> false

let is_verdict_row (e : S.path_entry) = contains e.S.tp_path ".verdict-"

let newest_updated rows =
  List.fold_left
    (fun acc (e : S.path_entry) ->
      match e.S.tp_updated_at with
      | Some t when t > acc -> t
      | _ -> acc)
    "" rows

let date10 s = if String.length s >= 10 then String.sub s 0 10 else s

let utc_clock () =
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%02d:%02d:%02d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

(* the newest commit line from a 30s-TTL cache over the server's own
   checkout (TUNA_SOURCE_DIR, default cwd; read-only).  Local to this
   module on purpose: reaching into Pages.Console would make
   Console -> Public -> Filmstrip -> Console a build cycle. *)
let git_cache : (float * string option) ref = ref (0.0, None)

let git_head () =
  let dir =
    match Sys.getenv_opt "TUNA_SOURCE_DIR" with
    | Some d when String.trim d <> "" -> String.trim d
    | _ -> Sys.getcwd ()
  in
  let now = Unix.time () in
  let ts, code = !git_cache in
  if now -. ts < 30.0 then code
  else
    let code =
      let cmd =
        Filename.quote_command "git"
          [ "-C"; dir; "log"; "-n"; "1"; "--date=short"
          ; "--pretty=format:%h%x1f%cs" ]
      in
      match Unix.open_process_in cmd with
      | exception _ -> None
      | ic ->
          let line = try input_line ic with End_of_file -> "" in
          (match Unix.close_process_in ic with
           | Unix.WEXITED 0 when line <> "" -> Some line
           | _ -> None)
    in
    git_cache := (now, code);
    code

let code_stamp () =
  match git_head () with
  | None -> "source feed idle"
  | Some line -> (
      match String.split_on_char '\x1f' line with
      | _hash :: date :: _ -> "last commit " ^ date
      | _ -> "last commit" )

(* the census limit is the store's OWN shared ceiling, not a local number:
   the old default was 200 (store/lib/store.ml), and a truncated census
   yields a tile count that can never equal the registry's own total — the
   tile would silently saturate below /claims.  Sharing the store's constant
   means the tile, /claims and the console census cannot drift apart. *)
let census_limit = S.census_limit

(* the newest log entry, asked of the store's newest-row query - never
   [list_messages] with a limit (that query is oldest-first: its last row
   is row N, not the latest). *)
let message_stamp = function
  | None -> "no messages yet"
  | Some (m : S.message) -> (
      let ts = date10 m.S.msg_created_at in
      if ts = "" then "append-only log" else "last message " ^ ts)

let live_stamps pool =
  S.list_citation_paths pool ~limit:census_limit () >>= fun census ->
  S.newest_message pool () >>= fun newest_msg ->
  let claims = List.filter is_claim_row census in
  let verdicts = List.filter is_verdict_row census in
  let newest = date10 (newest_updated census) in
  let claims_stamp =
    Printf.sprintf "%d claims%s" (List.length claims)
      (if newest = "" then "" else Printf.sprintf ", newest %s" newest)
  in
  return
    [ ("claims", claims_stamp)
    ; ("console", Printf.sprintf "%d verdicts" (List.length verdicts))
    ; ("messages", message_stamp newest_msg)
    ; ("code", code_stamp ())
    ; ("health", Printf.sprintf "ok %s UTC" (utc_clock ())) ]

(* -- markup -------------------------------------------------------------- *)

(* a live stamp that came back blank must not blank the tile: the baked
   stamp is the fallback, so every tile carries one honest line in both
   states (the no-JS page and the polled fragment). *)
let stamp_of stamps (t : tile) =
  match List.assoc_opt t.name stamps with
  | Some s when String.trim s <> "" -> s
  | _ -> t.stamp

(* every tile is a plain anchor with an href and a stamp; the href is the
   whole navigation, so the tile opens with JS disabled. *)
let tile_html ?(stamps = []) (t : tile) =
  Printf.sprintf
    {|<a class="tile" href="%s"><span class="glyph">%s</span><span class="name">%s</span><span class="summary">%s</span><span class="stamp">%s</span></a>|}
    (L.esc t.href) (L.esc t.glyph) (L.esc t.name) (L.esc t.summary)
    (L.esc (stamp_of stamps t))

let tiles_html ?(stamps = []) () =
  String.concat "\n" (List.map (fun t -> tile_html ~stamps t) tiles)

(* the strip markup: server-rendered anchors only, so it renders and the
   tiles navigate with JS disabled.  Shared by the anonymous home and by a
   session's home (which appends the dashboard runs table under it).  The
   container's hx-get is the gidget feed: same-origin, anonymous, and the
   swap is innerHTML so the baked tiles stay if a poll fails. *)
let strip_html ?(stamps = []) () =
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
.tile .stamp { margin-top:.35rem; color:#9ccfd8; font-size:.75em;
               letter-spacing:.03em; }
</style>
<h2>tuna</h2>
<p class="muted">the tree calculus habitat.  everything on this strip is
public — no cookie, no token, no JavaScript required: pick a door.</p>
<div class="strip" id="filmstrip" hx-get="/console/feed/strip" hx-trigger="every 30s" hx-swap="innerHTML">
%s
</div>
<noscript><p class="muted">the counts and stamps on these tiles refresh by
polling /console/feed/strip; this server-rendered strip is complete without
it — every tile is a plain link.</p></noscript>
<p class="muted">new here?  <a href="/welcome">/welcome</a> is the long
argument; <a href="/agent.txt">/agent.txt</a> is the one an agent reads.</p>|}
    (tiles_html ~stamps ())

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

(* GET /console/feed/strip — the filmstrip's gidget fragment: the same tile
   markup the page bakes, re-rendered with live counts and stamps.  Same
   origin, anonymous, read-only GET (routes.borg law 2: reserved surface,
   pinned in pages.ml next to the console's /console/feed/* feeds). *)
let feed pool _user _req =
  live_stamps pool >>= fun stamps -> Web.html (tiles_html ~stamps ())
