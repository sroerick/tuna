(* Filmstrip_shared: the shared launcher strip - the monorepo's first brick.

   ONE interface, two products: tuna (this repo's habitat) and pricklypear
   both render their public surfaces as a strip of app tiles through this
   module, so a refinement to the strip - the 44px tap target, the phone
   layout, the never-a-bare-number stamp rule - lands in both products at
   once instead of in two copies that can drift.

   THE CONTRACT (a tile is data; the markup is here and nowhere else):
   - a tile is glyph + name + one-line summary + href + a BAKED stamp;
   - every tile is a plain anchor, server-rendered, so the strip renders and
     navigates with JavaScript disabled (htmx may SWAP the strip, never
     build it; a failed poll leaves the server-rendered strip in place);
   - every live number carries a stamp (bake or live), never a bare number,
     and a blank live stamp falls back to the baked one;
   - the strip scrolls horizontally at phone width (the .strip-wrap
     scroller) and never overflows the page (body overflow-x:hidden);
   - every tile is a tap target of at least 44px (min-height/min-width on
     .tile), which the phone media query keeps true.

   The tile markup carries data-glyph and data-name so a gate, or any later
   CSS/script in either product, can read the contract without parsing
   class names. *)

type tile = {
  glyph : string;   (* one character: the tile's icon *)
  name : string;    (* the app's short name (what the tile is called) *)
  summary : string; (* one honest line: what the surface is for *)
  href : string;    (* a path the product serves without a session *)
  stamp : string;   (* the BAKED stamp: what the tile says without JS *)
}

(* the strip renders inside a page that already escapes what it owns; the
   library escapes the tile fields it interpolates, so no product can inject
   markup through a tile. *)
let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* a live stamp that came back blank must not blank the tile: the baked
   stamp is the fallback, so every tile carries one honest line in BOTH
   states (the no-JS page and the polled fragment). *)
let stamp_of stamps (t : tile) =
  match List.assoc_opt t.name stamps with
  | Some s when String.trim s <> "" -> s
  | _ -> t.stamp

(* every tile is a plain anchor with an href and a stamp; the href is the
   whole navigation, so the tile opens with JS disabled. *)
let tile_html ?(stamps = []) (t : tile) =
  Printf.sprintf
    {|<a class="tile" href="%s" data-glyph="%s" data-name="%s"><span class="glyph" aria-hidden="true">%s</span><span class="name">%s</span><span class="summary">%s</span><span class="stamp">%s</span></a>|}
    (esc t.href) (esc t.glyph) (esc t.name) (esc t.glyph) (esc t.name)
    (esc t.summary) (esc (stamp_of stamps t))

let tiles_html ?(stamps = []) tiles =
  String.concat "\n" (List.map (fun t -> tile_html ~stamps t) tiles)

(* The strip's CSS lives here with the markup: one copy, both products.
   .strip-wrap is the scroller (the strip itself must not push the page
   wide), .tile is a >= 44px target at every width, and the phone media
   query narrows the column instead of shrinking the target. *)
let css =
  {|<style>
.strip-wrap { overflow-x:auto; -webkit-overflow-scrolling:touch; margin:1rem 0 1.5rem; }
.strip { display:flex; flex-wrap:nowrap; gap:.8rem; padding-bottom:.35rem; }
.tile { display:flex; flex-direction:column; width:13rem; min-width:9rem;
        min-height:44px; padding:.7rem .8rem; border:1px solid #333;
        background:#181818; border-radius:4px; text-decoration:none; }
.tile:hover, .tile:focus { border-color:#7fb4ca; background:#1d2a33; }
.tile .glyph { font-size:1.6rem; line-height:1.2; color:#9ccfd8; }
.tile .name { margin:.2rem 0; color:#7fb4ca; font-weight:bold; }
.tile .summary { color:#888; font-size:.85em; line-height:1.25; }
.tile .stamp { margin-top:.35rem; color:#9ccfd8; font-size:.75em;
               letter-spacing:.03em; }
body { overflow-x:hidden; }
@media (max-width:480px) { .tile { width:11rem; min-height:44px; } }
</style>|}

(* the strip markup: the scroller, the container and the tiles.  A product
   passes its own feed path (the same-origin fragment that re-renders these
   tiles with live data) and the product's own prose around the strip; the
   grid, the CSS and the anchors are shared. *)
let strip_html ?(feed = "") ?(id = "filmstrip") ?(stamps = []) tiles =
  let feed_attr =
    if feed = "" then ""
    else Printf.sprintf {| hx-get="%s" hx-trigger="every 30s"|} (esc feed)
  in
  Printf.sprintf
    {|%s
<div class="strip-wrap">
<div class="strip" id="%s"%s hx-swap="innerHTML">
%s
</div>
</div>|}
    css (esc id) feed_attr (tiles_html ~stamps tiles)
