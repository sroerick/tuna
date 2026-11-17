(* Tuna_strip.Strip_adapter: the ONE place tuna meets the shared filmstrip.

   The strip markup, the responsive CSS and the tile contract live in
   filmstrip-shared/lib.ml (module Filmstrip_shared); tuna supplies only the
   data its public surfaces make - the tile list stays in its one home,
   Tuna_server.Pages.Filmstrip.  This adapter exists so tuna's page module
   depends on one small library instead of on the shared package directly:
   the record type below is a TYPE EQUATION with the shared tile, so a tuna
   tile and a shared tile cannot drift apart (adding a field to one is a
   build error in the other).

   The shared tile markup is a plain anchor carrying data-glyph and
   data-name (plus the baked/live stamp span), so the strip is server
   rendered and navigates with JS disabled. *)

type tile = Filmstrip_shared.tile = {
  glyph : string;
  name : string;
  summary : string;
  href : string;
  stamp : string;
}

let tile_html = Filmstrip_shared.tile_html
let tiles_html = Filmstrip_shared.tiles_html
let strip_html = Filmstrip_shared.strip_html
let css = Filmstrip_shared.css
