(* the gate's renderer: a FIXED tile set through Filmstrip_shared, so the
   output is a stable capture the gate can diff.  No product data, no clock,
   no store: same bytes on every run.

   `emit` (default) prints one DATA line per tile
   (name|glyph|href|summary|stamp) - that is what equivalence means for a
   markup refactor: the tile provider's data must survive the swap byte for
   byte.  `emit markup` prints the shared tile HTML; `emit strip` prints the
   whole container, which the gate greps for the rendered contract
   (.strip-wrap, the feed path, data-glyph, 44px targets, the phone media
   query, the horizontal overflow rule). *)

let tiles : Filmstrip_shared.tile list =
  [ { glyph = "C"
    ; name = "claims"
    ; summary = "the registry, backed by runs"
    ; href = "/claims"
    ; stamp = "the registry, no cookie needed" }
  ; { glyph = "K"
    ; name = "console"
    ; summary = "the swarm console, live"
    ; href = "/console"
    ; stamp = "live feed, htmx polling only" }
  ]

let data_line (t : Filmstrip_shared.tile) =
  String.concat "|" [ t.name; t.glyph; t.href; t.summary; t.stamp ]

let () =
  match Array.to_list Sys.argv with
  | _ :: "strip" :: _ ->
      print_string
        (Filmstrip_shared.strip_html ~feed:"/console/feed/strip" tiles);
      print_newline ()
  | _ :: "markup" :: _ ->
      print_string (Filmstrip_shared.tiles_html tiles);
      print_newline ()
  | _ ->
      print_string (String.concat "\n" (List.map data_line tiles));
      print_newline ()
