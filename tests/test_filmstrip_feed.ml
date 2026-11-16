(* TU2 tests: counts and stamps on the strip tiles (the gidget feed pattern).

   The strip is the home page (TU1): server-rendered app tiles.  TU2 bakes a
   stamp into every tile and lets the same-origin fragment
   GET /console/feed/strip re-render those tiles with live counts - the same
   shape the console already uses for /console/feed/*.

   These checks are PURE: no Postgres, no HTTP, no node.  They read the tile
   list from its ONE home (Tuna_server.Pages.Filmstrip), never from a baked
   copy, so a tile edit cannot pass here by being duplicated.

   LAW 1 (anonymous-200): every tile href is a surface that answers 200 with
   no cookie, no bearer and no session; a tile that 303s to /login would break
   the strip, so the private surfaces are never tiled.
   LAW 2 (server-rendered navigation): every tile is a plain anchor, so the
   strip renders and navigates with JS disabled; htmx only swaps the
   fragment (and a failed poll never removes the server-rendered strip). *)

module F = Tuna_server.Filmstrip

let contains s needle =
  let nl = String.length needle and n = String.length s in
  let rec go i = i + nl <= n && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let count_occurrences hay needle =
  let nl = String.length needle and n = String.length hay in
  let rec go i acc =
    if i + nl > n then acc
    else go (i + 1) (if String.sub hay i nl = needle then acc + 1 else acc)
  in
  go 0 0

let strip = F.strip_html ()

(* the handler exists in the tile module: the feed is `let feed`, not a
   second copy of the markup in pages.ml *)
let feed_is_defined () =
  ignore F.feed;
  true

let test_feed_wiring () =
  Alcotest.(check bool)
    "the strip container carries the gidget hx-get" true
    (contains strip {|hx-get="/console/feed/strip"|});
  Alcotest.(check bool) "the feed handler lives in the tile module" true
    (feed_is_defined ());
  Alcotest.(check bool) "the strip degrades to noscript" true
    (contains strip "<noscript>")

let test_tiles_are_links_with_stamps () =
  Alcotest.(check bool) "the strip has tiles" true (F.tiles <> []);
  Alcotest.(check int)
    "every tile node carries an href attribute" (List.length F.tiles)
    (count_occurrences strip {|class="tile" href="|});
  List.iter
    (fun (t : F.tile) ->
      let html = F.tile_html t in
      Alcotest.(check bool)
        (t.F.name ^ ": href=") true
        (contains html {|href="|});
      Alcotest.(check bool)
        (t.F.name ^ ": baked stamp is rendered") true
        (contains html {|<span class="stamp">|});
      Alcotest.(check bool)
        (t.F.name ^ ": baked stamp is not blank") true
        (String.trim t.F.stamp <> "");
      Alcotest.(check bool)
        (t.F.name ^ ": target is a path (anonymous-200 surface)") true
        (String.length t.F.href > 0 && t.F.href.[0] = '/'))
    F.tiles

let test_live_stamps () =
  List.iter
    (fun (t : F.tile) ->
      let baked = F.tile_html t in
      let live = F.tile_html ~stamps:[ (t.F.name, "LIVE-STAMP-PROBE") ] t in
      (* exactly ONE occurrence of the baked stamp is the stamp span: the same
         words may also appear in the tile's summary (the code tile says
         "read-only"), so count occurrences instead of testing absence. *)
      Alcotest.(check bool)
        (t.F.name ^ ": the live stamp replaces the baked one") true
        (contains live {|<span class="stamp">LIVE-STAMP-PROBE</span>|}
        && count_occurrences live t.F.stamp = count_occurrences baked t.F.stamp - 1))
    F.tiles;
  Alcotest.(check bool)
    "an unknown tile keeps its baked stamp" true
    (contains
       (F.tile_html ~stamps:[ ("nobody", "x") ] (List.hd F.tiles))
       (List.hd F.tiles).F.stamp)

(* the claims count must be a real census, not a saturated window: the store's
   [list_citation_paths] default is 200 (store/lib/store.ml), and a tile count
   clamped at 200 can never equal the registry's own total.  The tile module
   therefore asks for an explicit limit, and this pins it above the default. *)
let test_census_is_not_saturated () =
  Alcotest.(check bool)
    "the census limit is explicit and above the store default 200" true
    (F.census_limit > 200)

(* LAW 1 (anonymous-200): the strip's own page and every tile target are
   public surfaces; no tile points at a session surface such as /login,
   /grants or /identities, all of which would 303 or 401 an anonymous
   reader and break the strip. *)
let test_strip_is_anonymous () =
  Alcotest.(check bool)
    "the strip is the anonymous 200 home, not a private one" true
    (contains strip {|class="strip"|} && contains strip {|href="/|});
  List.iter
    (fun (t : F.tile) ->
      Alcotest.(check bool)
        (t.F.name ^ ": no session surface is tiled") false
        (List.mem t.F.href [ "/login"; "/logout"; "/grants"; "/identities" ]))
    F.tiles

let () =
  Alcotest.run "filmstrip_feed"
    [ ( "strip"
      , [ Alcotest.test_case "feed wiring" `Quick test_feed_wiring
        ; Alcotest.test_case "tiles are links with stamps" `Quick
            test_tiles_are_links_with_stamps
        ; Alcotest.test_case "live stamps" `Quick test_live_stamps
        ; Alcotest.test_case "census is not saturated" `Quick
            test_census_is_not_saturated
        ; Alcotest.test_case "strip is anonymous 200" `Quick
            test_strip_is_anonymous ] ) ]
