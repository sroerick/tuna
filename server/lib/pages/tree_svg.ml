(* Tuna_server.Pages.Tree_svg: render a Tuna.Tree.t as inline SVG in
   the tree-calculus book's visual language (Barry Jay's tree_book.pdf
   figures; the graphviz reduction-rule diagrams in reference/): glyph
   nodes for leaf/stem/fork, solid structure edges, and DECODED boxes
   layered on - a subtree that is a cstr string renders as a quoted
   box, a law-5 int as a number box, so a todo record reads as
   key-state -> open instead of ninety lines of "fork".

   This is the server-side twin of scripts/tern.py (same codecs, same
   best-effort caveats): a comparison or a display should never again
   be done by eye on raw ternary.

   Decode order mirrors the ambiguity law: int BEFORE string (a law-5
   int's sign+bits is also a plausible char list), and a string decode
   is only accepted when every char is printable - otherwise [1 2]
   would render as the "string" "\002\004".  Trees that decode no
   better than their structure render as raw glyphs. *)

open Tuna.Tree

let slot_w = 44 (* px per leaf slot *)
let level_h = 76 (* px per depth level *)
let node_r = 11 (* glyph radius *)

(* ---------- decoders (mirrors of common/lib codecs) ---------- *)

let rec bits_of_list t =
  match t with
  | Leaf -> Some []
  | Fork (Leaf, tl) -> ( match bits_of_list tl with Some bs -> Some (false :: bs) | None -> None )
  | Fork (Stem Leaf, tl) -> ( match bits_of_list tl with Some bs -> Some (true :: bs) | None -> None )
  | _ -> None

let int_of_bits bs =
  List.fold_left (fun (v, i) b -> (v + (if b then 1 lsl i else 0), i + 1)) (0, 0) bs |> fst

(* ints vs 2-lists are THE SAME TREES by the one-form law: [key value]
   with bool-ish components IS a law-5 int (e.g. [key-state todo-open]
   reads as -1).  A collapse to a number box is only honest when the
   magnitude is long enough that the list reading is implausible. *)
let min_int_mag_bits = 3

let decode_int_box t =
  match t with
  | Fork (sign, mag) -> (
      match bits_of_list mag with
      | Some bs when List.length bs >= min_int_mag_bits -> (
          match sign with
          | Leaf -> Some (int_of_bits bs)
          | Stem Leaf -> Some (- (int_of_bits bs))
          | _ -> None)
      | _ -> None)
  | _ -> None

let printable_char c = c >= ' ' && c <= '~'

let decode_str t =
  match Tuna.Cstr.decode t with
  | Some s when s <> "" && String.for_all printable_char s -> Some s
  | _ -> None

let decode_bool t =
  match t with Leaf -> Some false | Stem Leaf -> Some true | _ -> None

(* The collapse decision for a subtree: Some label = render as a box. *)
let box_label t =
  match decode_int_box t with
  | Some v -> Some (string_of_int v)
  | None -> (
      match decode_str t with
      | Some s -> Some (Printf.sprintf "\"%s\"" s)
      | None -> None)


(* ---------- measure + emit ---------- *)

exception Too_big

let label_width label = max 3 (String.length label + 2)

(* width in slots; raises Too_big past [max_nodes] *)
let rec measure ~budget t =
  if !budget <= 0 then raise Too_big;
  match box_label t with
  | Some label ->
      budget := !budget - Tuna.Tree.size t;
      label_width label
  | None -> (
      budget := !budget - 1;
      match t with
      | Leaf -> 1
      | Stem a -> max 1 (measure ~budget a)
      | Fork (a, b) -> measure ~budget a + measure ~budget b)

let esc_xml s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let truncate_label s n =
  if String.length s <= n then s else String.sub s 0 n ^ "…"

let truncate_str_prefixed s n =
  if String.length s <= n then "\"" ^ s ^ "\""
  else "\"" ^ String.sub s 0 n ^ "…\""

(* emit shapes+edges into buffers; x0 = left slot, returns subtree depth *)
let rec emit ~x0 ~depth ~w t glyphs edges =
  let cx = (float_of_int (x0 + (w / 2))) *. float_of_int slot_w in
  let cy = float_of_int depth *. float_of_int level_h in
  let cy = cy +. float_of_int node_r +. 8. in
  match box_label t with
  | Some label ->
      let label = truncate_label label 28 in
      let bw = float_of_int w *. float_of_int slot_w in
      Buffer.add_string glyphs
        (Printf.sprintf
           {|<rect x="%.1f" y="%.1f" width="%.1f" height="34" rx="9" class="tsv-box"/><text x="%.1f" y="%.1f" class="tsv-label">%s</text>|}
           (cx -. bw /. 2.) (cy -. 17.) bw cx (cy +. 5.) (esc_xml label));
      depth
  | None -> (
      match t with
      | Leaf ->
          Buffer.add_string glyphs
            (Printf.sprintf
               {|<rect x="%.1f" y="%.1f" width="13" height="13" transform="rotate(45 %.1f %.1f)" class="tsv-leaf"/>|}
               (cx -. 6.5) (cy -. 6.5) cx cy);
          depth
      | Stem a ->
          Buffer.add_string glyphs
            (Printf.sprintf
               {|<circle cx="%.1f" cy="%.1f" r="%d" class="tsv-stem"/><path d="M %.1f %.1f A %d %d 0 0 0 %.1f %.1f Z" class="tsv-half"/>|}
               cx cy node_r (cx -. float_of_int node_r) cy node_r node_r cx (cy +. float_of_int node_r));
          let cw = measure_unsafe a in
          Buffer.add_string edges
            (Printf.sprintf {|<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" class="tsv-e"/>|}
               cx (cy +. float_of_int node_r)
               (float_of_int ((x0 + (cw / 2)) * slot_w))
               (float_of_int ((depth + 1) * level_h + node_r + 8)));
          let d = emit ~x0 ~depth:(depth + 1) ~w:cw a glyphs edges in
          max depth d
      | Fork (a, b) ->
          Buffer.add_string glyphs
            (Printf.sprintf {|<circle cx="%.1f" cy="%.1f" r="%d" class="tsv-fork"/>|}
               cx cy node_r);
          let wa = measure_unsafe a and wb = measure_unsafe b in
          let edge x =
            Buffer.add_string edges
              (Printf.sprintf {|<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" class="tsv-e"/>|}
                 cx (cy +. float_of_int node_r)
                 (float_of_int (x * slot_w))
                 (float_of_int ((depth + 1) * level_h + node_r + 8)))
          in
          edge (x0 + (wa / 2));
          edge (x0 + wa + (wb / 2));
          let d1 = emit ~x0 ~depth:(depth + 1) ~w:wa a glyphs edges in
          let d2 = emit ~x0:(x0 + wa) ~depth:(depth + 1) ~w:wb b glyphs edges in
          max depth (max d1 d2))

(* measure without the budget (already bounded by the caller's pass) *)
and measure_unsafe t = measure ~budget:(ref 1_000_000) t

(* self-contained styling: the svg carries its own classes so exports
   (rsvg, screenshots) render identically to the page *)
let tsv_style =
  {|.tsv-e{stroke:#4a5560;stroke-width:1.6}
.tsv-leaf{fill:#161616;stroke:#9ccfd8;stroke-width:1.4}
.tsv-stem{fill:#161616;stroke:#9ccfd8;stroke-width:1.4}
.tsv-half{fill:#9ccfd8}
.tsv-fork{fill:#9ccfd8}
.tsv-box{fill:#1d2a33;stroke:#587d8c;stroke-width:1.2}
.tsv-label{fill:#d8dee9;font:13px monospace;text-anchor:middle}
svg.tsv{background:#161616}|}

let legend =
  {|<p class="muted tsv-legend">&#9671; leaf &middot; &#9684; stem &middot; &#9679; fork &middot; &#9633; decoded string / law-5 int</p>|}

(* The SVG for a tree; capped so a pathological value cannot flood the
   page.  Callers embed it directly (it is one <svg> element). *)
let svg ?(max_nodes = 4000) (t : Tuna.Tree.t) : string =
  let budget = ref max_nodes in
  let w =
    try measure ~budget t with Too_big -> -1
  in
  if w < 0 then
    Printf.sprintf
      {|<p class="muted">tree too large to draw (%d nodes, %d ternary chars) — read the ternary below or decode with <code>scripts/tern.py</code>.</p>|}
      (Tuna.Tree.size t) (String.length (Tuna.Canon.encode t))
  else
    let glyphs = Buffer.create 4096 and edges = Buffer.create 4096 in
    let depth = emit ~x0:0 ~depth:0 ~w t glyphs edges in
    let vw = float_of_int (w * slot_w + 20) in
    let vh = float_of_int ((depth + 1) * level_h + 24) in
    Printf.sprintf
      {|<svg class="tsv" viewBox="0 0 %.0f %.0f" width="%.0f" height="%.0f" xmlns="http://www.w3.org/2000/svg"><style>%s</style>%s%s</svg>|}
      vw vh vw vh tsv_style (Buffer.contents edges)
      (Buffer.contents glyphs)

(* One-line best-effort identification for badges and run rows: the
   same decode order as the drawing. *)
let summary (t : Tuna.Tree.t) : string =
  match decode_int_box t with
  | Some v -> Printf.sprintf "law-5 int %d" v
  | None -> (
      match decode_str t with
      | Some s -> Printf.sprintf "string %s" (truncate_str_prefixed s 32)
      | None -> (
          match decode_bool t with
          | Some b -> if b then "bool true" else "bool false"
          | None ->
              Printf.sprintf "tree · %d nodes · depth %d" (Tuna.Tree.size t)
                (Tuna.Tree.height t)))

let summary_of_ternary (ternary : string) : string =
  match Tuna.Canon.of_string ternary with
  | Ok t -> summary t
  | Error _ -> "unparseable"
