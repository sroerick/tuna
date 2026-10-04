(* Tuna_server.Dec: best-effort, LABELED decode views for ternary.

   The observability law (the v0.2-defect disposition + F4's bar):
   ternary is the wire format, never the reading interface.  These
   views render a tree the way the house codecs would read it — bool,
   small nat, law-5 int, cstr string, list of elements — every view a
   GUESS, all views beside each other, the ternary itself always
   authoritative and carried alongside.

   AMBIGUITY IS THE DESIGN (borg/stdlib.borg conventions, the
   one-form law): Fork(Leaf,Leaf) is simultaneously int-zero, the
   2-list [false false], and a pair of leaves, so no view can claim to
   BE the meaning; a decoder that picked one would manufacture
   findings.  Consumers pin semantics with a REPL twin or a corpus row
   (scripts/tern.py's rule, mirrored here); this module only puts the
   candidates on one line.

   Bounds: trees above [max_ternary] chars are never parsed (size
   only); lists render at most [max_elements] elements with an
   ellipsis; recursion is depth-bounded.  A malformed ternary yields
   [None], never an exception — a display aid must not be able to
   fail a run fetch. *)

let max_ternary = 4096
let max_elements = 8
let max_depth = 2

let parse (ternary : string) : Tuna.Tree.t option =
  if String.length ternary = 0 || String.length ternary > max_ternary then None
  else
    match Tuna.Canon.of_string ternary with
    | Ok t -> Some t
    | Error _ -> None

(* law-5 int: Fork(sign, magnitude); magnitude = LSB-first bit list,
   every bit canonical (Leaf | Stem Leaf); junk bits disqualify the
   view rather than silently reading 0 (math_prims' parse rule is for
   the PRIM, not for a human-facing guess). *)
let int_view (t : Tuna.Tree.t) : string option =
  match t with
  | Tuna.Tree.Fork (sign, mag) -> (
      let rec bits acc (m : Tuna.Tree.t) =
        match m with
        | Tuna.Tree.Leaf -> Some (List.rev acc)
        | Tuna.Tree.Fork (b, rest) -> (
            match b with
            | Tuna.Tree.Leaf -> bits (0 :: acc) rest
            | Tuna.Tree.Stem Tuna.Tree.Leaf -> bits (1 :: acc) rest
            | _ -> None)
        | _ -> None
      in
      match (sign, bits [] mag) with
      | Tuna.Tree.Leaf, Some [] -> Some "+0" (* both zeros one form *)
      | (Tuna.Tree.Leaf | Tuna.Tree.Stem Tuna.Tree.Leaf), Some bits -> (
          let v =
            List.fold_left (fun acc (b, i) -> acc + (b lsl i)) 0
              (List.mapi (fun i b -> (b, i)) bits)
          in
          Some
            (if (match sign with Tuna.Tree.Stem _ -> true | _ -> false) then
               Printf.sprintf "-%d" v
             else Printf.sprintf "+%d" v))
      | _ -> None)
  | _ -> None

let bool_view = function
  | Tuna.Tree.Leaf -> Some "false"
  | Tuna.Tree.Stem Tuna.Tree.Leaf -> Some "true"
  | _ -> None

let nat_view t =
  match Tuna.Cstr.unary_of t with Some n -> Some (string_of_int n) | None -> None

let str_view t = Tuna.Cstr.decode t

(* A compact one-line view for a table cell: the first candidate that
   reads cleanly, labeled.  Falls back to a truncated ternary. *)
let rec hint ?(depth = 0) (t : Tuna.Tree.t) : string =
  let short s =
    if String.length s > 48 then String.sub s 0 45 ^ "..." else s
  in
  match (bool_view t, nat_view t, int_view t, str_view t) with
  | Some b, _, _, _ -> b
  | _, Some n, _, _ -> Printf.sprintf "nat %s" n
  | _, _, Some i, _ -> i
  | _, _, _, Some s -> Printf.sprintf "%S" s
  | _ -> (
      match t with
      | Tuna.Tree.Fork (_, tl) when depth < max_depth -> (
          (* list guess: walk the spine; give up on non-list tails *)
          let rec els acc n (tl : Tuna.Tree.t) =
            match (n >= max_elements, tl) with
            | true, _ -> Some (List.rev acc, true)
            | false, Tuna.Tree.Leaf -> Some (List.rev acc, false)
            | false, Tuna.Tree.Fork (h, rest) ->
                els (h :: acc) (n + 1) rest
            | false, _ -> None
          in
          match els [] 0 tl with
          | Some (els, truncated) ->
              let items =
                List.map (fun e -> hint ~depth:(depth + 1) e) els
              in
              let body = String.concat ", " items in
              short
                (Printf.sprintf "[%s%s]" body
                   (if truncated then ", ..." else ""))
          | None -> short (Tuna.Canon.encode t))
      | _ -> short (Tuna.Canon.encode t))

(* hint for a raw ternary string (the shape journal rows carry):
   None when the ternary does not parse or is over the cap. *)
let hint_of_ternary (ternary : string) : string option =
  match parse ternary with Some t -> Some (hint t) | None -> None

(* The full labeled view: every candidate interpretation, JSON.  The
   ternary itself is NOT included — callers carry it beside the view. *)
let rec views (ternary : string) : Yojson.Basic.t option =
  match parse ternary with
  | None -> None
  | Some t ->
      let field name = function
        | Some v -> Some (name, `String v)
        | None -> None
      in
      let fields =
        List.filter_map Fun.id
          [ field "bool" (bool_view t)
          ; field "nat" (nat_view t)
          ; field "int" (int_view t)
          ; field "str" (str_view t) ]
      in
      let list_field =
        let rec els acc n (tl : Tuna.Tree.t) =
          match (n >= max_elements, tl) with
          | true, _ -> Some (List.rev acc, true)
          | false, Tuna.Tree.Leaf -> Some (List.rev acc, false)
          | false, Tuna.Tree.Fork (h, rest) -> els (h :: acc) (n + 1) rest
          | false, _ -> None
        in
        match t with
        | Tuna.Tree.Fork (_, _) -> (
            match els [] 0 t with
            | Some (els, truncated) ->
                Some
                  ( "list"
                  , `List
                      (List.map
                         (fun e ->
                           match views (Tuna.Canon.encode e) with
                           | Some v -> v
                           | None -> `Null)
                         els
                       @ if truncated then [ `String "..." ] else []) )
            | None -> None)
        | _ -> None
      in
      Some
        (`Assoc
          (fields
          @ (match list_field with Some f -> [ f ] | None -> [])
          @ [ ("size", `Int (Tuna.Tree.size t)) ]))
