(* Cstr: strings as trees — the BINARY convention of the Barry Jay
   tree-calculus reference (upstream tree-calculus/implementation/
   ocaml/lib/marshal.ml, tree_of_int / tree_of_char /
   tree_of_string), so tuna trees match the reference exactly.

     bool   false = Leaf          true = Stem Leaf
     int    little-endian bit LIST over bool (LSB first, no leading
            zero); nil = 0
     char   = the int of its code point
     string = LIST of chars

     nil = Leaf = 0 = empty string: one tree ends every list, which is
     exactly the book's encoding (tree_of_string "" = Leaf).  Decode is
     total — anything malformed (a Stem in list position, a bit that is
     not a bool, a char whose bit width exceeds a byte) decodes to
     None; the caller decides what that means.

   A byte costs O(#bits) nodes instead of the old v1 unary chain (a
   Stem^b Leaf up to 255 deep), so strings stay small enough to carry
   comfortably at the prim boundary.  Cstr.unary / unary_of are kept
   for the CALLSITE identity count (Cprim), which is a separate
   convention. *)

open Tree

(* Stem^n Leaf, n >= 0 (callsite site count; not string encoding) *)
let rec unary n = if n = 0 then Leaf else Stem (unary (n - 1))

(* number of stems wrapping a Leaf; None if the tree is not unary *)
let rec unary_of = function
  | Leaf -> Some 0
  | Stem t -> Option.map (fun n -> n + 1) (unary_of t)
  | Fork _ -> None

(* -- binary string codec (the book's marshal.ml convention) ---------- *)

let max_byte = 255

(* the char tree of a byte: little-endian bit list, LSB first *)
let rec char_tree v =
  if v = 0 then Leaf
  else Fork ((if v land 1 = 1 then Stem Leaf else Leaf), char_tree (v lsr 1))

let encode (s : string) : t =
  let rec go i acc =
    if i < 0 then acc else go (i - 1) (Fork (char_tree (Char.code s.[i]), acc))
  in
  go (String.length s - 1) Leaf

(* decode one char's bit list (LSB first) to its code point *)
let rec char_of_tree t =
  match t with
  | Leaf -> Some 0
  | Fork (Leaf, tail) -> ( match char_of_tree tail with Some v -> Some (2 * v) | None -> None )
  | Fork (Stem _, tail) -> ( match char_of_tree tail with Some v -> Some (1 + 2 * v) | None -> None )
  | _ -> None

let decode (t : t) : string option =
  let rec go t acc =
    match t with
    | Leaf -> Some (String.concat "" (List.rev_map (String.make 1) acc))
    | Fork (c, rest) -> (
        match char_of_tree c with
        | Some v when v <= max_byte -> go rest (Char.chr v :: acc)
        | _ -> None)
    | Stem _ -> None
  in
  go t []
