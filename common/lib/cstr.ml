(* Cstr: strings as trees — the v1 byte-list encoding used at the
   prim boundary (journal payloads, prim names, error messages).

   Convention: nil = Leaf; cons byte b rest = Fork (Stem (Stem^b Leaf),
   rest).  Byte values are unary trees, so a 255-byte string is a
   255-deep stem — fine for the short payloads v0 carries.  Decode is
   total: anything that is not a well-formed string tree decodes to
   None (the caller decides what that means). *)

open Tree

(* Stem^n Leaf, n >= 0 *)
let rec unary n = if n = 0 then Leaf else Stem (unary (n - 1))

(* number of stems wrapping a Leaf; None if the tree is not unary *)
let rec unary_of = function
  | Leaf -> Some 0
  | Stem t -> Option.map (fun n -> n + 1) (unary_of t)
  | Fork _ -> None

let max_byte = 255

let encode (s : string) : t =
  let rec go i acc =
    if i < 0 then acc
    else go (i - 1) (Fork (Stem (unary (Char.code s.[i])), acc))
  in
  go (String.length s - 1) Leaf

let decode (t : t) : string option =
  let rec go t acc =
    match t with
    | Leaf -> Some (String.concat "" (List.rev_map (String.make 1) acc))
    | Fork (Stem u, rest) -> (
        match unary_of u with
        | Some b when b <= max_byte -> go rest (Char.chr b :: acc)
        | _ -> None)
    | _ -> None
  in
  go t []
