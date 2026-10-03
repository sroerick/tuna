(* Int_enc: decimal literals -> the canonical law-5 int tree
   (borg/dialect.borg; borg/stdlib.borg conventions law 5).

   An int is fork(sign-bool, magnitude) where the magnitude is an
   LSB-first list of bool bits, high-order false bits stripped; both
   zeros are the ONE form fork false nil (the same tree as int-zero and
   as the 2-list [false,false]).  Sign false = Leaf (positive), true =
   Stem Leaf (negative).

   This module is the single shared codec the reader uses for `42` /
   `-7` literals (the seeded stdlib executes no host code, so the
   encoding has to live as code beside Tuna.Cstr).  It is deliberately
   arbitrary precision: decimal digits are divided down by two, no
   OCaml int is ever asked to hold the value, so a literal of any width
   canonicalizes exactly. *)

open Tree

(* bool tree: false = Leaf, true = Stem Leaf (stdlib law 1) *)
let bool_tree b = if b then Stem Leaf else Leaf

(* magnitude bits (LSB first) -> tree; [] = Leaf (nil) *)
let rec tree_of_bits = function
  | [] -> Leaf
  | b :: tl -> Fork (bool_tree b, tree_of_bits tl)

(* canonical positive int tree from LSB-first bits *)
let tree_of_mag bits =
  (* strip high-order false bits already; [] -> the canonical zero *)
  match bits with [] -> Fork (Leaf, Leaf) | bs -> Fork (Leaf, tree_of_bits bs)

(* negative int tree: sign true; a zero magnitude collapses to the one
   canonical zero (int-neg never mints -0, stdlib law 5) *)
let tree_of_neg_mag bits =
  match bits with [] -> Fork (Leaf, Leaf) | bs -> Fork (Stem Leaf, tree_of_bits bs)

(* -- decimal string -> magnitude bits (LSB first) -------------------- *)

let rec drop_leading_zeros = function
  | 0 :: tl -> drop_leading_zeros tl
  | l -> l

(* repeated long division of a decimal digit list by 2; remainder is
   the next low bit.  Returns bits LSB-first, high zeros stripped. *)
let bits_of_decimal (digits : int list) : bool list =
  let div2 (ds : int list) : (int list * bool) =
    (* long division left-to-right; carry holds the running remainder *)
    let rec go carry acc = function
      | [] -> (List.rev acc, carry mod 2 = 1)
      | d :: rest ->
          let cur = (carry * 10) + d in
          let q = cur / 2 in
          let r = cur mod 2 in
          (* drop leading zero quotients *)
          let acc = if acc = [] && q = 0 then [] else q :: acc in
          go r acc rest
    in
    go 0 [] ds
  in
  let rec loop ds acc =
    if ds = [] then List.rev acc
    else
      let q, bit = div2 ds in
      loop q (bit :: acc)
  in
  (* strip high-order false bits (they land last in the reversed acc) *)
  let rec strip = function
    | [] -> []
    | false :: tl -> strip tl
    | l -> l
  in
  List.rev (strip (List.rev (loop digits [])))

let digits_of_string s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c = s.[i] in
      if c >= '0' && c <= '9' then go (i + 1) ((Char.code c - 48) :: acc)
      else invalid_arg "Int_enc: not a decimal literal"
  in
  go 0 []

(* decimal string (without sign) -> canonical positive int tree; an
   all-zero / empty digit string is the canonical zero.  Leading zeros
   are ignored. *)
let positive_of_decimal (s : string) : t =
  let digits = digits_of_string s in
  (* drop leading zeros so bits_of_decimal strips cleanly anyway; the
     division handles them, this just keeps all-zero == zero *)
  let digits = drop_leading_zeros digits in
  tree_of_mag (bits_of_decimal digits)

let negative_of_decimal (s : string) : t =
  let digits = drop_leading_zeros (digits_of_string s) in
  tree_of_neg_mag (bits_of_decimal digits)

(* Parse a surface number atom: optional leading '-' then digits.
   [None] when the atom is not a plain decimal literal (the caller
   keeps its existing handling).  A bare "0" is intentionally NOT
   claimed here: that atom is the long-standing LEAF literal, and the
   reader preserves it (borg/dialect.borg L3 note). *)
let of_decimal_atom (a : string) : t option =
  let n = String.length a in
  if n = 0 then None
  else
    let neg, body =
      if n > 0 && a.[0] = '-' then (true, String.sub a 1 (n - 1))
      else (false, a)
    in
    if String.length body = 0 then None
    else if not (String.for_all (fun c -> c >= '0' && c <= '9') body) then None
    else if (not neg) && body = "0" then None (* bare 0 stays Leaf *)
    else if neg then Some (negative_of_decimal body)
    else Some (positive_of_decimal body)
