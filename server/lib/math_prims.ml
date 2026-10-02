(* math prim family (borg/math-prims.borg): the host mirror of the
   sabra stdlib law-5 int ops (sabralib-seeded int-add/int-sub/int-mul/
   int-cmp/int-neg are the REFERENCE SEMANTICS; tests/math_prim_tests.ml
   gates every differential vector against them).

   Representation (borg/stdlib.borg conventions law 4 + 5): an int is
   fork(sign-bool, LSB-first magnitude bit list), canonical; both zeros
   = fork false nil; cmp answers a stem-chain small nat (0/10/110).

   JUNK DISCIPLINE (the recorded split): every bit position that is
   not a canonical bool reads as 0 at parse, and only canonical forms
   are emitted.  The stdlib pins a junk-bit passthrough in low
   positions - the prim does NOT mirror it (the chapter's family
   stanza carries the split; the differential law ranges over the
   canonical domain, junk inputs are canonicalization probes).

   Host cost is O(bits) per call - the F6 class disappears into one
   journal row per call, exact at any width (no OCaml int anywhere:
   the run size_cap already bounds operand trees). *)

open Tuna.Tree

let bit_value = function Leaf -> 0 | Stem _ -> 1 | Fork _ -> 0 (* junk reads 0 *)

(* sign canonicalization = is-stem: leaf false, stem true, fork junk false *)
let sign_value = function Stem _ -> 1 | _ -> 0

(* mag spine: leaf = end; stem spine mirrors the stdlib read of a junk
   spine (list-fold's stem arm answers z, i.e. end-of-list). *)
let rec bits_of_mag acc = function
  | Leaf -> List.rev acc
  | Stem _ -> List.rev acc
  | Fork (h, tl) -> bits_of_mag (bit_value h :: acc) tl

(* drop high-order zeros from an LSB-first bit list *)
let rec canonical = function
  | [] -> []
  | 0 :: tl -> (match canonical tl with [] -> [] | r -> 0 :: r)
  | b :: tl -> b :: canonical tl

let int_of_tree t =
  match t with
  | Fork (sign, mag) ->
      let bits = canonical (bits_of_mag [] mag) in
      if bits = [] then (0, []) else (sign_value sign, bits)
  | _ -> (0, []) (* junk top-level (leaf/stem) -> +0, per int-canonical *)

let bit_tree b = if b = 1 then Stem Leaf else Leaf
let rec tree_of_bits = function [] -> Leaf | b :: tl -> Fork (bit_tree b, tree_of_bits tl)

let tree_of_int (s, bits) =
  match bits with
  | [] -> Fork (Leaf, Leaf) (* both zeros one form *)
  | _ -> Fork (bit_tree s, tree_of_bits bits)

(* -- magnitude ops over LSB-first int lists (0/1) -------------------- *)

let rec mag_ripple carry = function
  | [] -> if carry = 1 then [ 1 ] else []
  | hd :: tl ->
      if carry = 0 then hd :: tl
      else if hd = 0 then 1 :: tl
      else 0 :: mag_ripple 1 tl

let rec mag_add carry a b =
  match (a, b) with
  | [], xs -> mag_ripple carry xs
  | xs, [] -> mag_ripple carry xs
  | x :: atl, y :: btl ->
      let s = x + y + carry in
      (s land 1) :: mag_add (s lsr 1) atl btl

(* three-way: -1/0/+1; assumes canonical (callsites wrap canonical) *)
let mag_cmp a b =
  let rec go x y =
    match (x, y) with
    | [], [] -> 0
    | _ :: atl, [] -> if List.exists (fun b -> b = 1) x then 1 else go atl []
    | [], _ :: btl -> if List.exists (fun b -> b = 1) y then -1 else go [] btl
    | x :: atl, y :: btl -> (
        match go atl btl with
        | 0 -> compare x y
        | r -> r)
  in
  go a b

(* caller guarantees a >= b *)
let rec mag_sub borrow a b =
  match (a, b) with
  | [], _ -> []
  | x :: atl, [] ->
      if borrow = 0 then x :: atl
      else if x = 1 then 0 :: mag_sub 0 atl b
      else 1 :: mag_sub 1 atl b
  | x :: atl, y :: btl ->
      let d = x - y - borrow in
      if d >= 0 then d :: mag_sub 0 atl btl else (d + 2) :: mag_sub 1 atl btl

let rec mag_mul a b acc =
  match b with
  | [] -> acc
  | x :: btl ->
      let acc = if x = 1 then mag_add 0 acc a else acc in
      mag_mul (0 :: a) btl acc

(* -- the prim surface ------------------------------------------------- *)

let neg_i (s, m) = match m with [] -> (0, []) | _ -> (1 - s, m)

let add_i (sa, a) (sb, b) =
  if sa = sb then (sa, canonical (mag_add 0 a b))
  else
    match mag_cmp (canonical a) (canonical b) with
    | 1 -> (sa, canonical (mag_sub 0 a b))
    | -1 -> (sb, canonical (mag_sub 0 b a))
    | _ -> (0, [])

let sub_i a b = add_i a (neg_i b)

let mul_i (sa, a) (sb, b) =
  let m = canonical (mag_mul a b []) in
  match m with [] -> (0, []) | _ -> (sa lxor sb, m)

let cmp_i (sa, am) (sb, bm) =
  match (sa, sb) with
  | 0, 0 -> mag_cmp (canonical am) (canonical bm)
  | 1, 0 -> -1
  | 0, 1 -> 1
  | _ -> -mag_cmp (canonical am) (canonical bm)

let to_small_nat c = if c < 0 then Leaf else if c = 0 then Stem Leaf else Stem (Stem Leaf)

(* the prim surface: int trees in, int/small-nat tree out (arg-LIST
   shape is enforced by the Prims dispatch arms, which answer shape
   errors as journaled `Error's, never exceptions) *)
let add a b = tree_of_int (add_i (int_of_tree a) (int_of_tree b))
let sub a b = tree_of_int (sub_i (int_of_tree a) (int_of_tree b))
let mul a b = tree_of_int (mul_i (int_of_tree a) (int_of_tree b))
let neg a = tree_of_int (neg_i (int_of_tree a))
let cmp a b = to_small_nat (cmp_i (int_of_tree a) (int_of_tree b))
