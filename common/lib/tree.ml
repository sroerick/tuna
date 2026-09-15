(* Tuna common: tree type + size helpers.

   Normative: reference/tree-calculus/implementation/ocaml/lib/tree.ml.

   TC   | OCaml
   -----+------------
   △    | Leaf
   △ a  | Stem a
   △ a b| Fork (a, b) *)

type t = Leaf | Stem of t | Fork of t * t

(** Number of nodes in the tree (Leaves count 1). *)
let rec size = function
  | Leaf -> 1
  | Stem a -> 1 + size a
  | Fork (a, b) -> 1 + size a + size b

(** Height of the tree; the empty tree (Leaf) has height 0. *)
let rec height = function
  | Leaf -> 0
  | Stem a -> 1 + height a
  | Fork (a, b) -> 1 + max (height a) (height b)
