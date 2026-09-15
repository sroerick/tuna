type t = Leaf | Stem of t | Fork of t * t

val size : t -> int
(** Number of nodes in the tree (Leaves count 1). *)

val height : t -> int
(** Height of the tree; the empty tree (Leaf) has height 0. *)
