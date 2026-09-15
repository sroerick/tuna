open Tree

val unary : int -> t
(** [unary n] = Stem^n Leaf (n >= 0). *)

val unary_of : t -> int option
(** Number of stems wrapping a Leaf; None for non-unary trees. *)

val max_byte : int
(** Highest byte the encoding carries (255). *)

val encode : string -> t
(** String -> tree: nil = Leaf, cons b rest = Fork (Stem (unary b), rest). *)

val decode : t -> string option
(** Inverse of [encode]; None for any tree that is not a string tree. *)
