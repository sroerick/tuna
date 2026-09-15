open Tree

val gate : t
(** The fixed prim marker tree (ternary "11110"). *)

val gate_ternary : string

val call_tree : name:string -> site:int -> t
(** The tree whose application to an args tree is a prim call:
    Fork (gate, Fork (Cstr.encode name, Cstr.unary site)). *)

val shape : t -> (string * int) option
(** The (name, site) a tree denotes as a prim call, if any. *)
