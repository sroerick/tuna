(** Bidirectional schema-directed codec between Midden canonical s-expressions
    and Tuna dialect v1.1 trees (1-based small-nat tags over canonically
    byte-sorted schema fields, Law-1 bools, Law-2 lists, Law-3 options for
    :optional fields, Law-5 ints, Cstr strings/bytes). *)

val field_tag : Midden.Schema.schema -> string -> int option

val csexp_to_tree :
  Midden.Schema.schema -> Midden.Csexp.t -> (Tuna.Tree.t, string) result

val tree_to_csexp :
  Midden.Schema.schema -> Tuna.Tree.t -> (Midden.Csexp.t, string) result

val int64_to_tree : int64 -> Tuna.Tree.t
val int64_of_tree : Tuna.Tree.t -> int64 option
