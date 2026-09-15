val of_tree : Tree.t -> Digestif.SHA256.t
val hex_of_tree : Tree.t -> string
(** sha256 of the canonical ternary string, lowercase hex. *)

val hex_of_string : string -> string
val normalize_hex : string -> string
