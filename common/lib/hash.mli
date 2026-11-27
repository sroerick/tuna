val of_tree : Tree.t -> Digestif.SHA256.t
val hex_of_tree : Tree.t -> string
(** sha256 of the canonical Base-243 ({!Tpack}) tree bytes, lowercase hex. *)

val hex_of_tpack : string -> string
(** sha256 of canonical Base-243 tree bytes, lowercase hex. *)

val hex_of_trits : string -> string
(** sha256 of the Base-243 packing of a valid ['0'|'1'|'2'] tree trit string,
    lowercase hex. Raises {!Tpack.Parse_error} if the string is not a valid
    complete tree. *)

val hex_of_string : string -> string
(** Raw sha256 of arbitrary bytes/string, lowercase hex. *)

val normalize_hex : string -> string
