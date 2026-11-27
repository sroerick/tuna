exception Parse_error of int * string
(** [Parse_error (offset, message)] — offset is the 0-based index into
    the input string where the problem was detected. *)

val encode : Tree.t -> string
(** Canonical ternary trit string (['0'|'1'|'2']) of a tree. *)

val to_trits : Tree.t -> string
(** Alias for [encode] (upstream Polish ternary trit string). *)

val parse : string -> Tree.t
(** Strict parse of a ['0'|'1'|'2'] trit string; raises [Parse_error] on bad
    character, early end of input, or trailing characters. *)

val parse_exn : string -> Tree.t
(** Alias for [parse]. *)

val of_string : string -> (Tree.t, int * string) result
(** Strict parse of a ['0'|'1'|'2'] trit string returning [Result]. *)

val of_trits : string -> (Tree.t, int * string) result
(** Alias for [of_string]. *)

val to_string : Tree.t -> string
(** Alias for [encode]. *)

val encode_bytes : Tree.t -> string
(** Canonical Base-243 binary bytes (5 trits/byte, {!Tpack.encode}). *)

val decode_bytes : string -> (Tree.t, int * string) result
(** Strict decode of Base-243 binary bytes ({!Tpack.decode}). *)

val encode_trihex : Tree.t -> string
(** Canonical Base-27 Trihex string (3 trits/char, {!Trihex.encode}). *)

val decode_trihex : string -> (Tree.t, int * string) result
(** Strict decode of Base-27 Trihex string ({!Trihex.decode}). *)

val parse_any : string -> (Tree.t, int * string) result
(** Parse either ["0t<trihex>"] or raw ['0'|'1'|'2'] trits. *)
