exception Parse_error of int * string
(** [Parse_error (offset, message)] — offset is the 0-based index into
    the input string where the problem was detected. *)

val encode : Tree.t -> string
(** Canonical ternary string of a tree. *)

val parse : string -> Tree.t
(** Strict parse; raises [Parse_error] on bad character, early end of
    input, or trailing characters. *)

val parse_exn : string -> Tree.t
(** Alias for [parse]. *)

val of_string : string -> (Tree.t, int * string) result
(** Strict parse returning [Result.error (offset, message)]. *)

val to_string : Tree.t -> string
(** Alias for [encode]. *)
