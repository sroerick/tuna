(** Base-27 ("Trihex") canonical text encoding for tree calculus trees
    (3 trits/char, alphabet [0-9a-q]).

    Pre-order Polish trits (0 = Leaf, 1 = Stem, 2 = Fork) are packed 3 per
    character in big-endian order: [d = t0*9 + t1*3 + t2] in [0..26], mapped
    through ["0123456789abcdefghijklmnopq"]. The final partial character is
    zero-padded; rejecting characters outside [0-9a-q], extra trailing chars,
    and non-zero padding trits yields a headerless 1-to-1 bijection. *)

exception Parse_error of int * string

val alphabet : string
(** The 27-character lowercase Trihex alphabet: ["0123456789abcdefghijklmnopq"]. *)

val encode : Tree.t -> string
(** Encode a tree to canonical lowercase Base-27 Trihex (without ["0t"] prefix). *)

val decode : string -> (Tree.t, int * string) result
(** Strictly decode a lowercase Base-27 Trihex string (without ["0t"] prefix). *)

val decode_exn : string -> Tree.t
(** Strictly decode a Trihex string; raises [Parse_error] on invalid input. *)

val of_trits : string -> (string, int * string) result
val to_trits : string -> (string, int * string) result
val of_tpack : string -> (string, int * string) result
val to_tpack : string -> (string, int * string) result
