(** Base-243 binary canonical encoding for tree calculus trees (5 trits/byte).

    Pre-order Polish trits (0 = Leaf, 1 = Stem, 2 = Fork) are packed 5 per byte
    in big-endian order: [b = t0*81 + t1*27 + t2*9 + t3*3 + t4] in [0..242].
    The final partial byte is zero-padded; because complete Polish trees are
    prefix-free and self-delimiting, rejecting bytes >= 243, extra trailing
    bytes, and non-zero padding trits yields a headerless 1-to-1 bijection
    between [Tree.t] and canonical Base-243 byte strings. *)

exception Parse_error of int * string

val encode : Tree.t -> string
(** Encode a tree to canonical Base-243 bytes (5 trits/byte, stack-safe). *)

val decode : string -> (Tree.t, int * string) result
(** Strictly decode Base-243 bytes into a tree. *)

val decode_exn : string -> Tree.t
(** Strictly decode Base-243 bytes; raises [Parse_error] on invalid input. *)

val of_trits : string -> (string, int * string) result
(** Validate a complete canonical ['0'|'1'|'2'] trit string and pack directly
    into Base-243 bytes. *)

val of_trits_exn : string -> string
(** Like [of_trits], raising [Parse_error] on invalid trit strings. *)

val to_trits : string -> (string, int * string) result
(** Strictly unpack canonical Base-243 bytes to a ['0'|'1'|'2'] trit string. *)
