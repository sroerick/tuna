(* Int_enc: decimal literals -> canonical law-5 int trees.
   See borg/dialect.borg (numbers) and borg/stdlib.borg conventions 5. *)

val positive_of_decimal : string -> Tree.t
(** [positive_of_decimal "42"] is the canonical positive int tree for
    42.  The string must contain digits (leading zeros tolerated). *)

val negative_of_decimal : string -> Tree.t
(** [negative_of_decimal "42"] is the canonical negative int tree for
    -42.  A zero magnitude collapses to the one canonical zero. *)

val of_decimal_atom : string -> Tree.t option
(** [of_decimal_atom a] decodes a surface number atom: an optional
    leading ['-'] then one or more digits.  Returns [None] for anything
    else — including the bare ["0"] atom, which the reader keeps as the
    long-standing Leaf literal. *)
