(* Hash of a tree = sha256 of its canonical ternary string,
   lowercase hex (AGENTS.md rule 2). *)

let of_tree tree = Canon.encode tree |> Digestif.SHA256.digest_string

let hex_of_tree tree =
  let d = of_tree tree in
  String.lowercase_ascii (Digestif.SHA256.to_hex d)

let hex_of_string s =
  String.lowercase_ascii (Digestif.SHA256.to_hex (Digestif.SHA256.digest_string s))

(** Normalize any hex input to lowercase (hashes are compared as
    lowercase hex everywhere). *)
let normalize_hex h = String.lowercase_ascii h
