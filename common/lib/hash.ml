(* Hash of a tree = sha256 of its canonical Base-243 (Tpack) bytes,
   lowercase hex (AGENTS.md rule 2, 2026-11-27 amendment). Unifies tree
   addressing with Midden's universal sha256(exact_stored_bytes) law. *)

let of_tree tree = Tpack.encode tree |> Digestif.SHA256.digest_string

let hex_of_tree tree =
  let d = of_tree tree in
  String.lowercase_ascii (Digestif.SHA256.to_hex d)

let hex_of_string s =
  String.lowercase_ascii (Digestif.SHA256.to_hex (Digestif.SHA256.digest_string s))

let hex_of_tpack = hex_of_string

let hex_of_trits s = hex_of_string (Tpack.of_trits_exn s)

(** Normalize any hex input to lowercase (hashes are compared as
    lowercase hex everywhere). *)
let normalize_hex h = String.lowercase_ascii h
