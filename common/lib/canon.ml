(* Canonical ternary serialization: 0 = leaf, 1+child = stem,
   2+left+right = fork. Same encoding as upstream (see
   reference/tree-calculus/implementation/python/tree-calculus.py,
   parse_ternary / format_ternary). This is the canonical form: every
   tree has exactly one string and every valid string one tree. *)

let rec encode = function
  | Tree.Leaf -> "0"
  | Tree.Stem a -> "1" ^ encode a
  | Tree.Fork (a, b) -> "2" ^ encode a ^ encode b

exception Parse_error of int * string
(** [Parse_error (offset, message)] — offset is the 0-based index into
    the input string where the problem was detected. *)

let parse s =
  let len = String.length s in
  let rec go i =
    if i >= len then raise (Parse_error (i, "unexpected end of input"))
    else
      match s.[i] with
      | '0' -> (Tree.Leaf, i + 1)
      | '1' ->
          let child, i' = go (i + 1) in
          (Tree.Stem child, i')
      | '2' ->
          let left, i' = go (i + 1) in
          let right, i'' = go i' in
          (Tree.Fork (left, right), i'')
      | c -> raise (Parse_error (i, Printf.sprintf "unexpected character %C" c))
  in
  let tree, i = go 0 in
  if i <> len then raise (Parse_error (i, "trailing characters after tree"));
  tree

let parse_exn s = parse s

(** Strict parse returning [Result]. *)
let of_string s = try Ok (parse s) with Parse_error (off, msg) -> Error (off, msg)

let to_string = encode
