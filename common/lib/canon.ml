(* Canonical ternary serialization: 0 = leaf, 1+child = stem,
   2+left+right = fork. Same encoding as upstream (see
   reference/tree-calculus/implementation/python/tree-calculus.py,
   parse_ternary / format_ternary). Iterative linear-time Buffer encoder
   and stack-safe decoder; Base-243 (Tpack) and Base-27 (Trihex) re-exported
   for compact binary storage and text surfaces. *)

let encode (tree : Tree.t) : string =
  let buf = Buffer.create 64 in
  let rec loop = function
    | [] -> ()
    | Tree.Leaf :: rest ->
        Buffer.add_char buf '0';
        loop rest
    | Tree.Stem a :: rest ->
        Buffer.add_char buf '1';
        loop (a :: rest)
    | Tree.Fork (a, b) :: rest ->
        Buffer.add_char buf '2';
        loop (a :: b :: rest)
  in
  loop [ tree ];
  Buffer.contents buf

let to_trits = encode

exception Parse_error of int * string
(** [Parse_error (offset, message)] — offset is the 0-based index into
    the input string where the problem was detected. *)

type frame =
  | F_stem
  | F_fork_left
  | F_fork_right of Tree.t

let parse s =
  let len = String.length s in
  if len = 0 then raise (Parse_error (0, "unexpected end of input"));
  let frames = ref [] in
  let done_tree = ref None in
  let rec finish t =
    match !frames with
    | [] -> done_tree := Some t
    | F_stem :: rest ->
        frames := rest;
        finish (Tree.Stem t)
    | F_fork_left :: rest -> frames := F_fork_right t :: rest
    | F_fork_right left :: rest ->
        frames := rest;
        finish (Tree.Fork (left, t))
  in
  for i = 0 to len - 1 do
    (match !done_tree with
     | Some _ -> raise (Parse_error (i, "trailing characters after tree"))
     | None -> ());
    match s.[i] with
    | '0' -> finish Tree.Leaf
    | '1' -> frames := F_stem :: !frames
    | '2' -> frames := F_fork_left :: !frames
    | c -> raise (Parse_error (i, Printf.sprintf "unexpected character %C" c))
  done;
  match !done_tree with
  | Some t -> t
  | None -> raise (Parse_error (len, "unexpected end of input"))

let parse_exn s = parse s

(** Strict parse returning [Result]. *)
let of_string s = try Ok (parse s) with Parse_error (off, msg) -> Error (off, msg)

let of_trits = of_string
let to_string = encode

let encode_bytes = Tpack.encode
let decode_bytes = Tpack.decode
let encode_trihex = Trihex.encode
let decode_trihex = Trihex.decode

let parse_any s =
  if String.length s >= 2 && s.[0] = '0' && s.[1] = 't' then
    match Trihex.decode (String.sub s 2 (String.length s - 2)) with
    | Ok _ as ok -> ok
    | Error (off, msg) -> Error (off + 2, msg)
  else of_string s
