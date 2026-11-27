(* Base-27 ("Trihex") canonical text encoding (3 trits/char, 0-9a-q). *)

exception Parse_error of int * string

let alphabet = "0123456789abcdefghijklmnopq"

let digit_of_char c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'q' then 10 + (Char.code c - Char.code 'a')
  else -1

let encode (tree : Tree.t) : string =
  let buf = Buffer.create 64 in
  let acc = ref 0 in
  let count = ref 0 in
  let push_trit t =
    acc := (!acc * 3) + t;
    incr count;
    if !count = 3 then begin
      Buffer.add_char buf alphabet.[!acc];
      acc := 0;
      count := 0
    end
  in
  let rec loop = function
    | [] -> ()
    | Tree.Leaf :: rest ->
        push_trit 0;
        loop rest
    | Tree.Stem a :: rest ->
        push_trit 1;
        loop (a :: rest)
    | Tree.Fork (a, b) :: rest ->
        push_trit 2;
        loop (a :: b :: rest)
  in
  loop [ tree ];
  if !count > 0 then begin
    while !count < 3 do
      acc := !acc * 3;
      incr count
    done;
    Buffer.add_char buf alphabet.[!acc]
  end;
  Buffer.contents buf

type frame =
  | F_stem
  | F_fork_left
  | F_fork_right of Tree.t

let decode_exn (s : string) : Tree.t =
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
  let feed_trit idx trit =
    match !done_tree with
    | Some _ ->
        if trit <> 0 then
          raise (Parse_error (idx, "non-zero padding trit in final trihex digit"))
    | None -> (
        match trit with
        | 0 -> finish Tree.Leaf
        | 1 -> frames := F_stem :: !frames
        | 2 -> frames := F_fork_left :: !frames
        | _ -> assert false)
  in
  for i = 0 to len - 1 do
    (match !done_tree with
     | Some _ -> raise (Parse_error (i, "trailing characters after tree"))
     | None -> ());
    let d = digit_of_char s.[i] in
    if d < 0 then
      raise (Parse_error (i, Printf.sprintf "unexpected trihex character %C" s.[i]));
    let t0 = d / 9 in
    let r0 = d - (t0 * 9) in
    let t1 = r0 / 3 in
    let t2 = r0 - (t1 * 3) in
    feed_trit i t0;
    feed_trit i t1;
    feed_trit i t2
  done;
  match !done_tree with
  | Some t -> t
  | None -> raise (Parse_error (len, "unexpected end of input"))

let decode (s : string) : (Tree.t, int * string) result =
  try Ok (decode_exn s) with Parse_error (off, msg) -> Error (off, msg)

let of_trits (s : string) : (string, int * string) result =
  try
    let len = String.length s in
    if len = 0 then raise (Parse_error (0, "unexpected end of input"));
    let buf = Buffer.create ((len + 2) / 3) in
    let slots = ref 1 in
    let acc = ref 0 in
    let count = ref 0 in
    for i = 0 to len - 1 do
      if !slots = 0 then
        raise (Parse_error (i, "trailing characters after tree"));
      let trit =
        match s.[i] with
        | '0' ->
            decr slots;
            0
        | '1' -> 1
        | '2' ->
            incr slots;
            2
        | c -> raise (Parse_error (i, Printf.sprintf "unexpected character %C" c))
      in
      acc := (!acc * 3) + trit;
      incr count;
      if !count = 3 then begin
        Buffer.add_char buf alphabet.[!acc];
        acc := 0;
        count := 0
      end
    done;
    if !slots <> 0 then raise (Parse_error (len, "unexpected end of input"));
    if !count > 0 then begin
      while !count < 3 do
        acc := !acc * 3;
        incr count
      done;
      Buffer.add_char buf alphabet.[!acc]
    end;
    Ok (Buffer.contents buf)
  with Parse_error (off, msg) -> Error (off, msg)

let to_trits (s : string) : (string, int * string) result =
  try
    let len = String.length s in
    if len = 0 then raise (Parse_error (0, "unexpected end of input"));
    let buf = Buffer.create (len * 3) in
    let slots = ref 1 in
    let feed idx trit =
      if !slots = 0 then begin
        if trit <> 0 then
          raise (Parse_error (idx, "non-zero padding trit in final trihex digit"))
      end else
        match trit with
        | 0 ->
            Buffer.add_char buf '0';
            decr slots
        | 1 -> Buffer.add_char buf '1'
        | 2 ->
            Buffer.add_char buf '2';
            incr slots
        | _ -> assert false
    in
    for i = 0 to len - 1 do
      if !slots = 0 then raise (Parse_error (i, "trailing characters after tree"));
      let d = digit_of_char s.[i] in
      if d < 0 then
        raise (Parse_error (i, Printf.sprintf "unexpected trihex character %C" s.[i]));
      let t0 = d / 9 in
      let r0 = d - (t0 * 9) in
      let t1 = r0 / 3 in
      let t2 = r0 - (t1 * 3) in
      feed i t0;
      feed i t1;
      feed i t2
    done;
    if !slots <> 0 then raise (Parse_error (len, "unexpected end of input"));
    Ok (Buffer.contents buf)
  with Parse_error (off, msg) -> Error (off, msg)

let of_tpack (bytes : string) : (string, int * string) result =
  match Tpack.to_trits bytes with
  | Ok trits -> of_trits trits
  | Error _ as e -> e

let to_tpack (s : string) : (string, int * string) result =
  match to_trits s with
  | Ok trits -> Tpack.of_trits trits
  | Error _ as e -> e
