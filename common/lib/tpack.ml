(* Base-243 binary canonical encoding (5 trits/byte, 0x00..0xF2).
   Iterative/stack-safe encoder and decoder for arbitrarily deep trees. *)

exception Parse_error of int * string

let encode (tree : Tree.t) : string =
  let buf = Buffer.create 64 in
  let acc = ref 0 in
  let count = ref 0 in
  let push_trit t =
    acc := (!acc * 3) + t;
    incr count;
    if !count = 5 then begin
      Buffer.add_char buf (Char.chr !acc);
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
    while !count < 5 do
      acc := !acc * 3;
      incr count
    done;
    Buffer.add_char buf (Char.chr !acc)
  end;
  Buffer.contents buf

type frame =
  | F_stem
  | F_fork_left
  | F_fork_right of Tree.t

let decode_exn (bytes : string) : Tree.t =
  let len = String.length bytes in
  if len = 0 then raise (Parse_error (0, "unexpected end of input"));
  let frames = ref [] in
  let done_tree = ref None in
  let rec finish t =
    match !frames with
    | [] -> done_tree := Some t
    | F_stem :: rest ->
        frames := rest;
        finish (Tree.Stem t)
    | F_fork_left :: rest ->
        frames := F_fork_right t :: rest
    | F_fork_right left :: rest ->
        frames := rest;
        finish (Tree.Fork (left, t))
  in
  let feed_trit byte_idx trit =
    match !done_tree with
    | Some _ ->
        if trit <> 0 then
          raise (Parse_error (byte_idx, "non-zero padding trit in final byte"))
    | None -> (
        match trit with
        | 0 -> finish Tree.Leaf
        | 1 -> frames := F_stem :: !frames
        | 2 -> frames := F_fork_left :: !frames
        | _ -> assert false)
  in
  for i = 0 to len - 1 do
    (match !done_tree with
     | Some _ -> raise (Parse_error (i, "trailing bytes after tree"))
     | None -> ());
    let b = Char.code bytes.[i] in
    if b >= 243 then
      raise (Parse_error (i, Printf.sprintf "invalid base-243 byte 0x%02x" b));
    let t0 = b / 81 in
    let r0 = b - (t0 * 81) in
    let t1 = r0 / 27 in
    let r1 = r0 - (t1 * 27) in
    let t2 = r1 / 9 in
    let r2 = r1 - (t2 * 9) in
    let t3 = r2 / 3 in
    let t4 = r2 - (t3 * 3) in
    feed_trit i t0;
    feed_trit i t1;
    feed_trit i t2;
    feed_trit i t3;
    feed_trit i t4
  done;
  match !done_tree with
  | Some t -> t
  | None -> raise (Parse_error (len, "unexpected end of input"))

let decode (bytes : string) : (Tree.t, int * string) result =
  try Ok (decode_exn bytes) with Parse_error (off, msg) -> Error (off, msg)

let of_trits_exn (s : string) : string =
  let len = String.length s in
  if len = 0 then raise (Parse_error (0, "unexpected end of input"));
  let buf = Buffer.create ((len + 4) / 5) in
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
    if !count = 5 then begin
      Buffer.add_char buf (Char.chr !acc);
      acc := 0;
      count := 0
    end
  done;
  if !slots <> 0 then raise (Parse_error (len, "unexpected end of input"));
  if !count > 0 then begin
    while !count < 5 do
      acc := !acc * 3;
      incr count
    done;
    Buffer.add_char buf (Char.chr !acc)
  end;
  Buffer.contents buf

let of_trits (s : string) : (string, int * string) result =
  try Ok (of_trits_exn s) with Parse_error (off, msg) -> Error (off, msg)

let to_trits (bytes : string) : (string, int * string) result =
  try
    let len = String.length bytes in
    if len = 0 then raise (Parse_error (0, "unexpected end of input"));
    let buf = Buffer.create (len * 5) in
    let slots = ref 1 in
    let feed byte_idx trit =
      if !slots = 0 then begin
        if trit <> 0 then
          raise (Parse_error (byte_idx, "non-zero padding trit in final byte"))
      end else begin
        (match trit with
         | 0 ->
             Buffer.add_char buf '0';
             decr slots
         | 1 -> Buffer.add_char buf '1'
         | 2 ->
             Buffer.add_char buf '2';
             incr slots
         | _ -> assert false)
      end
    in
    for i = 0 to len - 1 do
      if !slots = 0 then raise (Parse_error (i, "trailing bytes after tree"));
      let b = Char.code bytes.[i] in
      if b >= 243 then
        raise (Parse_error (i, Printf.sprintf "invalid base-243 byte 0x%02x" b));
      let t0 = b / 81 in
      let r0 = b - (t0 * 81) in
      let t1 = r0 / 27 in
      let r1 = r0 - (t1 * 27) in
      let t2 = r1 / 9 in
      let r2 = r1 - (t2 * 9) in
      let t3 = r2 / 3 in
      let t4 = r2 - (t3 * 3) in
      feed i t0;
      feed i t1;
      feed i t2;
      feed i t3;
      feed i t4
    done;
    if !slots <> 0 then raise (Parse_error (len, "unexpected end of input"));
    Ok (Buffer.contents buf)
  with Parse_error (off, msg) -> Error (off, msg)
