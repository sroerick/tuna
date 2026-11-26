(* Tuna_local.Local_store: the local directory store (borg/local.borg
   L2).  No database:

     <root>/identity        the local identity name (one line)
     <root>/values/<sha256> one tree per file, canonical ternary
     <root>/ops.jsonl       the append-only op log, one JSON row per op,
                            in the tree_ops row shape and chained as
                            Store.op_concat chains it (genesis 64 zeros),
                            so L6 can push it as a federation ops window
                            without conversion
     <root>/last            the last journaled round's result hash (what
                            get/patch address with no program hash)

   A dict row also carries prev_hash (the value hash the name held
   before), outside the chained fields: apply does not carry it, it
   serves the laptop's own divergence report (L5/L6). *)

let genesis = String.make 64 '0'

(* the undef marker: the zero-length bytes value (borg/local.borg L5) *)
let undef_marker =
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

type op = {
  seq : int64;
  path : string;
  op : string;
  value_hash : string option;
  prev_version : int64 option;
  version : int64 option;
  actor : string;
  ts_unix : int64;
  op_hash : string;
  prev_hash : string option;  (* local-only, not chained *)
}

(* MUST equal Tuna_store.Store.op_concat (store/lib/store.ml); pinned by
   tests/local_tests.ml.  Copied, not linked: the CLI does not link the
   Postgres store. *)
let op_concat ~seq ~path ~op ~value_hash ~prev_version ~version ~actor ~ts_unix
    =
  let s64 = Int64.to_string in
  let opt f = function Some v -> f v | None -> "" in
  Printf.sprintf "%Ld:%s|%s|%s|%s|%s|%s|%Ld" seq path op
    (opt Fun.id value_hash) (opt s64 prev_version) (opt s64 version) actor
    ts_unix

let concat_of (o : op) =
  op_concat ~seq:o.seq ~path:o.path ~op:o.op ~value_hash:o.value_hash
    ~prev_version:o.prev_version ~version:o.version ~actor:o.actor
    ~ts_unix:o.ts_unix

type t = { root : string; identity : string }

let ( / ) = Filename.concat

let rec mkdir_p d =
  if not (Sys.file_exists d) then (
    mkdir_p (Filename.dirname d);
    try Sys.mkdir d 0o700 with Sys_error _ when Sys.file_exists d -> ())

let read_file p =
  let ic = open_in_bin p in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      really_input_string ic (in_channel_length ic))

(* write-then-rename, so a crash never leaves a half-written file *)
let write_file p s =
  let tmp = p ^ ".tmp" in
  let oc = open_out_bin tmp in
  output_string oc s;
  close_out oc;
  Sys.rename tmp p

let default_root () =
  match Sys.getenv_opt "TUNA_LOCAL_DIR" with
  | Some d when d <> "" -> d
  | _ ->
      let data =
        match Sys.getenv_opt "XDG_DATA_HOME" with
        | Some d when d <> "" -> d
        | _ -> Sys.getenv "HOME" / ".local" / "share"
      in
      data / "tuna"

(* open (creating on first use).  The identity is fixed at creation;
   a later [?identity] that disagrees is an error, so one directory
   never mixes two identities' dict paths. *)
let open_ ?identity root =
  mkdir_p (root / "values");
  let idf = root / "identity" in
  let stored =
    if Sys.file_exists idf then Some (String.trim (read_file idf)) else None
  in
  let identity =
    match (stored, identity) with
    | Some s, Some i when s <> i ->
        failwith
          (Printf.sprintf "%s belongs to identity %S, not %S" root s i)
    | Some s, _ -> s
    | None, i ->
        let i = Option.value i ~default:"local" in
        write_file idf (i ^ "\n");
        i
  in
  { root; identity }

(* -- values ------------------------------------------------------------ *)

let put_value t tree =
  let h = Tuna.Hash.hex_of_tree tree in
  let p = t.root / "values" / h in
  if not (Sys.file_exists p) then write_file p (Tuna.Canon.encode tree);
  h

let get_value t hash =
  let p = t.root / "values" / hash in
  if not (Sys.file_exists p) then None
  else
    match Tuna.Canon.of_string (String.trim (read_file p)) with
    | Ok tree when Tuna.Hash.hex_of_tree tree = hash -> Some tree
    | Ok _ -> failwith (Printf.sprintf "value %s does not hash to its name" hash)
    | Error (off, msg) ->
        failwith (Printf.sprintf "value %s unparseable at %d: %s" hash off msg)

(* -- the op log --------------------------------------------------------- *)

module J = Yojson.Basic

let json_of_op (o : op) : J.t =
  let opt_s = function Some s -> `String s | None -> `Null in
  let opt_i = function Some v -> `String (Int64.to_string v) | None -> `Null in
  `Assoc
    [ ("seq", `String (Int64.to_string o.seq)); ("path", `String o.path);
      ("op", `String o.op); ("value_hash", opt_s o.value_hash);
      ("prev_version", opt_i o.prev_version); ("version", opt_i o.version);
      ("actor", `String o.actor); ("ts", `String (Int64.to_string o.ts_unix));
      ("op_hash", `String o.op_hash); ("prev_hash", opt_s o.prev_hash) ]

let op_of_json (j : J.t) : op =
  let open J.Util in
  let s k = to_string (member k j) in
  let os k = to_string_option (member k j) in
  let oi k = Option.map Int64.of_string (os k) in
  { seq = Int64.of_string (s "seq"); path = s "path"; op = s "op";
    value_hash = os "value_hash"; prev_version = oi "prev_version";
    version = oi "version"; actor = s "actor";
    ts_unix = Int64.of_string (s "ts"); op_hash = s "op_hash";
    prev_hash = os "prev_hash" }

let ops t : op list =
  let p = t.root / "ops.jsonl" in
  if not (Sys.file_exists p) then []
  else
    read_file p |> String.split_on_char '\n'
    |> List.filter (fun l -> String.trim l <> "")
    |> List.map (fun l -> op_of_json (J.from_string l))

(* rechain every row: each op_hash = sha256(prev ^ concat), seqs are
   1.. contiguous.  First failure wins. *)
let verify t : (unit, string) result =
  let rec go prev expected = function
    | [] -> Ok ()
    | o :: rest ->
        if o.seq <> expected then
          Error (Printf.sprintf "seq %Ld where %Ld was expected" o.seq expected)
        else
          let h = Tuna.Hash.hex_of_string (prev ^ concat_of o) in
          if h <> o.op_hash then
            Error (Printf.sprintf "op %Ld does not chain (%s)" o.seq o.path)
          else go h (Int64.succ expected) rest
  in
  go genesis 1L (ops t)

(* append one effect op; versions are per path and local (federation:
   the value hash is the shared fact). *)
let append t ~path ~op ~value_hash ?prev_hash () : op =
  let all = ops t in
  let last_seq, last_hash =
    match List.rev all with
    | [] -> (0L, genesis)
    | o :: _ -> (o.seq, o.op_hash)
  in
  let prev_version =
    List.fold_left
      (fun acc o -> if o.path = path && o.version <> None then o.version else acc)
      None all
  in
  let version =
    Some (Int64.succ (Option.value prev_version ~default:0L))
  in
  let o0 =
    { seq = Int64.succ last_seq; path; op; value_hash; prev_version; version;
      actor = t.identity; ts_unix = Int64.of_float (Unix.time ());
      op_hash = ""; prev_hash }
  in
  let o = { o0 with op_hash = Tuna.Hash.hex_of_string (last_hash ^ concat_of o0) } in
  let oc =
    open_out_gen [ Open_wronly; Open_append; Open_creat; Open_binary ] 0o600
      (t.root / "ops.jsonl")
  in
  output_string oc (J.to_string (json_of_op o) ^ "\n");
  close_out oc;
  o

(* -- the dictionary: a fold of dict/<identity>/ puts --------------------- *)

let dict_prefix t = "dict/" ^ t.identity ^ "/"

(* name -> value hash, current binding; the undef marker removes *)
let dict_hashes t : (string * string) list =
  let pre = dict_prefix t in
  let n = String.length pre in
  List.fold_left
    (fun acc o ->
      if String.length o.path > n && String.sub o.path 0 n = pre then
        let name = String.sub o.path n (String.length o.path - n) in
        let acc = List.remove_assoc name acc in
        match o.value_hash with
        | Some h when h <> undef_marker -> (name, h) :: acc
        | _ -> acc
      else acc)
    [] (ops t)
  |> List.sort compare

let dict t : (string * Tuna.Tree.t) list =
  List.map
    (fun (n, h) ->
      match get_value t h with
      | Some tree -> (n, tree)
      | None -> failwith (Printf.sprintf "dict %s: value %s missing" n h))
    (dict_hashes t)

let bind t ~name ~value_hash =
  let prev_hash = List.assoc_opt name (dict_hashes t) in
  ignore
    (append t ~path:(dict_prefix t ^ name) ~op:"put" ~value_hash:(Some value_hash)
       ?prev_hash ())

(* -- session state -------------------------------------------------------- *)

let last t =
  let p = t.root / "last" in
  if Sys.file_exists p then Some (String.trim (read_file p)) else None

let set_last t hash = write_file (t.root / "last") (hash ^ "\n")
