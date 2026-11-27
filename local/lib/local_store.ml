(* Tuna_local.Local_store: the local directory store (borg/local.borg L2)
   backed by Midden_local (Phase 4 unified flat-file Midden store):

     <root>/identity        the local identity name (one line)
     <root>/values/<sha256> universal content-addressed store (Midden_local):
                            Base-243 Tpack trees, raw byte blobs (incl. sealed
                            deriv records), canonical csexp schemas and docs
     <root>/commits.csexp   Midden canonical commit log (def, def_visibility,
                            and op_audit commits), length-delimited & fsynced
     <root>/lock            Midden_local commit-level lockf lock
     <root>/session.lock    held (lockf) for the whole of each REPL/run round
     <root>/last            the last journaled round: its result hash,
                            or "-" when it finished with no result
     <root>/kv/<sha256>     store/put + store/get (L3): sha256 of the key's
                            Base-243 tree -> value hash in values/

   An op_audit commit stores the exact federation WINDOW row fields
   (Store.op_concat chained from genesis 64 zeros), and def / def_visibility
   writes also commit canonical Midden entities under the exact same schemas
   and UUIDv5 Entity_id namespaces as the server's Midden_store. *)

module C = Midden.Csexp
module S = Midden.Schema
module ML = Midden_local.Local_store

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
  prev_hash : string;  (* the previous row's op_hash (window field) *)
  prev_value_hash : string option;  (* local-only, not chained *)
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

(* Canonical Midden schemas matching Tuna_store.Tuna_schemas byte-for-byte. *)
let atom s = C.Atom s
let form xs = C.List xs

let schema_form name version fields =
  form
    [ atom "schema"; atom name;
      form [ atom "version"; atom (string_of_int version) ];
      form (atom "fields" :: fields) ]

let field ?(opt = false) name typ =
  let base = [ atom name; atom typ ] in
  form (if opt then base @ [ atom ":optional" ] else base)

let def_form =
  schema_form "def" 1
    [ field "identity-id" ":string";
      field "identity-name" ":string";
      field "name" ":string";
      field "value-hash" ":string" ]

let def_visibility_form =
  schema_form "def_visibility" 1
    [ field "identity" ":string";
      field "public" ":bool" ]

let op_audit_form =
  schema_form "op_audit" 1
    [ field "actor" ":string";
      field "op" ":string";
      field "op-hash" ":string";
      field "op-seq" ":int";
      field "path" ":string";
      field ~opt:true "prev-version" ":int";
      field "ts-unix" ":int";
      field ~opt:true "value-hash" ":string";
      field ~opt:true "version" ":int" ]

let parse_schema_exn f =
  match S.of_csexp f with
  | Ok s -> s
  | Error e -> failwith ("local_store schema: " ^ S.string_of_error e)

let def_schema = parse_schema_exn def_form
let def_visibility_schema = parse_schema_exn def_visibility_form
let op_audit_schema = parse_schema_exn op_audit_form

let def_schema_hash = Midden.Borge_binding.schema_hash def_schema
let def_visibility_schema_hash = Midden.Borge_binding.schema_hash def_visibility_schema
let op_audit_schema_hash = Midden.Borge_binding.schema_hash op_audit_schema

let record (fields : (string * C.t option) list) : C.t =
  let present =
    List.filter_map (fun (k, v_opt) -> Option.map (fun v -> (k, v)) v_opt) fields
  in
  match Midden.Record.canonical present with
  | Ok doc -> doc
  | Error e -> failwith ("local_store record: " ^ Midden.Record.string_of_error e)

let req_str k v = (k, Some (C.Atom v))
let opt_str k v = (k, Option.map (fun s -> C.Atom s) v)
let req_int k v = (k, Some (C.Atom (Int64.to_string v)))
let opt_int k v = (k, Option.map (fun n -> C.Atom (Int64.to_string n)) v)
let req_bool k v = (k, Some (C.Atom (if v then "true" else "false")))

type t = {
  root : string;
  identity : string;
  midden : ML.t;
}

let ( / ) = Filename.concat

let rec mkdir_p d =
  if not (Sys.file_exists d) then (
    mkdir_p (Filename.dirname d);
    try Sys.mkdir d 0o700 with Sys_error _ when Sys.file_exists d -> ())

let read_file p =
  let ic = open_in_bin p in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      really_input_string ic (in_channel_length ic))

(* write, fsync, rename: a crash never leaves a half-written or empty
   file under the final name *)
let write_file p s =
  let tmp = Printf.sprintf "%s.%d.tmp" p (Unix.getpid ()) in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let b = Bytes.of_string s in
      if Unix.write fd b 0 (Bytes.length b) <> Bytes.length b then
        failwith (p ^ ": short write");
      Unix.fsync fd);
  Sys.rename tmp p

let lock_file root f =
  let fd =
    Unix.openfile (Filename.concat root "session.lock")
      [ Unix.O_RDWR; Unix.O_CREAT ] 0o600
  in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      f ())

let default_root () =
  match Sys.getenv_opt "TUNA_LOCAL_DIR" with
  | Some d when d <> "" -> d
  | _ ->
      let data =
        match (Sys.getenv_opt "XDG_DATA_HOME", Sys.getenv_opt "HOME") with
        | Some d, _ when d <> "" -> d
        | _, Some h when h <> "" -> h / ".local" / "share"
        | _ -> failwith "no data directory: set --dir, TUNA_LOCAL_DIR or HOME"
      in
      data / "tuna"

let ensure_schemas m =
  List.iter
    (fun f ->
      match ML.register_schema m f with
      | Ok _ -> ()
      | Error e -> failwith ("local_store register_schema: " ^ e))
    [ def_form; def_visibility_form; op_audit_form ]

(* open (creating on first use).  The identity is fixed at creation;
   a later [?identity] that disagrees is an error, so one directory
   never mixes two identities' dict paths. *)
let open_ ?identity root =
  let midden = ML.open_dir root in
  lock_file root @@ fun () ->
  ensure_schemas midden;
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
  { root; identity; midden }

let midden t = t.midden
let value_path t hash = t.root / "values" / hash

(* -- values (universal Base-243 trees + raw byte blobs in values/) ----- *)

let is_hash h =
  String.length h = 64
  && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) h

let put_value t tree =
  let packed = Tuna.Tpack.encode tree in
  ML.put_blob t.midden packed

(* a name that is not a sha256 is never a file path *)
let get_value t hash =
  if not (is_hash hash) then None
  else
    match ML.get_blob t.midden hash with
    | None -> None
    | Some bytes ->
        if Tuna.Hash.hex_of_string bytes <> hash then
          failwith (Printf.sprintf "value %s does not hash to its name" hash)
        else
          (match Tuna.Tpack.decode bytes with
           | Ok tree -> Some tree
           | Error _ -> None)

(* -- kv (store/get, store/put) and byte values ---------------------------- *)

let kv_get t key =
  let p = t.root / "kv" / Tuna.Hash.hex_of_tree key in
  if not (Sys.file_exists p) then None else get_value t (String.trim (read_file p))

let kv_put t ~key ~value =
  mkdir_p (t.root / "kv");
  let h = put_value t value in
  write_file (t.root / "kv" / Tuna.Hash.hex_of_tree key) (h ^ "\n")

let put_bytes t s = ML.put_blob t.midden s

let get_bytes t h =
  if not (is_hash h) then None
  else
    match ML.get_blob t.midden h with
    | None -> None
    | Some b ->
        if Tuna.Hash.hex_of_string b <> h then
          failwith (Printf.sprintf "value %s does not hash to its name" h)
        else Some b

(* -- the op log (stored as op_audit commits in Midden_local) ----------- *)

module J = Yojson.Basic

(* the window row of Api.window_row_json (ints for seq, versions, ts),
   plus the local-only prev_value_hash *)
let json_of_op (o : op) : J.t =
  let opt_s = function Some s -> `String s | None -> `Null in
  let opt_i = function Some v -> `Int (Int64.to_int v) | None -> `Null in
  `Assoc
    [ ("seq", `Int (Int64.to_int o.seq)); ("path", `String o.path);
      ("op", `String o.op); ("value_hash", opt_s o.value_hash);
      ("prev_version", opt_i o.prev_version); ("version", opt_i o.version);
      ("actor", `String o.actor); ("ts", `Int (Int64.to_int o.ts_unix));
      ("op_hash", `String o.op_hash); ("prev_hash", `String o.prev_hash);
      ("prev_value_hash", opt_s o.prev_value_hash) ]

let op_of_json (j : J.t) : op =
  let open J.Util in
  let s k = to_string (member k j) in
  let os k = to_string_option (member k j) in
  let oi k = Option.map Int64.of_int (to_int_option (member k j)) in
  { seq = Int64.of_int (to_int (member "seq" j)); path = s "path"; op = s "op";
    value_hash = os "value_hash"; prev_version = oi "prev_version";
    version = oi "version"; actor = s "actor";
    ts_unix = Int64.of_int (to_int (member "ts" j)); op_hash = s "op_hash";
    prev_hash = s "prev_hash"; prev_value_hash = os "prev_value_hash" }

let field_of_doc doc name =
  match doc with
  | C.List pairs ->
      List.find_map
        (function
          | C.List [ C.Atom k; C.Atom v ] when String.equal k name -> Some v
          | _ -> None)
        pairs
  | _ -> None

let req_field doc name =
  match field_of_doc doc name with
  | Some v -> v
  | None -> failwith ("op_audit doc missing required field " ^ name)

let req_i64_field doc name =
  let s = req_field doc name in
  match Int64.of_string_opt s with
  | Some n -> n
  | None -> failwith ("op_audit doc non-int field " ^ name)

let opt_i64_field doc name =
  match field_of_doc doc name with
  | None -> None
  | Some s -> (
      match Int64.of_string_opt s with
      | Some n -> Some n
      | None -> failwith ("op_audit doc non-int field " ^ name))

let ops t : op list =
  let commits = ML.commits t.midden in
  let prev_by_path = Hashtbl.create 16 in
  let _, acc =
    List.fold_left
      (fun (prev_op_hash, acc) (c : Midden.Log_commits.t) ->
        match (c.schema, c.head) with
        | Some sh, Some h when String.equal sh op_audit_schema_hash ->
            let bytes =
              match ML.get_blob t.midden h with
              | Some b -> b
              | None ->
                  failwith (Printf.sprintf "op_audit blob %s missing" h)
            in
            if Tuna.Hash.hex_of_string bytes <> h then
              failwith (Printf.sprintf "op_audit blob %s tampered" h);
            let doc =
              match C.decode bytes with
              | Ok d -> d
              | Error _ ->
                  failwith (Printf.sprintf "op_audit blob %s invalid csexp" h)
            in
            let path = req_field doc "path" in
            let op_name = req_field doc "op" in
            let value_hash = field_of_doc doc "value-hash" in
            let prev_value_hash =
              if String.length path >= 5 && String.sub path 0 5 = "dict/"
                 && value_hash <> None
              then Hashtbl.find_opt prev_by_path path
              else None
            in
            (match value_hash with
             | Some vh -> Hashtbl.replace prev_by_path path vh
             | None -> ());
            let op_hash = req_field doc "op-hash" in
            let row =
              { seq = req_i64_field doc "op-seq";
                path;
                op = op_name;
                value_hash;
                prev_version = opt_i64_field doc "prev-version";
                version = opt_i64_field doc "version";
                actor = req_field doc "actor";
                ts_unix = req_i64_field doc "ts-unix";
                op_hash;
                prev_hash = prev_op_hash;
                prev_value_hash }
            in
            (op_hash, row :: acc)
        | _ -> (prev_op_hash, acc))
      (genesis, []) commits
  in
  List.rev acc

(* rechain every row and verify the underlying Midden commit chain:
   each op_hash = sha256(prev ^ concat), seqs are 1.. contiguous. *)
let verify t : (unit, string) result =
  try
    let rec go prev expected = function
      | [] -> Ok ()
      | o :: rest ->
          if o.seq <> expected then
            Error (Printf.sprintf "seq %Ld where %Ld was expected" o.seq expected)
          else
            let h = Tuna.Hash.hex_of_string (prev ^ concat_of o) in
            if o.prev_hash <> prev then
              Error (Printf.sprintf "op %Ld names the wrong predecessor" o.seq)
            else if h <> o.op_hash then
              Error (Printf.sprintf "op %Ld does not chain (%s)" o.seq o.path)
            else go h (Int64.succ expected) rest
    in
    go genesis 1L (ops t)
  with Failure msg -> Error msg

(* append one effect op as a Midden op_audit commit; versions are per path
   and local (federation: the value hash is the shared fact). [no_versions]
   appends a non-effect row (dict-visibility), whose versions are null as on
   the server, so fed apply lands it as a cas proof row. *)
let append t ~path ~op ~value_hash ?prev_value_hash ?(no_versions = false) () : op =
  let all = ops t in
  let last_seq, last_hash =
    match List.rev all with
    | [] -> (0L, genesis)
    | o :: _ -> (o.seq, o.op_hash)
  in
  let prev_version =
    if no_versions then None
    else
      List.fold_left
        (fun acc o -> if o.path = path && o.version <> None then o.version else acc)
        None all
  in
  let version =
    if no_versions then None
    else Some (Int64.succ (Option.value prev_version ~default:0L))
  in
  let o0 =
    { seq = Int64.succ last_seq; path; op; value_hash; prev_version; version;
      actor = t.identity; ts_unix = Int64.of_float (Unix.time ());
      op_hash = ""; prev_hash = last_hash; prev_value_hash }
  in
  let o = { o0 with op_hash = Tuna.Hash.hex_of_string (last_hash ^ concat_of o0) } in
  let doc =
    record
      [ req_str "actor" o.actor;
        req_str "op" o.op;
        req_str "op-hash" o.op_hash;
        req_int "op-seq" o.seq;
        req_str "path" o.path;
        opt_int "prev-version" o.prev_version;
        req_int "ts-unix" o.ts_unix;
        opt_str "value-hash" o.value_hash;
        opt_int "version" o.version ]
  in
  let entity =
    Midden.Entity_id.of_key ~namespace:"local-op-audit"
      (Printf.sprintf "%s:%Ld:%s" t.identity o.seq o.op_hash)
  in
  (match
     ML.put t.midden ~entity ~prev:None ~schema:op_audit_schema_hash
       ~author:t.identity ~reason:("op:" ^ op) doc
   with
   | Ok _ -> ()
   | Error e -> failwith ("local_store append: " ^ e));
  o

(* run [f] holding the directory's round lock (one round at a time) *)
let with_lock t f = lock_file t.root f

(* the identity fixed at directory creation (L2): a local call is self-only *)
let identity t = t.identity

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

(* the raw last value at a dict path, the undef marker included *)
let last_value t path =
  List.fold_left
    (fun acc o -> if o.path = path && o.value_hash <> None then o.value_hash else acc)
    None (ops t)

(* dict-visibility (borg/federation.borg dictionary-visibility): one
   non-effect op, path vis/<identity>/public|private, no value hash, no
   versions - what fed apply lands as a cas proof row.  Also writes the
   canonical Midden def_visibility entity using the server's Entity_id. *)
let visibility_path ~identity ~public =
  "vis/" ^ identity ^ (if public then "/public" else "/private")

let set_visibility t ~target ~public =
  let entity = Midden.Entity_id.of_key ~namespace:"def_visibility" target in
  let prev =
    match ML.head t.midden ~entity with None -> None | Some h -> h.head
  in
  let doc =
    record [ req_str "identity" target; req_bool "public" public ]
  in
  (match
     ML.put t.midden ~entity ~prev ~schema:def_visibility_schema_hash
       ~author:t.identity ~reason:"dict-visibility" doc
   with
   | Ok _ -> ()
   | Error e -> failwith ("local_store set_visibility: " ^ e));
  ignore
    (append t ~path:(visibility_path ~identity:target ~public)
       ~op:"dict-visibility" ~value_hash:None ~no_versions:true ())

let bind t ~name ~value_hash =
  let entity =
    Midden.Entity_id.of_key ~namespace:"def" (t.identity ^ "/" ^ name)
  in
  let prev =
    match ML.head t.midden ~entity with None -> None | Some h -> h.head
  in
  if String.equal value_hash undef_marker then (
    ignore (put_bytes t "");
    (match prev with
     | Some _ -> (
         match
           ML.delete t.midden ~entity ~prev ~author:t.identity
             ~reason:("undef:" ^ name) ()
         with
         | Ok _ -> ()
         | Error e -> failwith ("local_store undef: " ^ e))
     | None -> ()))
  else (
    let doc =
      record
        [ req_str "identity-id" t.identity;
          req_str "identity-name" t.identity;
          req_str "name" name;
          req_str "value-hash" value_hash ]
    in
    match
      ML.put t.midden ~entity ~prev ~schema:def_schema_hash ~author:t.identity
        ~reason:("def:" ^ name) doc
    with
    | Ok _ -> ()
    | Error e -> failwith ("local_store def: " ^ e));
  let path = dict_prefix t ^ name in
  let prev_value_hash = last_value t path in
  ignore (append t ~path ~op:"put" ~value_hash:(Some value_hash) ?prev_value_hash ())

(* -- dictionary visibility (borg/federation.borg dictionary-visibility) -- *)

let visibility_of_path path =
  let pre = "vis/" in
  if String.length path > String.length pre && String.sub path 0 (String.length pre) = pre
  then
    match String.rindex_opt path '/' with
    | Some i when i > String.length pre ->
        let id = String.sub path (String.length pre) (i - String.length pre) in
        (match String.sub path (i + 1) (String.length path - i - 1) with
         | "public" -> Some (id, true)
         | "private" -> Some (id, false)
         | _ -> None)
    | _ -> None
  else None

let public_identities t : string list =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun o ->
      if o.op = "dict-visibility" then
        match visibility_of_path o.path with
        | Some (id, public) -> Hashtbl.replace tbl id public
        | None -> ())
    (ops t);
  List.sort compare
    (Hashtbl.fold (fun id public acc -> if public then id :: acc else acc) tbl [])

(* -- session state -------------------------------------------------------- *)

type last = No_round | No_result | Result of string

let last t =
  let p = t.root / "last" in
  if not (Sys.file_exists p) then No_round
  else
    match String.trim (read_file p) with
    | "-" -> No_result
    | h -> Result h

(* a round with no result tree still becomes the last round, as on the
   server (repl_state moves to every journaled round) *)
let set_last t = function
  | Some hash -> write_file (t.root / "last") (hash ^ "\n")
  | None -> write_file (t.root / "last") "-\n"
