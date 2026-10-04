(* Tuna_server.Tree_prims: the M10 substrate prims over the derived
   path index (migrations 0005/0006).

   tree/get, tree/put, tree/cas, tree/list address the path index; tree/del
   removes a path (M11, paths only - the value store is immutable); ns/fork
   copies an index slice under a new prefix.  Paths and prefixes travel
   as string trees (Tuna.Cstr); the value side is content-addressed
   ternary (tree_values, dedup by sha256) and tree_paths stores hashes
   only -- paths are never storage truth (tuna.borg watch note).

   Contract discipline (replay.prim-versioning): these prims pin
   prim_contract "2" (see Prims.contract — v2 = binary string codec).
   Handlers here do the store work and journal their
   own EFFECT rows (value_hash + version) into the tree_ops chain; the
   run boundary (Run.execute) journals every tree-prim ERROR answer as a
   no-effect op row (denials, CAS conflicts, bad paths) -- journaled
   error answers, per grants.borg/journal.borg.  Payload caps follow the
   v1 prims: args at the boundary, values and list results here. *)

open Tuna_store.Direct

module S = Tuna_store.Store

type answer = [ `Ok of Tuna.Tree.t | `Error of string ]

let names =
  [ "tree/get"; "tree/put"; "tree/cas"; "tree/list"; "tree/del"; "ns/fork" ]

let exists name = List.mem name names

(* the tree_ops op kind of a substrate prim, None for everything else *)
let op_of_name = function
  | "tree/get" -> Some "get"
  | "tree/put" -> Some "put"
  | "tree/cas" -> Some "cas"
  | "tree/list" -> Some "list"
  | "tree/del" -> Some "delete"
  | "ns/fork" -> Some "fork"
  | _ -> None

(* -- path validation -------------------------------------------------- *)

(* non-empty, no leading/trailing '/', printable ASCII, <= 512 chars.
   Violations are error ANSWERS, never exceptions. *)
let path_cap = 512

let validate_path what s =
  if String.length s = 0 then Some (Printf.sprintf "%s: path must be non-empty" what)
  else if String.length s > path_cap then
    Some (Printf.sprintf "%s: path exceeds %d characters" what path_cap)
  else if s.[0] = '/' then Some (Printf.sprintf "%s: path must not start with '/'" what)
  else if s.[String.length s - 1] = '/' then
    Some (Printf.sprintf "%s: path must not end with '/'" what)
  else if
    not (String.for_all (fun c -> c >= ' ' && c <= '~') s)
  then Some (Printf.sprintf "%s: path must be printable ASCII" what)
  else None

(* like validate_path but the empty string is the whole-namespace
   prefix, and a trailing '/' is allowed (prefixes name a namespace,
   not a path).  The API/prim list surfaces use this. *)
let validate_prefix what s =
  if s = "" then None
  else if s.[String.length s - 1] = '/' then
    if String.length s = 1 then
      Some (Printf.sprintf "%s: prefix must not be just '/'" what)
    else validate_path what (String.sub s 0 (String.length s - 1))
  else validate_path what s

let check_path what s =
  match validate_path what s with Some e -> Error e | None -> Ok s

(* best-effort path candidate from a call's args (first element as a
   string tree): what the denial op row records when the boundary
   answers an error.  Undecodable args give "". *)
let op_path_of_args (args : Tuna.Tree.t) : string =
  match Prims.list_of_tree args with
  | t :: _ -> (match Prims.unstr t with Some s -> s | None -> "")
  | [] -> ""

(* the paths a substrate call would touch, for the live grant prefix
   check: get/put/cas/del/list address their first arg; ns/fork reads src
   AND writes dst, so the grant must cover both.  None = no decodable
   paths (the handler will reject the call as an error answer). *)
let grant_paths (name : string) (args : Tuna.Tree.t) : string list option =
  let elems = Prims.list_of_tree args in
  let str i =
    match List.nth_opt elems i with Some t -> Prims.unstr t | None -> None
  in
  match name with
  | "tree/get" | "tree/put" | "tree/cas" | "tree/list" | "tree/del" ->
      Option.map (fun p -> [ p ]) (str 0)
  | "ns/fork" -> (
      match (str 0, str 1) with
      | Some src, Some dst -> Some [ src; dst ]
      | _ -> None)
  | _ -> None

(* -- value side -------------------------------------------------------- *)

(* store a value tree content-addressed; returns (hash, ternary) *)
let value_store p ~value =
  let ternary = Tuna.Canon.encode value in
  let hash = Tuna.Hash.hex_of_string ternary in
  S.value_put p ~hash ~ternary >>= fun () -> return (hash, ternary)

let fetch_value_tree p value_hash =
  S.value_fetch p value_hash
  >>= function
  | None -> return (Error ("stored value missing for hash " ^ value_hash))
  | Some ternary -> (
      match Tuna.Canon.of_string ternary with
      | Ok t -> return (Ok t)
      | Error (off, msg) ->
          return
            (Error (Printf.sprintf "stored value unparseable at %d: %s" off msg)))

let version_string v = Printf.sprintf "version:%Ld" v

(* -- tree/get: [path] -> value tree ------------------------------------ *)

let tree_get p ~actor args =
  match Prims.list_of_tree args with
  | [ path_t ] -> (
      match Prims.unstr path_t with
      | None -> return (`Error "tree/get: path must be a string tree")
      | Some path -> (
          match validate_path "tree/get" path with
          | Some e -> return (`Error e)
          | None -> (
              S.path_get p ~path
              >>= function
              | None -> return (`Error ("tree/get: no value at path " ^ path))
              | Some entry -> (
                  S.op_append p ~op:"get" ~path ~value_hash:None ~prev_version:None
                    ~version:None ~actor
                  >>= fun _ ->
                  fetch_value_tree p entry.S.tp_value_hash
                  >>= (function
                        | Ok t -> return (`Ok t)
                        | Error e -> return (`Error e))))))
  | _ -> return (`Error "tree/get: args must be [path]")

(* -- tree/put: [path value] -> "version:N" (unconditional write) ------- *)

let tree_put p ~actor args =
  match Prims.list_of_tree args with
  | [ path_t; value ] -> (
      match Prims.unstr path_t with
      | None -> return (`Error "tree/put: path must be a string tree")
      | Some path -> (
          match validate_path "tree/put" path with
          | Some e -> return (`Error e)
          | None -> (
              let ternary = Tuna.Canon.encode value in
              if String.length ternary > Prims.payload_cap then
                return (`Error "tree/put: value exceeds the journal payload cap")
              else
                let hash = Tuna.Hash.hex_of_string ternary in
                S.value_put p ~hash ~ternary
                >>= fun () ->
                S.path_put p ~path ~value_hash:hash ~owner:actor
                >>= fun newv ->
                S.op_append p ~op:"put" ~path ~value_hash:(Some hash)
                  ~prev_version:(if newv = 1L then None else Some (Int64.pred newv))
                  ~version:(Some newv) ~actor
                >>= fun _ -> return (`Ok (Prims.str (version_string newv))))))
  | _ -> return (`Error "tree/put: args must be [path value]")

(* -- tree/cas: [path expected-version value] -> "version:N" ------------ *)

let tree_cas p ~actor args =
  match Prims.list_of_tree args with
  | [ path_t; ver_t; value ] -> (
      match Prims.unstr path_t with
      | None -> return (`Error "tree/cas: path must be a string tree")
      | Some path -> (
          match validate_path "tree/cas" path with
          | Some e -> return (`Error e)
          | None -> (
              match Option.bind (Prims.unstr ver_t) int_of_string_opt with
              | None ->
                  return
                    (`Error "tree/cas: expected-version must be an integer string")
              | Some n -> (
                  let ternary = Tuna.Canon.encode value in
                  if String.length ternary > Prims.payload_cap then
                    return
                      (`Error "tree/cas: value exceeds the journal payload cap")
                  else
                    let hash = Tuna.Hash.hex_of_string ternary in
                    S.value_put p ~hash ~ternary
                    >>= fun () ->
                    S.path_put_cas p ~path ~value_hash:hash ~owner:actor
                      ~expected_version:(Some (Int64.of_int n))
                      ~expected_hash:None
                    >>= (function
                          | `Ok v ->
                              S.op_append p ~op:"cas" ~path ~value_hash:(Some hash)
                                ~prev_version:(Some (Int64.of_int n))
                                ~version:(Some v) ~actor
                              >>= fun _ ->
                              return (`Ok (Prims.str (version_string v)))
                          | `Conflict ->
                              return
                                (`Error
                                   (Printf.sprintf
                                      "tree/cas: version conflict at %s (expected %d)"
                                      path n))
                          | `Absent ->
                              return
                                (`Error ("tree/cas: no value at path " ^ path)))))))
  | _ -> return (`Error "tree/cas: args must be [path expected-version value]")

(* -- tree/list: [prefix] -> list of (path value version) --------------- *)

(* read bound: entries per list call *)
let list_cap = 256

let entry_tree (e : S.path_entry) value =
  Tuna.Tree.Fork
    ( Prims.str e.S.tp_path
    , Tuna.Tree.Fork (value, Prims.str (Int64.to_string e.S.tp_version)) )

let tree_list p ~actor args =
  (* the shared tail: fetch entry values, cap the encoded result, journal
     the list op.  [after] is the exclusive cursor bound ("" = from the
     range start), [limit] the entry count. *)
  let list_after prefix ~after ~limit =
    S.path_list p ~prefix ~limit ~after ()
    >>= fun entries ->
    let rec go acc = function
      | [] -> return (Ok (List.rev acc))
      | e :: rest -> (
          fetch_value_tree p e.S.tp_value_hash
          >>= (function
                | Ok t -> go (entry_tree e t :: acc) rest
                | Error msg -> return (Error msg)))
    in
    go [] entries
    >>= (function
          | Error msg -> return (`Error msg)
          | Ok trees -> (
              let result = Prims.tree_of_list trees in
              if String.length (Tuna.Canon.encode result) > Prims.payload_cap
              then return (`Error "tree/list: result exceeds the journal payload cap")
              else
                S.op_append p ~op:"list" ~path:prefix ~value_hash:None
                  ~prev_version:None ~version:None ~actor
                >>= fun _ -> return (`Ok result)))
  in
  match Prims.list_of_tree args with
  | [ prefix_t ] -> (
      (* the 1-arg shape is byte-identical to the pre-window contract
         (board.borg L6 / F15(b)): uncapped-by-caller, list_cap bound,
         journaled "list" op on the prefix. *)
      match Prims.unstr prefix_t with
      | None -> return (`Error "tree/list: prefix must be a string tree")
      | Some prefix -> (
          match validate_prefix "tree/list" prefix with
          | Some e -> return (`Error e)
          | None -> list_after prefix ~after:"" ~limit:list_cap))
  | [ prefix_t; cursor_t; limit_t ] -> (
      (* windowed read (board.borg L6): [prefix cursor limit].  The cursor
         is an EXCLUSIVE lower bound within the prefix range - "" starts
         at the range head, so a page walk feeds each window the last
         path of the previous one.  The limit is a decimal string
         (same convention as the cas expected-version arg). *)
      match Prims.unstr prefix_t with
      | None -> return (`Error "tree/list: prefix must be a string tree")
      | Some prefix -> (
          match Prims.unstr cursor_t with
          | None -> return (`Error "tree/list: cursor must be a string tree")
          | Some cursor -> (
              match Prims.unstr limit_t with
              | None -> return (`Error "tree/list: limit must be a string tree")
              | Some limit_s -> (
                  match validate_prefix "tree/list" prefix with
                  | Some e -> return (`Error e)
                  | None -> (
                      let digits =
                        limit_s <> ""
                        && String.for_all
                             (fun c -> c >= '0' && c <= '9') limit_s
                      in
                      if not digits then
                        return (`Error "tree/list: limit must be a decimal number")
                      else
                        let limit = int_of_string limit_s in
                        if limit < 1 then
                          return (`Error "tree/list: limit must be >= 1")
                        else
                          let cursor_valid =
                            if cursor = "" then None else validate_prefix "tree/list" cursor
                          in
                          match cursor_valid with
                          | Some e -> return (`Error e)
                          | None -> list_after prefix ~after:cursor ~limit)))))
  | _ -> return (`Error "tree/list: args must be [prefix]")

(* -- ns/fork: [src dst] -> "copied:N" ---------------------------------- *)

(* copy bound: the source slice may not exceed this many paths *)
let fork_cap = 4096

let ns_fork_prim p ~actor args =
  match Prims.list_of_tree args with
  | [ src_t; dst_t ] -> (
      match (Prims.unstr src_t, Prims.unstr dst_t) with
      | Some src, Some dst -> (
          match (validate_path "ns/fork src" src, validate_path "ns/fork dst" dst)
          with
          | Some e, _ | _, Some e -> return (`Error e)
          | None, None -> (
              S.path_list p ~prefix:src ~limit:(fork_cap + 1) ()
              >>= fun probe ->
              if List.length probe > fork_cap then
                return
                  (`Error
                     (Printf.sprintf "ns/fork: source namespace too large (> %d paths)"
                        fork_cap))
              else
                S.ns_fork p ~src_prefix:src ~dst_prefix:dst ~actor
                >>= fun copied ->
                return (`Ok (Prims.str (Printf.sprintf "copied:%d" copied)))))
      | _ -> return (`Error "ns/fork: src and dst must be string trees"))
  | _ -> return (`Error "ns/fork: args must be [src dst]")


(* -- tree/del: [path] -> "deleted:N" (the row's next version) ----------- *)

(* M11 tree/del (operator-approved pins): paths only - the value store is
   IMMUTABLE, the delete never touches values, so the journal row carries
   value_hash NULL (no value effect) and version = the row's NEXT version
   (v+1, what a re-put would have slotted past), making replay ordering
   across the path's lifetime unambiguous; the rewind fold drops the path
   on exactly these rows via prev_version.  Grant checking is tree/put's
   covering-prefix rule with admins exempt, enforced by the run boundary
   (Run.execute).  Deleting an absent path is an ERROR ANSWER - journaled
   as a NULL-effect denial row by the boundary - never an exception. *)
let tree_del p ~actor args =
  match Prims.list_of_tree args with
  | [ path_t ] -> (
      match Prims.unstr path_t with
      | None -> return (`Error "tree/del: path must be a string tree")
      | Some path -> (
          match validate_path "tree/del" path with
          | Some e -> return (`Error e)
          | None -> (
              S.path_delete p ~path ~expected_version:None
              >>= function
              | `Ok v ->
                  S.op_append p ~op:"delete" ~path ~value_hash:None
                    ~prev_version:(Some v) ~version:(Some (Int64.succ v)) ~actor
                  >>= fun _ ->
                  return
                    (`Ok
                       (Prims.str
                            (Printf.sprintf "deleted:%Ld" (Int64.succ v))))
              | `Absent -> return (`Error ("tree/del: no value at path " ^ path))
              | `Conflict ->
                  (* unconditional deletes cannot conflict; kept for
                     exhaustiveness *)
                  return (`Error "tree/del: unexpected conflict"))))
  | _ -> return (`Error "tree/del: args must be [path]")

(* -- dispatch ----------------------------------------------------------- *)

let dispatch ~pool ~actor ~name ~args : answer =
  match name with
  | "tree/get" -> tree_get pool ~actor args
  | "tree/put" -> tree_put pool ~actor args
  | "tree/cas" -> tree_cas pool ~actor args
  | "tree/list" -> tree_list pool ~actor args
  | "tree/del" -> tree_del pool ~actor args
  | "ns/fork" -> ns_fork_prim pool ~actor args
  | other -> return (`Error ("unknown prim: " ^ other))
