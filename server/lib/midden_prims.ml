(* Tuna_server.Midden_prims: native Midden boundary prims for Sabra programs.

   midden/get    [schema entity]                   -> (pair head doc-tree) | Leaf
   midden/put    [schema entity prev-head doc]     -> new-head-hash
   midden/delete [schema entity prev-head]         -> "deleted:<seq>"
   midden/query  [projection filter cursor limit]  -> list of encoded row trees

   Every call is structurally grant-checked by Run.execute (scope_schema /
   scope_namespace / scope_projection) and journaled into Tuna's run journal
   so faithful replay verifies Midden-backed programs identically to any
   other run. *)

open Tuna_store.Direct

module S = Tuna_store.Store
module Db = Tuna_store.Db
module TS = Tuna_store.Tuna_schemas
module MS = Midden_store.Store
module MC = Tuna_codec.Midden_codec
module V = Pgx.Value

type answer = [ `Ok of Tuna.Tree.t | `Error of string ]

let names = [ "midden/get"; "midden/put"; "midden/delete"; "midden/query" ]

let exists name = List.mem name names

(* Extract the structural scope (schema option * projection option) that a
   midden/* prim call touches, so Run.execute can enforce Store.check_midden_grant. *)
let grant_scope pool name args : (string option * string option) option =
  TS.ensure_all pool;
  let elems = Prims.list_of_tree args in
  let str i =
    match List.nth_opt elems i with Some t -> Prims.unstr t | None -> None
  in
  match name with
  | "midden/get" | "midden/put" | "midden/delete" -> (
      match str 0 with
      | Some schema -> Some (Some schema, None)
      | None -> None)
  | "midden/query" -> (
      match str 0 with
      | Some proj_name -> (
          match
            Db.q ~params:[ V.of_string proj_name ] pool
              "SELECT schema_name FROM midden_projections WHERE name = $1"
          with
          | [ r ] -> Some (Some (S.text r 0 "midden_projections.schema_name"), Some proj_name)
          | _ -> (
              match
                Db.q ~params:[ V.of_string proj_name ] pool
                  "SELECT name FROM midden_schema_refs WHERE name = $1"
              with
              | [ _ ] -> Some (Some proj_name, Some proj_name)
              | _ -> Some (None, Some proj_name)))
      | None -> None)
  | _ -> None

let registered_schema pool name =
  TS.ensure_all pool;
  match
    Db.q ~params:[ V.of_string name ] pool
      "SELECT r.hash, v.bytes FROM midden_schema_refs r \
       JOIN midden_values v ON v.hash = r.hash WHERE r.name = $1"
  with
  | [ r ] -> (
      let hash = S.text r 0 "midden_schema_refs.hash" in
      let bytes = S.binary r 1 "midden_values.bytes" in
      match Midden.Csexp.decode bytes with
      | Error e -> Error (Midden.Csexp.string_of_error e)
      | Ok form -> (
          match Midden.Schema.of_csexp form with
          | Error e -> Error (Midden.Schema.string_of_error e)
          | Ok sc -> Ok (hash, sc)))
  | _ -> Error (Printf.sprintf "unknown Midden schema %S" name)

let midden_get pool args =
  match Prims.list_of_tree args with
  | [ schema_t; entity_t ] -> (
      match (Prims.unstr schema_t, Prims.unstr entity_t) with
      | Some schema_name, Some entity -> (
          match registered_schema pool schema_name with
          | Error msg -> return (`Error ("midden/get: " ^ msg))
          | Ok (_sh, schema) -> (
              let m = Db.midden pool in
              match MS.head m ~entity with
              | Error e ->
                  return (`Error ("midden/get: " ^ MS.string_of_error e))
              | Ok None | Ok (Some { head = None; _ }) ->
                  return (`Ok Tuna.Tree.Leaf)
              | Ok (Some { head = Some head_hash; _ }) -> (
                  match MS.get_as m ~entity ~version:schema.Midden.Schema.version with
                  | Error e ->
                      return (`Error ("midden/get: " ^ MS.string_of_error e))
                  | Ok None -> return (`Ok Tuna.Tree.Leaf)
                  | Ok (Some stored) -> (
                      match MC.csexp_to_tree schema stored.MS.document with
                      | Error e -> return (`Error ("midden/get: " ^ e))
                      | Ok doc_tree ->
                          let res =
                            Tuna.Tree.Fork (Prims.str head_hash, doc_tree)
                          in
                          if String.length (Tuna.Canon.encode res) > Prims.payload_cap
                          then
                            return
                              (`Error
                                 "midden/get: result exceeds the journal payload cap")
                          else return (`Ok res)))))
      | _ -> return (`Error "midden/get: schema and entity must be string trees"))
  | _ -> return (`Error "midden/get: args must be [schema entity]")

let midden_put pool ~actor args =
  match Prims.list_of_tree args with
  | [ schema_t; entity_t; prev_t; doc_t ] -> (
      match (Prims.unstr schema_t, Prims.unstr entity_t, Prims.unstr prev_t) with
      | Some schema_name, Some entity, Some prev_s -> (
          match registered_schema pool schema_name with
          | Error msg -> return (`Error ("midden/put: " ^ msg))
          | Ok (schema_hash, schema) -> (
              match MC.tree_to_csexp schema doc_t with
              | Error msg -> return (`Error ("midden/put: " ^ msg))
              | Ok doc -> (
                  let prev = if prev_s = "" then None else Some prev_s in
                  match
                    MS.put (Db.midden pool) ~entity ~prev ~schema:schema_hash
                      ~author:actor ~reason:("midden/put:" ^ schema_name) doc
                  with
                  | Ok receipt -> return (`Ok (Prims.str receipt.MS.head))
                  | Error e ->
                      return (`Error ("midden/put: " ^ MS.string_of_error e)))))
      | _ ->
          return
            (`Error
               "midden/put: schema, entity, and prev_head must be string trees"))
  | _ -> return (`Error "midden/put: args must be [schema entity prev_head doc]")

let midden_delete pool ~actor args =
  match Prims.list_of_tree args with
  | [ schema_t; entity_t; prev_t ] -> (
      match (Prims.unstr schema_t, Prims.unstr entity_t, Prims.unstr prev_t) with
      | Some schema_name, Some entity, Some prev_s -> (
          match registered_schema pool schema_name with
          | Error msg -> return (`Error ("midden/delete: " ^ msg))
          | Ok _ -> (
              let prev = if prev_s = "" then None else Some prev_s in
              match
                MS.delete (Db.midden pool) ~entity ~prev ~author:actor
                  ~reason:("midden/delete:" ^ schema_name) ()
              with
              | Ok receipt ->
                  return
                    (`Ok
                       (Prims.str (Printf.sprintf "deleted:%d" receipt.MS.seq)))
              | Error e ->
                  return (`Error ("midden/delete: " ^ MS.string_of_error e))))
      | _ ->
          return
            (`Error
               "midden/delete: schema, entity, and prev_head must be string trees"))
  | _ -> return (`Error "midden/delete: args must be [schema entity prev_head]")

let quote_ident name =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

let query_cap = 256

let parse_filters s =
  if String.trim s = "" then Ok []
  else
    let parts = String.split_on_char '&' s in
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | "" :: rest -> go acc rest
      | kv :: rest -> (
          match String.index_opt kv '=' with
          | None -> Error (Printf.sprintf "bad filter clause %S (expected k=v)" kv)
          | Some i ->
              let k = String.sub kv 0 i in
              let v = String.sub kv (i + 1) (String.length kv - i - 1) in
              go ((k, v) :: acc) rest)
    in
    go [] parts

let tree_of_pg_cell ~data_type cell =
  match cell with
  | None -> Tuna.Tree.Leaf
  | Some _ -> (
      match data_type with
      | "bigint" | "integer" | "smallint" ->
          MC.int64_to_tree (V.to_int64_exn cell)
      | "boolean" ->
          if V.to_bool_exn cell then Tuna.Tree.Stem Tuna.Tree.Leaf
          else Tuna.Tree.Leaf
      | "bytea" -> Prims.str (V.to_binary_exn cell)
      | _ -> Prims.str (V.to_string_exn cell))

let rec nat n = if n <= 0 then Tuna.Tree.Leaf else Tuna.Tree.Stem (nat (n - 1))

(* Encode a SQL projection row as a dialect v1.1 tagged record (1-based tags
   in information_schema ordinal_position order, matching schema field order
   for default views and declared column order for projections). *)
let tree_of_row (cols : (string * string) list) (row : Pgx.row) : Tuna.Tree.t =
  let pairs =
    List.mapi
      (fun i (_name, data_type) ->
        let v = tree_of_pg_cell ~data_type (List.nth row i) in
        (i + 1, v))
      cols
  in
  List.fold_right
    (fun (tag, v) tl ->
      Tuna.Tree.Fork (Tuna.Tree.Fork (nat tag, Tuna.Tree.Fork (v, Tuna.Tree.Leaf)), tl))
    pairs Tuna.Tree.Leaf

let midden_query pool args =
  TS.ensure_all pool;
  match Prims.list_of_tree args with
  | [ proj_t; filter_t; cursor_t; limit_t ] -> (
      match
        (Prims.unstr proj_t, Prims.unstr filter_t, Prims.unstr cursor_t, Prims.unstr limit_t)
      with
      | Some proj_name, Some filter_s, Some cursor_s, Some limit_s -> (
          let limit_opt =
            if limit_s = "" then Some 50
            else
              match int_of_string_opt limit_s with
              | Some n when n >= 1 && n <= query_cap -> Some n
              | _ -> None
          in
          match limit_opt with
          | None ->
              return
                (`Error
                   (Printf.sprintf "midden/query: limit must be 1..%d" query_cap))
          | Some limit -> (
              (* Verify proj_name is a registered projection or schema table *)
              let is_proj =
                Db.q ~params:[ V.of_string proj_name ] pool
                  "SELECT 1 FROM midden_projections WHERE name = $1 \
                   UNION ALL \
                   SELECT 1 FROM midden_schema_refs WHERE name = $1 LIMIT 1"
                <> []
              in
              if not is_proj then
                return
                  (`Error
                     (Printf.sprintf "midden/query: unknown projection %S"
                        proj_name))
              else
                match parse_filters filter_s with
                | Error msg -> return (`Error ("midden/query: " ^ msg))
                | Ok filters -> (
                    let cols =
                      Db.q ~params:[ V.of_string proj_name ] pool
                        "SELECT column_name, data_type \
                         FROM information_schema.columns \
                         WHERE table_schema = 'public' AND table_name = $1 \
                         ORDER BY ordinal_position"
                      |> List.map (fun r ->
                             ( S.text r 0 "col.name",
                               S.text r 1 "col.data_type" ))
                    in
                    let bad_filter =
                      List.find_opt
                        (fun (k, _) -> not (List.mem_assoc k cols))
                        filters
                    in
                    match bad_filter with
                    | Some (k, _) ->
                        return
                          (`Error
                             (Printf.sprintf
                                "midden/query: unknown filter column %S on %S" k
                                proj_name))
                    | None -> (
                        let offset =
                          if cursor_s = "" then Some 0
                          else
                            match int_of_string_opt cursor_s with
                            | Some n when n >= 0 -> Some n
                            | _ -> None
                        in
                        match offset with
                        | None ->
                            return
                              (`Error
                                 "midden/query: cursor must be empty or a non-negative integer offset")
                        | Some offset ->
                            let col_select =
                              String.concat ", "
                                (List.map
                                   (fun (c, dt) ->
                                     let q = quote_ident c in
                                     if dt = "uuid" || dt = "timestamp with time zone"
                                     then q ^ "::text"
                                     else q)
                                   cols)
                            in
                            let where_parts, params =
                              List.fold_left
                                (fun (ws, ps) (k, v) ->
                                  let idx = List.length ps + 1 in
                                  let dt = List.assoc k cols in
                                  let param, cast =
                                    match dt with
                                    | "bigint" | "integer" | "smallint" ->
                                        (V.of_int64 (Int64.of_string v), "bigint")
                                    | "boolean" ->
                                        (V.of_bool (String.equal v "true"), "boolean")
                                    | "uuid" -> (V.of_string v, "uuid")
                                    | _ -> (V.of_string v, "text")
                                  in
                                  let clause =
                                    Printf.sprintf "%s = $%d::%s"
                                      (quote_ident k) idx cast
                                  in
                                  (clause :: ws, ps @ [ param ]))
                                ([], []) filters
                            in
                            let where_sql =
                              match List.rev where_parts with
                              | [] -> ""
                              | ws -> " WHERE " ^ String.concat " AND " ws
                            in
                            let order_sql =
                              if List.mem_assoc "created" cols && List.mem_assoc "entity" cols then
                                " ORDER BY \"created\" DESC, \"entity\" DESC"
                              else if List.mem_assoc "seq" cols then
                                " ORDER BY \"seq\" ASC"
                              else if List.mem_assoc "entity" cols then
                                " ORDER BY \"entity\" ASC"
                              else
                                match cols with
                                | (first_col, _) :: _ ->
                                    " ORDER BY " ^ quote_ident first_col ^ " ASC"
                                | [] -> ""
                            in
                            let lim_idx = List.length params + 1 in
                            let off_idx = lim_idx + 1 in
                            let sql =
                              Printf.sprintf
                                "SELECT %s FROM %s%s%s LIMIT $%d OFFSET $%d"
                                col_select (quote_ident proj_name) where_sql
                                order_sql lim_idx off_idx
                            in
                            let params =
                              params @ [ V.of_int limit; V.of_int offset ]
                            in
                            let rows = Db.q ~params pool sql in
                            let trees = List.map (tree_of_row cols) rows in
                            let result = Prims.tree_of_list trees in
                            if
                              String.length (Tuna.Canon.encode result)
                              > Prims.payload_cap
                            then
                              return
                                (`Error
                                   "midden/query: result exceeds the journal payload cap")
                            else return (`Ok result)))))
      | _ ->
          return
            (`Error
               "midden/query: projection, filter, cursor, and limit must be string trees"))
  | _ ->
      return
        (`Error "midden/query: args must be [projection filter cursor limit]")

let dispatch ~pool ~actor ~name ~args : answer =
  match name with
  | "midden/get" -> midden_get pool args
  | "midden/put" -> midden_put pool ~actor args
  | "midden/delete" -> midden_delete pool ~actor args
  | "midden/query" -> midden_query pool args
  | other -> return (`Error ("unknown prim: " ^ other))
