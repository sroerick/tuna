(* Tuna_server.Midden_eval: Midden M2 evaluator adapter (midden/borg/evaluators.borg
   + midden/borg/claims.borg).

   Binds the `tuna` evaluator to Midden's verification verdicts:
     - WELL-FORMED: checks a schema form parses & normalizes canonically,
       records a journaled Tuna run carrying the schema hash, and mints a
       Midden `claim` entity citing the run + verdict.
     - LENS-TOTAL: checks Midden.Schema.lens_total between two registered
       schema hashes, records a journaled Tuna run, and mints a `claim`.
     - PROJECTION: compares a materialized declarative projection table
       against a fresh replay from midden_commits at the current log chain
       hash, records a journaled Tuna run, and mints a `claim`. *)

open Tuna_store.Direct

module S = Tuna_store.Store
module Db = Tuna_store.Db
module TS = Tuna_store.Tuna_schemas
module MS = Midden_store.Store
module J = Yojson.Basic

let ( let* ) = Result.bind

(* Deterministic Sabra identity program: `(lambda (x) x)` compiled once
   and stored content-addressed.  Running it over the serialized verdict
   input tree produces a real replay-verifiable Tuna run whose result
   hash commits to the verdict payload. *)
let verdict_program_source = "(lambda (x) x)"

let ensure_verdict_program pool ~caller =
  try
    let cr = Tuna_compiler.Bracket.compile_source verdict_program_source in
    let ternary = Tuna.Canon.encode cr.Tuna_compiler.Bracket.tree in
    let hash = cr.Tuna_compiler.Bracket.hash_hex in
    ignore
      (S.upsert_program pool ~hash ~ternary
         ~source:(Some verdict_program_source) ~ir:None
         ~created_by:(Some caller));
    Ok hash
  with exn -> Error ("verdict program compile failed: " ^ Printexc.to_string exn)

let record_verdict_run pool ~caller ~(payload_json : J.t) :
    (string * string, string) result =
  let* prog_hash = ensure_verdict_program pool ~caller in
  let input_tree = Tuna.Cstr.encode (J.to_string payload_json) in
  match
    Run.execute_run pool ~caller ~program_hash:prog_hash
      ~input_trees:[ input_tree ] ~grant_ids:[] ~fuel:10_000 ~size_cap:10_000 ()
  with
  | Error (_, msg) -> Error ("verdict run failed: " ^ msg)
  | Ok (row, _) -> (
      match Replay.verify_and_record pool ~run_id:row.S.r_id () with
      | Replay.Verified _ ->
          Ok
            ( row.S.r_id,
              Option.value row.S.r_result_hash ~default:"" )
      | _ -> Error ("verdict run " ^ row.S.r_id ^ " failed replay verification"))

(* Parse canonical csexp bytes OR a plain s-expression string like
   `(schema foo (version 1) (fields (k :string)))` or `(projection ...)` into
   Midden.Csexp.t. Quoted strings keep their outer quotes as atoms so
   Midden.Declarative string literals parse cleanly. *)
let parse_plain_sexp (s : string) : (Midden.Csexp.t, string) result =
  let n = String.length s in
  let rec skip_ws i =
    if i >= n then i
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> skip_ws (i + 1)
      | ';' ->
          let rec eol j =
            if j >= n || s.[j] = '\n' then skip_ws (j + 1) else eol (j + 1)
          in
          eol (i + 1)
      | _ -> i
  in
  let rec parse_one i =
    let i = skip_ws i in
    if i >= n then Error "unexpected end of s-expression"
    else
      match s.[i] with
      | '(' -> parse_list (i + 1) []
      | ')' -> Error (Printf.sprintf "unexpected ')' at %d" i)
      | '"' ->
          let buf = Buffer.create 16 in
          Buffer.add_char buf '"';
          let rec scan j =
            if j >= n then Error "unterminated string literal"
            else if s.[j] = '"' then (
              Buffer.add_char buf '"';
              Ok (Midden.Csexp.Atom (Buffer.contents buf), j + 1))
            else (
              Buffer.add_char buf s.[j];
              scan (j + 1))
          in
          scan (i + 1)
      | _ ->
          let rec scan j =
            if j >= n then j
            else
              match s.[j] with
              | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' -> j
              | _ -> scan (j + 1)
          in
          let j = scan i in
          Ok (Midden.Csexp.Atom (String.sub s i (j - i)), j)
  and parse_list i acc =
    let i = skip_ws i in
    if i >= n then Error "unterminated '('"
    else if s.[i] = ')' then Ok (Midden.Csexp.List (List.rev acc), i + 1)
    else
      match parse_one i with
      | Error _ as e -> e
      | Ok (v, next) -> parse_list next (v :: acc)
  in
  match parse_one 0 with
  | Error _ as e -> e
  | Ok (v, next) ->
      let next = skip_ws next in
      if next = n then Ok v
      else Error (Printf.sprintf "trailing tokens at %d" next)

let csexp_of_sexp_string (s : string) : (Midden.Csexp.t, string) result =
  match Midden.Csexp.decode s with
  | Ok v -> Ok v
  | Error _ -> parse_plain_sexp s

let register_schema pool ~form_str : (int * J.t) =
  TS.ensure_all pool;
  match csexp_of_sexp_string form_str with
  | Error msg -> return (400, `Assoc [ ("error", `String msg) ])
  | Ok form -> (
      match MS.register_schema (Db.midden pool) form with
      | Error e ->
          return (400, `Assoc [ ("error", `String (MS.string_of_error e)) ])
      | Ok reg ->
          return
            ( 201,
              `Assoc
                [ ("name", `String reg.MS.name);
                  ("version", `Int reg.MS.version);
                  ("hash", `String reg.MS.hash) ] ))

let define_projection pool ~form_str : (int * J.t) =
  TS.ensure_all pool;
  match csexp_of_sexp_string form_str with
  | Error msg -> return (400, `Assoc [ ("error", `String msg) ])
  | Ok form -> (
      match MS.define_projection (Db.midden pool) form with
      | Error e ->
          return (400, `Assoc [ ("error", `String (MS.string_of_error e)) ])
      | Ok info ->
          return
            ( 201,
              `Assoc
                [ ("name", `String info.MS.name);
                  ("form_hash", `String info.MS.form_hash);
                  ("schema_name", `String info.MS.schema_name);
                  ("schema_version", `Int info.MS.schema_version);
                  ("cursor_seq", `Int info.MS.cursor_seq) ] ))

let list_schemas pool : (int * J.t) =
  TS.ensure_all pool;
  let rows =
    Db.q pool
      "SELECT name, version, hash FROM midden_schema_refs ORDER BY name"
  in
  let items =
    List.map
      (fun r ->
        `Assoc
          [ ("name", `String (S.text r 0 "midden_schema_refs.name"));
            ("version", `Int (S.int r 1 "midden_schema_refs.version"));
            ("hash", `String (S.text r 2 "midden_schema_refs.hash")) ])
      rows
  in
  return (200, `Assoc [ ("schemas", `List items) ])

let current_log_head pool =
  match
    Db.q pool
      "SELECT seq, chain FROM midden_commits WHERE namespace = 'local' \
       ORDER BY seq DESC LIMIT 1"
  with
  | [ r ] -> (S.int r 0 "seq", S.text r 1 "chain")
  | _ -> (0, String.make 64 '0')

let fetch_schema_by_hash pool hash =
  match
    Db.q ~params:[ Pgx.Value.of_string hash ] pool
      "SELECT v.bytes FROM midden_schema_versions sv \
       JOIN midden_values v ON v.hash = sv.hash WHERE sv.hash = $1 LIMIT 1"
  with
  | [ r ] -> (
      let bytes = S.binary r 0 "bytes" in
      match Midden.Csexp.decode bytes with
      | Error e -> Error (Midden.Csexp.string_of_error e)
      | Ok form -> (
          match Midden.Schema.of_csexp form with
          | Error e -> Error (Midden.Schema.string_of_error e)
          | Ok sc -> Ok sc))
  | _ -> Error ("schema hash not registered: " ^ hash)

let verify_well_formed pool ~caller ~author ~schema_hash : (int * J.t) =
  TS.ensure_all pool;
  let verdict, detail =
    match fetch_schema_by_hash pool schema_hash with
    | Ok sc ->
        let canon_hash = Midden.Borge_binding.schema_hash sc in
        if String.equal canon_hash schema_hash then
          ("pass", Printf.sprintf "schema %s v%d is canonical and well-formed" sc.name sc.version)
        else
          ("fail", Printf.sprintf "canonical hash %s differs from %s" canon_hash schema_hash)
    | Error msg -> ("fail", msg)
  in
  let payload =
    `Assoc
      [ ("claim_kind", `String "WELL-FORMED");
        ("evaluator", `String "tuna");
        ("schema_hash", `String schema_hash);
        ("verdict", `String verdict);
        ("detail", `String detail) ]
  in
  match record_verdict_run pool ~caller ~payload_json:payload with
  | Error msg -> return (500, `Assoc [ ("error", `String msg) ])
  | Ok (run_id, result_hash) ->
      let full_body =
        `Assoc
          [ ("claim_kind", `String "WELL-FORMED");
            ("evaluator", `String "tuna");
            ("schema_hash", `String schema_hash);
            ("verdict", `String verdict);
            ("detail", `String detail);
            ("run_id", `String run_id);
            ("result_hash", `String result_hash) ]
      in
      let path =
        Printf.sprintf "midden/claim/well-formed-%s"
          (String.sub schema_hash 0 (min 12 (String.length schema_hash)))
      in
      let entity, body_hash, seq =
        S.put_claim pool ~path ~kind:"WELL-FORMED" ~subject:schema_hash ~verdict
          ~body_bytes:(J.to_string full_body) ~author
      in
      return
        ( 201,
          `Assoc
            [ ("entity", `String entity);
              ("path", `String path);
              ("kind", `String "WELL-FORMED");
              ("subject", `String schema_hash);
              ("verdict", `String verdict);
              ("detail", `String detail);
              ("run_id", `String run_id);
              ("body_hash", `String body_hash);
              ("seq", `Int seq) ] )

let verify_lens_total pool ~caller ~author ~from_hash ~to_hash : (int * J.t) =
  TS.ensure_all pool;
  let verdict, detail =
    match (fetch_schema_by_hash pool from_hash, fetch_schema_by_hash pool to_hash) with
    | Ok src, Ok dst -> (
        match Midden.Schema.lens_total src dst with
        | Ok () ->
            ( "pass",
              Printf.sprintf "lens %s v%d -> v%d is total" dst.name src.version
                dst.version )
        | Error e -> ("fail", Midden.Schema.string_of_error e))
    | Error m, _ | _, Error m -> ("fail", m)
  in
  let subject = from_hash ^ "->" ^ to_hash in
  let payload =
    `Assoc
      [ ("claim_kind", `String "LENS-TOTAL");
        ("evaluator", `String "tuna");
        ("from_hash", `String from_hash);
        ("to_hash", `String to_hash);
        ("verdict", `String verdict);
        ("detail", `String detail) ]
  in
  match record_verdict_run pool ~caller ~payload_json:payload with
  | Error msg -> return (500, `Assoc [ ("error", `String msg) ])
  | Ok (run_id, result_hash) ->
      let full_body =
        `Assoc
          [ ("claim_kind", `String "LENS-TOTAL");
            ("evaluator", `String "tuna");
            ("from_hash", `String from_hash);
            ("to_hash", `String to_hash);
            ("verdict", `String verdict);
            ("detail", `String detail);
            ("run_id", `String run_id);
            ("result_hash", `String result_hash) ]
      in
      let path =
        Printf.sprintf "midden/claim/lens-total-%s-%s"
          (String.sub from_hash 0 (min 8 (String.length from_hash)))
          (String.sub to_hash 0 (min 8 (String.length to_hash)))
      in
      let entity, body_hash, seq =
        S.put_claim pool ~path ~kind:"LENS-TOTAL" ~subject ~verdict
          ~body_bytes:(J.to_string full_body) ~author
      in
      return
        ( 201,
          `Assoc
            [ ("entity", `String entity);
              ("path", `String path);
              ("kind", `String "LENS-TOTAL");
              ("subject", `String subject);
              ("verdict", `String verdict);
              ("detail", `String detail);
              ("run_id", `String run_id);
              ("body_hash", `String body_hash);
              ("seq", `Int seq) ] )

let snapshot_table pool table_name =
  let q =
    "\"" ^ String.concat "\"\"" (String.split_on_char '"' table_name) ^ "\""
  in
  let rows =
    Db.q pool (Printf.sprintf "SELECT row_to_json(t)::text FROM %s t" q)
    |> List.map (fun r -> S.text r 0 "row_json")
    |> List.sort String.compare
  in
  Tuna.Hash.hex_of_string (String.concat "\n" rows)

let verify_projection pool ~caller ~author ~projection_name : (int * J.t) =
  TS.ensure_all pool;
  match
    Db.q ~params:[ Pgx.Value.of_string projection_name ] pool
      "SELECT form_hash FROM midden_projections WHERE name = $1"
  with
  | [] ->
      return
        ( 404,
          `Assoc
            [ ("error", `String ("unknown projection: " ^ projection_name)) ] )
  | [ r ] -> (
      let form_hash = S.text r 0 "midden_projections.form_hash" in
      let before_digest = snapshot_table pool projection_name in
      let log_seq, log_chain = current_log_head pool in
      let verdict, detail =
        match MS.rebuild_projection (Db.midden pool) ~name:projection_name with
        | Error e -> ("fail", MS.string_of_error e)
        | Ok () ->
            let after_digest = snapshot_table pool projection_name in
            if String.equal before_digest after_digest then
              ( "pass",
                Printf.sprintf
                  "projection %s matches replay at seq %d (digest %s)"
                  projection_name log_seq
                  (String.sub after_digest 0 12) )
            else
              ( "fail",
                Printf.sprintf "projection %s drifted: live %s != replay %s"
                  projection_name before_digest after_digest )
      in
      let payload =
        `Assoc
          [ ("claim_kind", `String "PROJECTION");
            ("evaluator", `String "tuna");
            ("projection", `String projection_name);
            ("form_hash", `String form_hash);
            ("log_seq", `Int log_seq);
            ("log_chain", `String log_chain);
            ("verdict", `String verdict);
            ("detail", `String detail) ]
      in
      match record_verdict_run pool ~caller ~payload_json:payload with
      | Error msg -> return (500, `Assoc [ ("error", `String msg) ])
      | Ok (run_id, result_hash) ->
          let full_body =
            `Assoc
              [ ("claim_kind", `String "PROJECTION");
                ("evaluator", `String "tuna");
                ("projection", `String projection_name);
                ("form_hash", `String form_hash);
                ("log_seq", `Int log_seq);
                ("log_chain", `String log_chain);
                ("verdict", `String verdict);
                ("detail", `String detail);
                ("run_id", `String run_id);
                ("result_hash", `String result_hash) ]
          in
          let path =
            Printf.sprintf "midden/claim/projection-%s-%s" projection_name
              (String.sub log_chain 0 (min 12 (String.length log_chain)))
          in
          let entity, body_hash, seq =
            S.put_claim pool ~path ~kind:"PROJECTION" ~subject:projection_name
              ~verdict ~body_bytes:(J.to_string full_body) ~author
          in
          return
            ( 201,
              `Assoc
                [ ("entity", `String entity);
                  ("path", `String path);
                  ("kind", `String "PROJECTION");
                  ("subject", `String projection_name);
                  ("form_hash", `String form_hash);
                  ("log_seq", `Int log_seq);
                  ("log_chain", `String log_chain);
                  ("verdict", `String verdict);
                  ("detail", `String detail);
                  ("run_id", `String run_id);
                  ("body_hash", `String body_hash);
                  ("seq", `Int seq) ] ))
  | _ -> return (500, `Assoc [ ("error", `String "duplicate projection rows") ])

let list_claims pool ?kind () : (int * J.t) =
  TS.ensure_all pool;
  let rows = S.list_claims pool ?kind () in
  let items =
    List.map
      (fun (c : S.claim_row) ->
        `Assoc
          [ ("entity", `String c.cl_entity);
            ("path", `String c.cl_path);
            ("kind", `String c.cl_kind);
            ("subject", `String c.cl_subject);
            ("verdict", `String c.cl_verdict);
            ("body_hash", `String c.cl_body_hash);
            ("author", `String c.cl_author);
            ("at", `String c.cl_at) ])
      rows
  in
  return (200, `Assoc [ ("claims", `List items) ])
