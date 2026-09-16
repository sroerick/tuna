(* Tuna_server.Routes: M11 routes-as-paths (borg/routes.borg).

   A route is a tree_paths entry under the reserved route/ namespace
   whose value is a JSON record naming a template byte value and/or a
   program hash:

     {"method": "GET" | "POST" | "any",
      "template": <sha256 hex> | null, "program": <sha256 hex> | null,
      "content_type": <str>, "grant_prefix": <path> | null}

   Records are JSON BYTE values (byte_values, migration 0008) under
   route/<site-path>, written through the normal path ops (law 1): the
   ops chain audits every publish/delete, and rewind-as-fold covers
   routes because routes are paths.  Publishing an endpoint is a store
   write - journaled, versioned (CAS), grant-gated, rewritable - with
   no rebuild and no redeploy.

   Dispatch (law 2): reserved surfaces (Pages, /api/*, /health,
   /login, /logout) match first, ALWAYS - the router mounts this
   dispatch after every reserved matcher, and a publish whose site-path
   collides with a reserved prefix is a journaled denial.  The
   reserved-prefix list is pinned in routes.borg (transcribed here,
   never extended in code) and may only grow by book change.

   Law 3: template routes serve bytes to any caller, anonymous
   included; program routes execute the stored program as a
   daemon-attributed run (caller = the invoking identity when the
   request carries a valid bearer, else the daemon) whose ONLY
   capability is the record's own grant_prefix - minted fresh per run
   as a prim-"*" grant scoped to that prefix and live-checked by the
   run boundary like any other grant; the visitor carries nothing.

   Law 4: a program run must end in a value hash (+ the record's
   content_type); anything else answers 500 with the run id in the
   body.  Law 5: record-method mismatch and no-match answer plain 404,
   no login redirect.  When a record names both template and program,
   the program answers (dynamic is the more specific intent).

   Handlers here are Dream-free: they return plain response records /
   (code, json) tuples so tests exercise the routing table directly;
   Api.dispatch_route adapts them to HTTP. *)

open Lwt.Infix

module S = Tuna_store.Store
module J = Yojson.Basic

(* routes.borg law 2, pinned list *)
let reserved =
  [ "api"; "health"; "login"; "logout"; "frag"; "grants"; "programs"
  ; "runs"; "repl"; "value"; "route"; "static" ]

let reserved_site_path sp =
  match String.index_opt sp '/' with
  | Some i -> List.mem (String.sub sp 0 i) reserved
  | None -> List.mem sp reserved

let route_key site_path = "route/" ^ site_path

(* a response record; Api.dispatch_route turns it into an HTTP answer *)
type response = {
  code : int
; content_type : string
; body : string
; headers : (string * string) list
}

let respond ?(headers = []) ~code ~content_type body =
  { code; content_type; body; headers }

let err_json ?(headers = []) ?run_id code msg =
  let fields =
    [ ("error", `String msg) ]
    @
    match run_id with Some r -> [ ("run_id", `String r) ] | None -> []
  in
  { code
  ; content_type = "application/json"
  ; body = J.to_string (`Assoc fields)
  ; headers }

let plain_404 =
  { code = 404; content_type = "text/plain"; body = "not found"; headers = [] }

let is_hex64 s =
  String.length s = 64
  && String.for_all
       (fun c ->
         (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
       s

let member_opt k (j : J.t) : J.t option =
  match J.Util.member k j with `Null -> None | v -> Some v

let get_string_opt j k =
  match member_opt k j with Some (`String s) -> Some s | _ -> None

(* -- route records ------------------------------------------------------ *)

type record = {
  r_method : string  (* GET | POST | ANY *)
; r_template : string option
; r_program : string option
; r_content_type : string
; r_grant_prefix : string option
}

let parse_record (j : J.t) : (record, string) result =
  match j with
  | `Assoc _ -> (
      let method_ =
        String.uppercase_ascii (Option.value (get_string_opt j "method") ~default:"any")
      in
      let field_hash k =
        match get_string_opt j k with
        | None -> Ok None
        | Some h when is_hex64 h -> Ok (Some (String.lowercase_ascii h))
        | Some _ ->
            Error (Printf.sprintf "record %S must be a sha256 hex hash or null" k)
      in
      if method_ <> "GET" && method_ <> "POST" && method_ <> "ANY" then
        Error "record \"method\" must be GET, POST or any"
      else
        match (field_hash "template", field_hash "program") with
        | Error e, _ | _, Error e -> Error e
        | Ok template, Ok program -> (
            match get_string_opt j "content_type" with
            | None -> Error "record \"content_type\" is required"
            | Some ct when String.length ct = 0 ->
                Error "record \"content_type\" must be non-empty"
            | Some ct -> (
                let grant_prefix =
                  match get_string_opt j "grant_prefix" with
                  | None -> Ok None
                  | Some p -> (
                      match Tree_prims.validate_path "grant_prefix" p with
                      | Some e -> Error e
                      | None -> Ok (Some p))
                in
                match grant_prefix with
                | Error e -> Error e
                | Ok grant_prefix ->
                    if template = None && program = None then
                      Error "record must name a template or a program"
                    else
                      Ok
                        { r_method = method_
                        ; r_template = template
                        ; r_program = program
                        ; r_content_type = ct
                        ; r_grant_prefix = grant_prefix })))
  | _ -> Error "route record must be a JSON object"

(* record bytes -> JSON text: byte values carry the JSON directly; a
   tree value carries it as a string tree (probe-both-stores
   resolution, byte-values.borg law 1) *)
let record_text_of_probe = function
  | S.Bytes b -> Ok b
  | S.Tree ternary -> (
      match Tuna.Canon.of_string ternary with
      | Ok t -> (
          match Tuna.Cstr.decode t with
          | Some s -> Ok s
          | None -> Error "route record is not a string tree")
      | Error (off, msg) ->
          Error (Printf.sprintf "route record unparseable at %d: %s" off msg))

(* -- capability checks -------------------------------------------------- *)

(* routes.borg surface: grants on publish/delete follow the existing
   prefix-grant model (admin bypass) - the grant must COVER the route
   path; byte-values law 2's blanket any-grant rule does not extend
   here *)
let allowed pool ~caller_id ~caller_admin ~path =
  if caller_admin then Lwt.return true
  else S.has_covering_grant pool caller_id path

let journal_route_op pool ~actor ~op ~path ?value_hash ?prev_version ?version () =
    S.op_append pool ~op ~path ~value_hash ~prev_version ~version ~actor
    >>= fun _ -> Lwt.return ()

(* -- publish / delete: store writes, law 1 ------------------------------ *)

let publish pool ~caller_id ~caller_admin ~site_path ~(record : J.t)
    ~expected_version : (int * J.t) Lwt.t =
  let deny op_path code msg =
    journal_route_op pool ~actor:caller_id ~op:"put" ~path:op_path ()
    >>= fun () -> Lwt.return (code, `Assoc [ ("error", `String msg) ])
  in
  match Tree_prims.check_path "path" site_path with
  | Error e -> Lwt.return (400, `Assoc [ ("error", `String e) ])
  | Ok site_path -> (
      match parse_record record with
      | Error e -> Lwt.return (400, `Assoc [ ("error", `String e) ])
      | Ok _parsed -> (
          let key = route_key site_path in
          if reserved_site_path site_path then
            deny key 400
              (Printf.sprintf
                 "site path %S collides with a reserved prefix (routes.borg law 2)"
                 site_path)
          else
            allowed pool ~caller_id ~caller_admin ~path:key
            >>= function
            | false -> deny key 403 (Printf.sprintf "no covering grant for %s" key)
            | true -> (
                let bytes = J.to_string record in
                let hash = S.byte_hash bytes in
                S.byte_value_put pool ~hash ~bytes
                >>= fun () ->
                let ok_json version =
                  `Assoc
                    [ ("ok", `Bool true)
                    ; ("path", `String site_path)
                    ; ("version", `Int (Int64.to_int version))
                    ; ("value_hash", `String hash) ]
                in
                match expected_version with
                | None ->
                    S.path_put pool ~path:key ~value_hash:hash ~owner:caller_id
                    >>= fun newv ->
                    journal_route_op pool ~actor:caller_id ~op:"put" ~path:key
                      ~value_hash:hash
                      ?prev_version:(if newv = 1L then None else Some (Int64.pred newv))
                      ~version:newv ()
                    >>= fun () -> Lwt.return (201, ok_json newv)
                | Some n -> (
                    S.path_put_cas pool ~path:key ~value_hash:hash
                      ~owner:caller_id
                      ~expected_version:(Some (Int64.of_int n))
                      ~expected_hash:None
                    >>= function
                    | `Ok newv ->
                        journal_route_op pool ~actor:caller_id ~op:"cas" ~path:key
                          ~value_hash:hash ~prev_version:(Int64.of_int n)
                          ~version:newv ()
                        >>= fun () -> Lwt.return (201, ok_json newv)
                    | `Conflict ->
                        journal_route_op pool ~actor:caller_id ~op:"cas" ~path:key ()
                        >>= fun () ->
                        Lwt.return
                          ( 409
                          , `Assoc
                              [ ("error", `String "route put conflict: version mismatch")
                              ; ("path", `String site_path) ] )
                    | `Absent ->
                        journal_route_op pool ~actor:caller_id ~op:"cas" ~path:key ()
                        >>= fun () ->
                        Lwt.return
                          ( 404
                          , `Assoc
                              [ ( "error"
                                , `String (Printf.sprintf "no route at path %s" site_path)
                                ) ])))))

let delete pool ~caller_id ~caller_admin ~site_path ~expected_version :
    (int * J.t) Lwt.t =
  match Tree_prims.check_path "path" site_path with
  | Error e -> Lwt.return (400, `Assoc [ ("error", `String e) ])
  | Ok site_path -> (
      let key = route_key site_path in
      allowed pool ~caller_id ~caller_admin ~path:key
      >>= function
      | false ->
          journal_route_op pool ~actor:caller_id ~op:"delete" ~path:key ()
          >>= fun () ->
          Lwt.return
            ( 403
            , `Assoc
                [ ( "error"
                  , `String (Printf.sprintf "no covering grant for %s" key) ) ] )
      | true -> (
          S.path_delete pool ~path:key
            ~expected_version:(Option.map Int64.of_int expected_version)
          >>= function
          | `Ok v ->
              (* the delete row carries the version it removed; rewind
                 folds drop the path on exactly these rows *)
              journal_route_op pool ~actor:caller_id ~op:"delete" ~path:key
                ~prev_version:v ()
              >>= fun () ->
              Lwt.return
                ( 200
                , `Assoc
                    [ ("ok", `Bool true)
                    ; ("path", `String site_path)
                    ; ("deleted_version", `Int (Int64.to_int v)) ] )
          | `Absent ->
              journal_route_op pool ~actor:caller_id ~op:"delete" ~path:key ()
              >>= fun () ->
              Lwt.return
                ( 404
                , `Assoc
                    [ ( "error"
                      , `String (Printf.sprintf "no route at path %s" site_path) )
                    ] )
          | `Conflict ->
              journal_route_op pool ~actor:caller_id ~op:"delete" ~path:key ()
              >>= fun () ->
              Lwt.return
                ( 409
                , `Assoc
                    [ ( "error"
                      , `String "route delete conflict: version mismatch" ) ] )))

(* -- get / list: the routing table is data ------------------------------ *)

let get pool ~actor ~site_path : (int * J.t) Lwt.t =
  match Tree_prims.check_path "path" site_path with
  | Error e -> Lwt.return (400, `Assoc [ ("error", `String e) ])
  | Ok site_path -> (
      let key = route_key site_path in
      S.path_get pool ~path:key
      >>= function
      | None ->
          journal_route_op pool ~actor ~op:"get" ~path:key ()
          >>= fun () ->
          Lwt.return
            ( 404
            , `Assoc
                [ ( "error"
                  , `String (Printf.sprintf "no route at path %s" site_path) ) ] )
      | Some entry -> (
          S.probe_value pool entry.S.tp_value_hash
          >>= function
          | None ->
              Lwt.return
                ( 500
                , `Assoc
                    [ ( "error"
                      , `String
                          ("stored route record missing for hash "
                         ^ entry.S.tp_value_hash) ) ] )
          | Some probe -> (
              match record_text_of_probe probe with
              | Error m -> Lwt.return (500, `Assoc [ ("error", `String m) ])
              | Ok txt -> (
                  match J.from_string txt with
                  | exception _ ->
                      Lwt.return
                        ( 500
                        , `Assoc
                            [ ( "error"
                              , `String "stored route record is not valid JSON" )
                            ] )
                  | record ->
                      journal_route_op pool ~actor ~op:"get" ~path:key ()
                      >>= fun () ->
                      Lwt.return
                        ( 200
                        , `Assoc
                            [ ("path", `String site_path)
                            ; ("record", record)
                            ; ( "version"
                              , `Int (Int64.to_int entry.S.tp_version) )
                            ; ("value_hash", `String entry.S.tp_value_hash) ] )))))

let list pool ~actor ~prefix : (int * J.t) Lwt.t =
  match Tree_prims.validate_prefix "prefix" prefix with
  | Some e -> Lwt.return (400, `Assoc [ ("error", `String e) ])
  | None -> (
      let full = route_key prefix in
      S.path_list pool ~prefix:full ()
      >>= fun entries ->
      journal_route_op pool ~actor ~op:"list" ~path:full ()
      >>= fun () ->
      let site_path_of p = String.sub p 6 (String.length p - 6) in
      let rec build acc = function
        | [] -> Lwt.return (Ok (List.rev acc))
        | (e : S.path_entry) :: rest -> (
            S.probe_value pool e.S.tp_value_hash
            >>= function
            | None ->
                Lwt.return
                  (Error
                     (Printf.sprintf "stored route record missing for %s" e.S.tp_path))
            | Some probe -> (
                match record_text_of_probe probe with
                | Error m -> Lwt.return (Error m)
                | Ok txt -> (
                    match J.from_string txt with
                    | exception _ ->
                        Lwt.return
                          (Error
                             (Printf.sprintf "route record is not valid JSON: %s"
                                e.S.tp_path))
                    | record ->
                        build
                          ( `Assoc
                              [ ("path", `String (site_path_of e.S.tp_path))
                              ; ("record", record)
                              ; ("version", `Int (Int64.to_int e.S.tp_version))
                              ; ("value_hash", `String e.S.tp_value_hash) ]
                          :: acc )
                          rest)))
      in
      build [] entries
      >>= (function
            | Error m -> Lwt.return (500, `Assoc [ ("error", `String m) ])
            | Ok entries ->
                Lwt.return
                  (200, `Assoc [ ("entries", `List entries) ])))

(* -- dispatch (law 2: after every reserved matcher) --------------------- *)

(* the request context a program route receives as its single input: a
   JSON string tree {"method","path","query","body_hash","actor"} *)
let context_json ~meth ~site_path ~query ~body ~actor =
  `Assoc
    [ ("method", `String meth)
    ; ("path", `String site_path)
    ; ("query", (match query with Some q -> `String q | None -> `Null))
    ; ( "body_hash"
      , (match body with "" -> `Null | b -> `String (Tuna.Hash.hex_of_string b)) )
    ; ("actor", (match actor with Some a -> `String a | None -> `Null)) ]

let serve_template pool ~record ~site_path =
  match record.r_template with
  | None -> Lwt.return plain_404
  | Some hash -> (
      S.probe_value pool hash
      >>= function
      | None ->
          Lwt.return
            (err_json 500
               (Printf.sprintf "route %s: template missing for hash %s" site_path
                  hash))
      | Some probe ->
          let body =
            match probe with S.Bytes b -> b | S.Tree ternary -> ternary
          in
          Lwt.return
            (respond ~code:200 ~content_type:record.r_content_type body
               ~headers:[ ("X-Tuna-Route", site_path) ]))

let serve_program pool ~daemon ~record ~site_path ~meth ~query ~body ~actor =
  let program_hash = Option.get record.r_program in
  let run_caller = match actor with Some a -> a | None -> daemon in
  let grants =
    match record.r_grant_prefix with
    | None -> Lwt.return []
    | Some pfx ->
        S.mint_grant pool ~prim:"*" ~args_attenuation:"null"
          ~path_prefix:(Some pfx) ~caller:run_caller ~minted_by:(Some daemon) ()
        >>= fun g -> Lwt.return [ g.S.g_id ]
  in
  let ctx =
    Tuna.Cstr.encode
      (J.to_string (context_json ~meth ~site_path ~query ~body ~actor))
  in
  (* law 4: the run must end in a value hash served with the record's
     content_type; anything else answers 500 with the run id *)
  let finish (row : S.run) =
    match (row.S.r_status, row.S.r_result_ternary) with
    | S.Run_status.Normal, Some ternary -> (
        match Tuna.Canon.of_string ternary with
        | Error (off, msg) ->
            Lwt.return
              (err_json ~run_id:row.S.r_id 500
                 (Printf.sprintf "run %s result unparseable at %d: %s"
                    row.S.r_id off msg))
        | Ok t -> (
            match Tuna.Cstr.decode t with
            | Some h when is_hex64 h ->
                let h = String.lowercase_ascii h in
                S.probe_value pool h
                  >>= (function
                | None ->
                    Lwt.return
                      (err_json ~run_id:row.S.r_id 500
                         (Printf.sprintf "run %s ended in unknown value %s"
                            row.S.r_id h))
                | Some probe ->
                    let body =
                      match probe with S.Bytes b -> b | S.Tree t2 -> t2
                    in
                    Lwt.return
                      (respond ~code:200 ~content_type:record.r_content_type
                         body
                         ~headers:
                           [ ("X-Tuna-Route", site_path)
                           ; ("X-Tuna-Run", row.S.r_id) ]))
            | _ ->
                Lwt.return
                  (err_json ~run_id:row.S.r_id 500
                     (Printf.sprintf
                        "run %s did not end in a value hash (law 4)"
                        row.S.r_id))))
    | _ ->
        Lwt.return
          (err_json ~run_id:row.S.r_id 500
             (Printf.sprintf "run %s did not end in a value hash (law 4)"
                row.S.r_id))
  in
  grants
  >>= fun grant_ids ->
  Run.execute_run pool ~caller:run_caller ~program_hash ~input_trees:[ ctx ]
    ~grant_ids ~fuel:10_000 ~size_cap:10_000 ()
  >>= (function
        | Error (_, msg) ->
            (* no run row exists yet (unknown program hash, grant
               validation); debuggability beats silence *)
            Lwt.return
              (err_json 500
                 (Printf.sprintf "route %s: program %s did not run: %s"
                    site_path program_hash msg))
        | Ok (row, _js) -> finish row)

let dispatch pool ~daemon ~meth ~site_path ~query ~body ~actor : response Lwt.t
    =
  let meth = String.uppercase_ascii meth in
  if reserved_site_path site_path then Lwt.return plain_404
  else
    S.path_get pool ~path:(route_key site_path)
    >>= function
    | None -> Lwt.return plain_404
    | Some entry -> (
        S.probe_value pool entry.S.tp_value_hash
        >>= function
        | None -> Lwt.return plain_404
        | Some probe -> (
            match record_text_of_probe probe with
            | Error _ -> Lwt.return plain_404
            | Ok txt -> (
                match J.from_string txt with
                | exception _ -> Lwt.return plain_404
                | j -> (
                    match parse_record j with
                    | Error _ -> Lwt.return plain_404
                    | Ok record ->
                        if record.r_method <> "ANY" && record.r_method <> meth
                        then Lwt.return plain_404
                        else
                          match record.r_program with
                          | Some _ ->
                              serve_program pool ~daemon ~record ~site_path
                                ~meth ~query ~body ~actor
                          | None -> serve_template pool ~record ~site_path))))
