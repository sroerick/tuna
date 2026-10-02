(* Tuna_server.Agent: the public agent-discovery documents (pp-slice T3),
   shaped after pricklypear's image/lib/agent_discovery.ml.

     GET /.well-known/agent.json  — machine contract
     GET /agent.txt               — prose instructions

   Both are HARD-PUBLIC (no secrets, no auth): an agent that fetches
   them learns the auth model (bearer is the agent tier, browser
   sessions are the human tier), the endpoint census, and the
   verification/replay surfaces that make tuna's runs auditable.  The
   base URL is derived from the request's forwarded host headers,
   exactly PP's rule (never invented). *)

let base_url_of_headers (headers : (string * string) list) : string option =
  let header name =
    List.find_map
      (fun (k, v) ->
        if String.equal (String.lowercase_ascii k) name then Some (String.trim v)
        else None)
      headers
  in
  let host =
    match header "x-forwarded-host" with
    | Some h when h <> "" -> Some h
    | _ -> ( match header "host" with Some h when h <> "" -> Some h | _ -> None)
  in
  match host with
  | None -> None
  | Some host ->
      let proto =
        match header "x-forwarded-proto" with
        | Some p when p <> "" -> String.lowercase_ascii p
        | _ ->
            let loopback =
              host = "localhost"
              || String.starts_with ~prefix:"localhost:" host
              || String.starts_with ~prefix:"127." host
              || String.starts_with ~prefix:"[::1]" host
            in
            if loopback then "http" else "https"
      in
      Some (proto ^ "://" ^ host)

let philosophy =
  "The tree calculus is the API. Compile an s-expression to a program \
   hash, POST a run, and read the journal; every firing is a step and \
   every run is replayable from its journal."

let rules =
  [ "Authenticate with Authorization: Bearer <identity token> — the agent \
     tier.  Browser clients may instead carry the tuna_session cookie \
     (minted at /login); agents never need it."
  ; "POST /api/programs compiles an s-expression to a content-addressed \
     program hash.  POST /api/runs executes one with inputs and grants."
  ; "Grants are arguments, not ambient: a prim call must be covered by a \
     live grant row, checked by the host at the boundary.  Denial is a \
     journaled error result, never an exception."
  ; "A step is one triage-rule firing; the two wrapper applications are \
     application, not steps.  Step counts are an invariant of the \
     calculus, so they are stable across engines."
  ; "Replay determinism: GET /api/runs/:id/trace and POST \
     /api/runs/:id/verify re-execute program+inputs with prim calls \
     answered sequentially from the run's journal.  Replay never touches \
     the live host."
  ; "Fuel and size caps are ordinary results (fuel_exhausted / \
     size_exhausted), never timeouts; a wall-clock cap only finalizes a \
     run as deadline_exceeded at the operator boundary."
  ; "Publishing endpoints is a store write: POST /api/route/put writes a \
     route/<site-path> record (method, template/program hash, \
     content_type, grant_prefix) served data-first.  Do not hardcode \
     endpoints."
  ; "Discovery: /code is the read-only program+run browser; /.well-known/\
     agent.json is this contract; /agent.txt is prose."
  ]

let first_calls =
  [ "curl -sS -H 'Authorization: Bearer $TOKEN' -H 'Content-Type: application/json' \
     -d '{\"source\":\"(\\\\f. \\\\x. f x)\"}' $BASE/api/programs"
  ; "curl -sS -H 'Authorization: Bearer $TOKEN' -H 'Content-Type: application/json' \
     -d '{\"program\":\"<hash>\",\"inputs\":[]}' $BASE/api/runs"
  ; "curl -sS -H 'Authorization: Bearer $TOKEN' $BASE/api/runs/<id>/trace"
  ; "curl -sS $BASE/code"
  ]

let curl_example (base : string) : string =
  Printf.sprintf
    "curl -sS -H 'Authorization: Bearer $TOKEN' -H 'Content-Type: application/json' \\\n\
    \  -d '{\"source\":\"(\\\\f. \\\\x. f x)\"}' %s/api/programs"
    base

let agent_json ?(base_url : string option = None) () : Yojson.Basic.t =
  let base_field =
    match base_url with
    | Some b -> [ ("base_url", `String b) ]
    | None ->
        [ ( "base_url_note"
          , `String
              "Set base_url to the origin you fetched this document from \
               (scheme + host)." ) ]
  in
  let example_base =
    match base_url with Some b -> b | None -> "https://tuna.example"
  in
  `Assoc
    ( [ ("name", `String "tuna")
      ; ("version", `Int 1)
      ; ("philosophy", `String philosophy)
      ]
    @ base_field
    @ [ ( "auth"
        , `Assoc
            [ ("scheme", `String "Bearer")
            ; ( "realm"
              , `String "tuna identities table (bearer token, sha256 at rest)" )
            ; ( "how_to_get_credentials"
              , `String
                  "A human supplies the bearer token once (boot prints root's, \
                   or an admin mints one at /identities); store it mode 600.  \
                   There are no credentials in this document." )
            ; ( "session_cookie"
              , `String
                  "Browser clients may use cookie tuna_session (HttpOnly, \
                   SameSite=Lax) after POST /login; agents use Bearer." )
            ; ("login_url", `String "/login")
            ; ( "notes"
              , `List
                  [ `String "Bearer is the agent tier; sessions are the browser tier."
                  ; `String "No OAuth/JWT in v1."
                  ; `String "Never log or commit the token." ] )
            ] )
      ; ( "endpoints"
        , `Assoc
            [ ( "programs_post"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/programs")
                  ; ("auth", `String "required")
                  ; ( "request"
                    , `Assoc [ ("source", `String "(\\f. \\x. f x)") ] )
                  ; ( "response_ok"
                    , `Assoc
                        [ ("hash", `String "sha256-hex")
                        ; ("ternary", `String "…") ] ) ] )
            ; ( "program_get"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/api/programs/<hash>")
                  ; ("auth", `String "required") ] )
            ; ( "runs_post"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/runs")
                  ; ("auth", `String "required")
                  ; ( "request"
                    , `Assoc
                        [ ("program", `String "<hash>")
                        ; ("inputs", `List [])
                        ; ("grants", `List [])
                        ; ("fuel", `Int 10000)
                        ; ("size_cap", `Int 10000) ] ) ] )
            ; ( "runs_get"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/api/runs/<id>")
                  ; ("auth", `String "required") ] )
            ; ( "run_trace"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/api/runs/<id>/trace")
                  ; ("auth", `String "required")
                  ; ( "returns"
                    , `String "the firing trace (steps, prims, step counts)" ) ] )
            ; ( "run_verify"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/runs/<id>/verify")
                  ; ("auth", `String "required")
                  ; ( "returns"
                    , `String "replay verdict: verified | failed" ) ] )
            ; ( "journal"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/api/journals/<run_id>")
                  ; ("auth", `String "required")
                  ; ( "returns"
                    , `String
                        "the run's prim-call journal rows (the replay \
                         source of truth)" ) ] )
            ; ( "grants_post"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/grants")
                  ; ("auth", `String "required")
                  ; ( "request"
                    , `Assoc
                        [ ("prim", `String "*")
                        ; ("args_attenuation", `Assoc [])
                        ; ("path_prefix", `String "route/") ] ) ] )
            ; ( "identities_post"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/identities")
                  ; ("auth", `String "required (admin)")
                  ; ( "request"
                    , `Assoc
                        [ ("name", `String "agent-label")
                        ; ("password", `String "(optional) initial browser password") ] )
                  ; ( "response_ok"
                    , `Assoc
                        [ ("token", `String "returned ONCE, only sha256 stored")
                        ; ("is_admin", `Bool false) ] ) ] )
            ; ( "route_put"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/route/put")
                  ; ("auth", `String "required (covering grant)")
                  ; ( "request"
                    , `Assoc
                        [ ("path", `String "hello")
                        ; ( "record"
                          , `Assoc
                              [ ("method", `String "GET")
                              ; ("template", `String "<hash>")
                              ; ("content_type", `String "text/plain") ] ) ] ) ] )
            ; ( "health"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/health")
                  ; ("auth", `String "none") ] )
            ; ( "discovery_json"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/.well-known/agent.json")
                  ; ("auth", `String "none") ] )
            ; ( "discovery_text"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/agent.txt")
                  ; ("auth", `String "none") ] )
            ] )
      ; ( "bootstrap"
        , `Assoc
            [ ( "first_calls"
              , `List (List.map (fun s -> `String s) first_calls) )
            ; ("rules", `List (List.map (fun s -> `String s) rules)) ] )
      ; ("examples", `Assoc [ ("curl", `String (curl_example example_base)) ])
      ; ( "related"
        , `Assoc
            [ ("welcome", `String "/welcome")
            ; ("source_browser", `String "/code")
            ; ("source_tarball", `String "/src.tgz")
            ; ("book", `String "tuna.borg + borg/*.borg in the source tarball")
            ; ("login", `String "/login") ] )
      ] )

let agent_markdown ?(base_url : string option = None) () : string =
  let origin =
    match base_url with Some b -> b | None -> "https://tuna.example"
  in
  let first =
    String.concat "\n" (List.map (fun s -> "- `" ^ s ^ "`") first_calls)
  in
  let rule_lines =
    rules
    |> List.mapi (fun i r -> string_of_int (i + 1) ^ ". " ^ r)
    |> String.concat "\n"
  in
  String.concat "\n"
    [ "# Tuna agent instructions"
    ; ""
    ; philosophy
    ; ""
    ; "## Origin"
    ; ""
    ; "Base URL (this host): " ^ origin
    ; "Machine contract: GET " ^ origin ^ "/.well-known/agent.json"
    ; ""
    ; "## Auth"
    ; ""
    ; "Scheme: Authorization: Bearer <identity token> (the agent tier)."
    ; "Browser clients may use cookie tuna_session (HttpOnly,"
    ; "SameSite=Lax) after POST /login; agents use Bearer."
    ; ""
    ; "How to get credentials: ask the human once. Store them in a local"
    ; "secrets file (mode 600). This document never contains tokens."
    ; "An admin mints a non-admin identity + token at " ^ origin
    ; "/identities (or POST /api/identities)."
    ; ""
    ; "## Eval API"
    ; ""
    ; "Programs are compiled then run in two steps:"
    ; ""
    ; "  POST " ^ origin ^ "/api/programs   {\"source\":\"(\\\\f. \\\\x. f x)\"}"
    ; "    -> {\"hash\":\"…\",\"ternary\":\"…\"}"
    ; ""
    ; "  POST " ^ origin ^ "/api/runs       {\"program\":\"<hash>\",\"inputs\":[]}"
    ; "    -> {\"id\":\"…\",\"status\":\"normal\",\"step_count\":N,…}"
    ; ""
    ; "Authentication is required on /api/*; /health, /welcome, /code,"
    ; "/src.tgz, /agent.txt and /.well-known/agent.json are public."
    ; ""
    ; "## Verification surfaces"
    ; ""
    ; "- GET  " ^ origin ^ "/api/runs/<id>/trace    the firing trace"
    ; "- POST " ^ origin ^ "/api/runs/<id>/verify   replay verdict"
    ; "- GET  " ^ origin ^ "/api/journals/<run_id>  the prim-call journal"
    ; ""
    ; "Replay re-executes program+inputs with prim calls answered"
    ; "sequentially from the journal; it never touches the live host."
    ; ""
    ; "## First calls"
    ; ""
    ; first
    ; ""
    ; "## Rules"
    ; ""
    ; rule_lines
    ; ""
    ; "## curl example"
    ; ""
    ; "    " ^ curl_example origin
    ; ""
    ; "## Related public surfaces"
    ; ""
    ; "- Welcome: " ^ origin ^ "/welcome"
    ; "- Source browser: " ^ origin ^ "/code"
    ; "- Source tarball: " ^ origin ^ "/src.tgz"
    ; "- Health: " ^ origin ^ "/health"
    ; "- Interactive REPL (session required): " ^ origin ^ "/repl"
    ]
