(* Agent contract tests: the examples published at /agent.txt and
   /.well-known/agent.json (server/lib/agent.ml) must work as written.
   The documents are hand-written strings, so these pin the facts an
   agent's first calls depend on: the example source compiles and runs,
   the run request names the field post_run reads, every documented
   route exists, and shell examples expand $TOKEN. *)

module A = Tuna_server.Agent

let json = A.agent_json ~base_url:(Some "https://tuna.example") ()
let md = A.agent_markdown ~base_url:(Some "https://tuna.example") ()

let member k = function
  | `Assoc kv -> ( try List.assoc k kv with Not_found -> `Null)
  | _ -> `Null

let endpoint name = json |> member "endpoints" |> member name

let contains sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

let example_source_runs () =
  match endpoint "programs_post" |> member "request" |> member "source" with
  | `String src -> (
      let art = Tuna_compiler.Bracket.compile_source src in
      let arg s =
        match Tuna.Canon.of_string s with
        | Ok t -> t
        | Error _ -> Alcotest.failf "bad ternary %s" s
      in
      match
        Tuna_interp.Eval.eval ~fuel:1000 ~size_cap:1000 ~program:art.tree
          [ arg "0"; arg "10" ]
      with
      | Tuna_interp.Eval.Normal (t, _) ->
          (* the markdown's documented answer for inputs ["0","10"] *)
          Alcotest.(check string) "result" "110" (Tuna.Canon.encode t)
      | _ -> Alcotest.fail "documented example did not reach normal form")
  | _ -> Alcotest.fail "programs_post.request.source missing"

let run_request_field () =
  let req = endpoint "runs_post" |> member "request" in
  Alcotest.(check bool) "runs_post names program_hash" true
    (member "program_hash" req <> `Null);
  Alcotest.(check bool) "markdown run example names program_hash" true
    (contains "{\"program_hash\":" md)

(* every endpoint agent.json advertises must be in the router's table
   (the same lists Api.router mounts); the pool connects lazily, so
   building the table touches no database *)
let route_table () =
  let pool = Tuna_store.Db.init (Tuna_store.Db.config_from_env ()) in
  let module W = Tuna_server.Web in
  (W.get "/health" (fun _ -> assert false)
   :: Tuna_server.Api.api_routes pool
   @ Tuna_server.Pages.open_routes pool
   @ Tuna_server.Pages.routes pool)
  |> List.filter_map (function
       | W.RMeth m, p, _ -> Some (m, p)
       | W.RAny, _, _ -> None)

let documented_routes_exist () =
  let routes = route_table () in
  let norm p =
    String.split_on_char '/' p
    |> List.map (fun seg ->
           if String.length seg > 0 && (seg.[0] = '<' || seg.[0] = ':') then ":"
           else seg)
    |> String.concat "/"
  in
  let known = List.map (fun (m, p) -> (m, norm p)) routes in
  match json |> member "endpoints" with
  | `Assoc eps ->
      List.iter
        (fun (name, ep) ->
          match (member "method" ep, member "path" ep) with
          | `String m, `String p ->
              Alcotest.(check bool)
                (Printf.sprintf "%s %s %s is routed" name m p)
                true
                (List.mem (m, norm p) known)
          | _ -> ())
        eps
  | _ -> Alcotest.fail "endpoints missing"

let token_expands () =
  Alcotest.(check bool) "no single-quoted $TOKEN" false
    (contains "'Authorization: Bearer $TOKEN'" md)

let () =
  Alcotest.run "agent-contract"
    [ ( "examples"
      , [ Alcotest.test_case "documented source compiles and runs" `Quick
            example_source_runs
        ; Alcotest.test_case "run request field" `Quick run_request_field
        ; Alcotest.test_case "documented routes exist" `Quick
            documented_routes_exist
        ; Alcotest.test_case "shell examples expand $TOKEN" `Quick
            token_expands ] ) ]
