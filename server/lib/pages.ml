(* Tuna_server.Pages: the human surface (M8) — server-rendered pages +
   htmx fragments, session-cookie auth over the ONE credential store
   (identities.token_hash), and no-JS-degradable forms everywhere.

   Routes (session auth; login/logout are open):

      GET  /                    dashboard (session); anonymous / -> /welcome
     GET  /login   POST /login GET /logout
     GET  /frag/runs           runs-table fragment (htmx tick / no-JS view)
     GET  /programs/lookup?hash=…
     GET  /programs/:hash
     POST /programs/:hash/patch    CAS structural patch
     POST /programs/:hash/run       run with inputs (+grants)
      GET  /runs/:id
      GET  /runs/:id/journal       journal tick fragment
      GET  /runs/:id/trace         firing trace (borg/trace.borg)
      POST /runs/:id/verify         replay-verify button
       GET  /grants (read-only view; mint/revocation live on the /api/grants surface)
     GET  /grants/:id            one capability: its live lineage + the runs it authorized
     GET  /repl       POST /repl/eval
      GET  /view/:hash             value viewer (tree/bytes/program)
      GET  /todo                   the todo board (store-backed)
      POST /todo/add  /todo/:id/state  /todo/:id/del

   The JSON agent surface lives in Api (mounted alongside these routes
   by bin/main.ml). *)

module S = Tuna_store.Store
module L = Layout
module A = Auth

let routes pool =
  List.map
    (fun (method_, path, handler) ->
      (match method_ with
       | `Get -> Web.get path (A.require_auth pool handler)
       | `Post -> Web.post path (A.require_auth pool handler)))
      [ (`Get, "/frag/runs", Dashboard.frag_runs pool)
    ; (`Get, "/programs/:hash", Program.view pool)
    ; (`Post, "/programs/:hash/patch", Program.patch_post pool)
    ; (`Post, "/programs/:hash/run", Program.run_post pool)
    ; (`Get, "/runs/:id", Run_page.view pool)
      ; (`Get, "/runs/:id/journal", Run_page.journal_frag pool)
      ; (`Get, "/runs/:id/trace", Run_page.trace_page pool)
    ; (`Post, "/runs/:id/verify", Run_page.verify_post pool)
    ; (`Get, "/grants", Grants.view pool)
    ; (`Get, "/grants/:id", Grants.detail pool)
    ; (`Get, "/identities", Identities.view pool)
    ; (`Post, "/identities/mint", Identities.mint pool)
    ; (`Get, "/repl", Repl.view pool)
      ; (`Post, "/repl/eval", Repl.eval pool)
    ; (`Get, "/view/:hash", Value_view.view pool)
    ; (`Get, "/todo", Todo.view pool)
    ; (`Post, "/todo/add", Todo.add pool)
    ; (`Post, "/todo/:id/state", Todo.set_state pool)
    ; (`Post, "/todo/:id/del", Todo.del pool) ]

let open_routes pool =
  [ Web.get "/login" A.login_get
  ; Web.post "/login" (A.login_post pool)
  ; Web.get "/logout" (A.logout_get pool)
    (* pp-slice T3: the hard-public face (no session).  Reserved
       surfaces match before the route dispatcher (routes.borg law 2).
       2026-10-27 revamp: the anonymous front door (/), the prim
       registry, and the claims registry + minting guide. *)
    ; Web.get "/" (Public.home pool)
    ; Web.get "/welcome" (Public.welcome pool None)
    ; Web.get "/prims" (Public.prims pool None)
    ; Web.get "/claims" (Public.claims pool None)
    (* a claim is identified by its PATH, not by an opaque id: the path IS
       the assertion, and it contains dots and slashes-in-name only, so a
       trailing ** capture carries it verbatim (no encoding round-trip on a
       URL a human types).  Registered after the exact /claims so the
       registry still wins its own request. *)
    ; Web.get "/claims/**" (Public.claim_detail pool None)
    ; Web.get "/code" (Public.code pool None)
    ; Web.get "/agent.txt" (Public.agent_txt pool)
    ; Web.get "/.well-known/agent.json" (Public.agent_json pool)
  ; Web.get "/programs/lookup"
      (A.require_auth pool (fun _user req ->
           let hash =
             match Web.query req "hash" with
             | Some h -> String.trim h
             | None -> ""
           in
           Web.redirect req ("/programs/" ^ hash))) ]
