(* Tuna_server.Pages: the human surface (M8) — server-rendered pages +
   htmx fragments, session-cookie auth over the ONE credential store
   (identities.token_hash), and no-JS-degradable forms everywhere.

   Routes (session auth; login/logout are open):

     GET  /                    dashboard (runs table, auto-refresh)
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
     GET  /grants
     POST /grants/mint
     POST /grants/:id/revoke
     GET  /repl       POST /repl/eval

   The JSON agent surface lives in Api (mounted alongside these routes
   by bin/main.ml). *)

module S = Tuna_store.Store
module L = Layout
module A = Auth

let routes pool =
  List.map
    (fun (method_, path, handler) ->
      (match method_ with
       | `Get -> Dream.get path (A.require_auth pool handler)
       | `Post -> Dream.post path (A.require_auth pool handler)))
    [ (`Get, "/", Dashboard.view pool)
    ; (`Get, "/frag/runs", Dashboard.frag_runs pool)
    ; (`Get, "/programs/:hash", Program.view pool)
    ; (`Post, "/programs/:hash/patch", Program.patch_post pool)
    ; (`Post, "/programs/:hash/run", Program.run_post pool)
    ; (`Get, "/runs/:id", Run_page.view pool)
      ; (`Get, "/runs/:id/journal", Run_page.journal_frag pool)
      ; (`Get, "/runs/:id/trace", Run_page.trace_page pool)
    ; (`Post, "/runs/:id/verify", Run_page.verify_post pool)
    ; (`Get, "/grants", Grants.view pool)
    ; (`Post, "/grants/mint", Grants.mint pool)
    ; (`Post, "/grants/:id/revoke", Grants.revoke pool)
    ; (`Get, "/repl", Repl.view pool)
    ; (`Post, "/repl/eval", Repl.eval pool) ]

let open_routes pool =
  [ Dream.get "/login" A.login_get
  ; Dream.post "/login" (A.login_post pool)
  ; Dream.get "/logout" A.logout_get
  ; Dream.get "/programs/lookup"
      (A.require_auth pool (fun _user req ->
           let hash =
             match Dream.query req "hash" with
             | Some h -> String.trim h
             | None -> ""
           in
           Dream.redirect req ("/programs/" ^ hash))) ]
