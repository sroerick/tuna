(* Tuna_server.Pages.Grants: the read-only human view of the grants
   table (session auth, the same seam as every other page; anonymous
   requests are sent to login by Pages.require_auth).  It reads the
   same Store accessor the admin JSON list uses (Store.list_grants).
   Minting and revocation stay API-first: claim paths and grants are
   the machine surface - POST /api/grants, /api/grants/:id/attenuate
   and /api/grants/:id/revoke. *)

open Tuna_store.Direct

module S = Tuna_store.Store
module L = Layout

let view pool user _req =
  S.list_grants pool ~author:None ()
  >>= fun gs ->
  let row (g : S.grant) =
    let status =
      match g.S.g_revoked_at with
      | Some at -> L.badge "bad" ("revoked " ^ at)
      | None -> L.badge "ok" "live"
    in
    let scope =
      match g.S.g_path_prefix with
      | Some p -> Printf.sprintf "<code>%s</code>" (L.esc p)
      | None -> {|<span class="muted">unscoped</span>|}
    in
    let granted_by =
      match g.S.g_minted_by with
      | Some m ->
        Printf.sprintf "<code title=\"%s\">%s</code>" (L.esc m)
          (L.esc (L.short_hash m))
      | None -> {|<span class="muted">-</span>|}
    in
    Printf.sprintf
      {|<tr id="grant-%s"><td><a href="/grants/%s"><code title="%s">%s</code></a></td><td><code title="%s">%s</code></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><code>%s</code></td></tr>|}
      (L.esc g.S.g_id)
      (L.esc g.S.g_id)
      (L.esc g.S.g_id)
      (L.esc (L.short_hash g.S.g_id))
      (L.esc g.S.g_caller)
      (L.esc (L.short_hash g.S.g_caller))
      (L.esc g.S.g_prim)
      scope status granted_by
      (L.esc g.S.g_created_at)
  in
  let rows =
    if gs = [] then
      {|<tr><td colspan="7" class="muted">no grants minted yet</td></tr>|}
    else String.concat "" (List.map row gs)
  in
  L.page ~user ~title:"tuna — grants"
    (Printf.sprintf
       {|<h2>grants</h2>
<section>
<table>
<tr><th>grant</th><th>identity</th><th>prim</th><th>path_prefix</th><th>status</th><th>granted_by</th><th>created_at</th></tr>
%s
</table>
</section>
<p class="muted">read-only view.  minting and revocation stay API-first:
claim paths and grants are the machine surface (<code>POST /api/grants</code>,
<code>POST /api/grants/:id/attenuate</code>, <code>POST /api/grants/:id/revoke</code>).</p>|}
       rows)

(* -- grant detail: what a capability actually authorized -------------

   The list page answers "what grants exist".  The question a capability
   model has to answer is the other direction: "what did THIS grant
   permit?", because a grant row is a promise and the journal is the
   record of whether the promise was ever used.  grants.borg states the
   headline demo — the effect record IS the audit record — and
   verify-6-audit.sh proves the query over psql.  This page is the same
   query for a human: every run the grant authorized, which prims it was
   exercised at, and the exact callsite rows underneath.

   DENIALS ARE NOT EXERCISES.  A denial is journaled with grant_id NULL
   (the boundary records the attempt before crediting any grant), so this
   page never claims a grant authorized something it refused.  Runs that
   tried and were refused are a different question, and are not shown as
   audit rows here. *)

(* the live-lineage walk, rendered: a grant's authority dies if ANY
   ancestor is revoked, so the chain is the honest answer to "why is
   this grant still live" (grants.grant-token, lineage death) *)
let lineage_html pool g =
  S.grant_lineage pool ~max_hops:64 g.S.g_id
  >>= fun ancestors ->
  let parent =
    match g.S.g_parent_grant with
    | None -> {|<span class="muted">none (root of its lineage)</span>|}
    | Some p -> Printf.sprintf {|<a href="/grants/%s"><code>%s</code></a>|} (L.esc p) (L.esc (L.short_hash p))
  in
  let anc_rows =
    if ancestors = [] then {|<tr><td colspan="4" class="muted">no ancestors</td></tr>|}
    else
      String.concat ""
        (List.map
           (fun (a : S.grant) ->
             Printf.sprintf
               {|<tr><td><a href="/grants/%s"><code>%s</code></a></td><td><code>%s</code></td><td>%s</td><td>%s</td></tr>|}
               (L.esc a.S.g_id)
               (L.esc (L.short_hash a.S.g_id))
               (L.esc a.S.g_prim)
               (match a.S.g_path_prefix with
                | Some p -> Printf.sprintf "<code>%s</code>" (L.esc p)
                | None -> {|<span class="muted">unscoped</span>|})
               (match a.S.g_revoked_at with
                | Some at -> L.badge "bad" ("revoked " ^ at)
                | None -> L.badge "ok" "live"))
           ancestors)
  in
  return
    (parent, ancestors, anc_rows)

let detail pool user req =
  let id = Web.param req "id" in
  S.fetch_grant pool id
  >>= function
  | None -> L.not_found ~user "unknown grant id"
  | Some g ->
      lineage_html pool g
      >>= fun (parent_html, ancestors, anc_rows) ->
      S.list_grant_exercises pool ~limit:200 g.S.g_id
      >>= fun exercises ->
      (* the live verdict has to weigh the whole chain, not the row *)
      let self_revoked = g.S.g_revoked_at <> None in
      let chain_dead =
        List.exists (fun (a : S.grant) -> a.S.g_revoked_at <> None) ancestors
      in
      let verdict =
        if self_revoked then L.badge "bad" "revoked (this row)"
        else if chain_dead then
          L.badge "bad" "dead (an ancestor is revoked)"
        else L.badge "ok" "live"
      in
      let scope_html =
        match g.S.g_path_prefix with
        | Some p -> Printf.sprintf "<code>%s</code>" (L.esc p)
        | None ->
            {|<span class="muted">unscoped — authorizes pathless calls only</span>|}
      in
      let total_calls =
        List.fold_left
          (fun acc (e : S.grant_exercise) -> Int64.add acc e.S.ge_calls)
          0L exercises
      in
      let run_rows =
        if exercises = [] then
          {|<tr><td colspan="6" class="muted">no run has ever exercised this grant — minted but unused</td></tr>|}
        else
          String.concat ""
            (List.map
               (fun (e : S.grant_exercise) ->
                 Printf.sprintf
                   {|<tr><td>%s</td><td><code>%s</code></td><td>%s</td><td>%Ld</td><td><code>%s</code></td><td>%s</td></tr>|}
                   (L.link_run e.S.ge_run_id)
                   (L.esc (String.concat ", " e.S.ge_prim))
                   (L.status_badge e.S.ge_run_status)
                   e.S.ge_calls
                   (L.esc ("/" ^ e.S.ge_first_callsite))
                   (L.esc (Option.value e.S.ge_run_created ~default:"—")))
               exercises)
      in
      (* per-run callsite detail: the whole audit, rolled into one block
         so a reader can see every authorized call without leaving *)
      let rec call_blocks acc = function
        | [] -> return (List.rev acc)
        | (e : S.grant_exercise) :: rest ->
            S.list_grant_calls pool ~grant_id:g.S.g_id ~run_id:e.S.ge_run_id
            >>= fun calls ->
            let rows =
              String.concat ""
                (List.map
                   (fun (seq, cs, prim, contract, err, rh) ->
                     Printf.sprintf
                       {|<tr><td>%d</td><td><code>/%s</code></td><td><code>%s</code></td><td><code>%s</code></td><td>%s</td><td>%s</td></tr>|}
                       seq (L.esc cs) (L.esc prim) (L.esc contract)
                       (match err with
                        | Some m -> Printf.sprintf {|<span class="err">%s</span>|} (L.esc m)
                        | None -> {|<span class="muted">ok</span>|})
                       (match rh with
                        | Some h -> Printf.sprintf "<code>%s…</code>" (L.esc (L.short_hash h))
                        | None -> "—"))
                   calls)
            in
            call_blocks
              (Printf.sprintf
                 {|<details><summary>%s — %d authorized call(s)</summary>
<table>
<tr><th>seq</th><th>callsite</th><th>prim</th><th>contract version</th><th>result</th><th>result hash</th></tr>
%s
</table></details>|}
                 (L.esc (L.short_hash e.S.ge_run_id))
                 (List.length calls) rows
              :: acc)
              rest
      in
      call_blocks [] exercises
      >>= fun blocks ->
      L.page ~user ~title:"tuna — grant"
        (Printf.sprintf
           {|<h2>grant <code>%s…</code></h2>
<p>%s</p>
<section><h3>the capability</h3>
<table>
<tr><th>id</th><td><code>%s</code></td></tr>
<tr><th>prim</th><td><code>%s</code></td></tr>
<tr><th>path scope</th><td>%s</td></tr>
<tr><th>attenuation</th><td><code>%s</code></td></tr>
<tr><th>caller</th><td><code>%s</code></td></tr>
<tr><th>minted by</th><td><code>%s</code></td></tr>
<tr><th>parent grant</th><td>%s</td></tr>
<tr><th>state</th><td>%s</td></tr>
</table>
<p class="muted">a grant is an opaque tree argument to the calculus: the
evaluator never resolves a name, and the host checks the row LIVE at
every prim boundary.  revocation is immediate and cuts the whole
descendant subtree, because the boundary walks the parent chain.</p>
</section>
<section><h3>lineage (%d ancestor(s))</h3>
<table>
<tr><th>grant</th><th>prim</th><th>scope</th><th>state</th></tr>
%s
</table>
<p class="muted">authority only ever narrows going down: an attenuation
admits no more than its parent, and any revoked ancestor kills the
subtree's future.</p></section>
<section><h3>audit — what this grant authorized (%d run(s), %Ld call(s))</h3>
<table>
<tr><th>run</th><th>prims exercised</th><th>status</th><th>calls</th><th>first callsite</th><th>when</th></tr>
%s
</table>
%s
<p class="muted">this is the audit query over journal rows joined to run
rows — no log grep, no text heuristic.  a run that was REFUSED at this
prim is not here: denials journal with grant_id NULL, so the grant is
never credited with a call it rejected.</p></section>|}
           (L.esc (L.short_hash g.S.g_id))
           verdict
           (L.esc g.S.g_id)
           (L.esc g.S.g_prim)
           scope_html
           (L.esc g.S.g_args_attenuation)
           (L.esc g.S.g_caller)
           (L.esc
              (match g.S.g_minted_by with
               | Some m -> m
               | None -> "—"))
           parent_html
           verdict
           (List.length ancestors)
           anc_rows
           (List.length exercises)
           total_calls
           run_rows
           (String.concat "" blocks))
