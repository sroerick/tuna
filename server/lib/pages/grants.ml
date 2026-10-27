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
      {|<tr><td><code title="%s">%s</code></td><td><code title="%s">%s</code></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><code>%s</code></td></tr>|}
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
