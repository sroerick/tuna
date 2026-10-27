(* Tuna_server.Pages.Grants: the grants admin page (mint / list /
   revoke) over the same Store accessors the JSON API uses.  htmx
   enhances mint + revoke in place; plain forms redirect back. *)

open Tuna_store.Direct

module S = Tuna_store.Store
module J = Yojson.Basic
module L = Layout

let view pool user _req =
  S.list_grants pool ~author:None ()
  >>= fun gs ->
  let row (g : S.grant) =
    let revoked =
      match g.S.g_revoked_at with
      | Some at -> L.badge "bad" ("revoked " ^ at)
      | None -> L.badge "ok" "live"
    in
    let revoke_btn =
      if g.S.g_revoked_at = None then
        Printf.sprintf
          {|<form hx-post="/grants/%s/revoke" hx-target="closest tr" hx-swap="outerHTML" method="post" action="/grants/%s/revoke" style="display:inline"><button type="submit">revoke</button></form>|}
          g.S.g_id g.S.g_id
      else ""
    in

      let scope_html =
        match g.S.g_path_prefix with
        | Some p -> Printf.sprintf "<code>%s</code>" (L.esc p)
        | None -> {|<span class="muted">unscoped</span>|}
      in
      Printf.sprintf
        {|<tr id="grant-%s"><td><code>%s</code></td><td>%s</td><td>%s</td><td><code>%s</code></td><td><code>%s</code></td><td>%s</td><td><code>%s</code></td><td>%s</td></tr>|}
        (L.esc g.S.g_id)
        (L.esc (L.short_hash g.S.g_id))
        (L.esc g.S.g_prim)
        scope_html
        (L.esc g.S.g_args_attenuation)
        (L.esc (L.short_hash g.S.g_caller))
        revoked
        (L.esc (match g.S.g_minted_by with Some m -> L.short_hash m | None -> ""))
        revoke_btn
  in
  let rows = String.concat "" (List.map row gs) in
  let prim_options =
    String.concat ""
      (List.map
         (fun n -> Printf.sprintf {|<option value="%s">%s</option>|} n n)
          (Prims.wildcard :: Prims.names))
  in
  L.page ~user ~title:"tuna — grants"
    (Printf.sprintf
       {|<h2>grants</h2>
<section><h3>mint</h3>
<form hx-post="/grants/mint" hx-target="#mint-result" method="post" action="/grants/mint">
<p><label>prim <select name="prim">%s</select></label>
<label>args_attenuation JSON <input name="args_attenuation" value="{}" style="width:40ch"/></label>
<label>path_prefix <input name="path_prefix" placeholder="empty = unscoped" style="width:24ch"/></label></p>
<button type="submit">mint</button>
</form>
<div id="mint-result"></div>
</section>
<section><h3>issued</h3>
<table>
<tr><th>id</th><th>prim</th><th>path_prefix</th><th>attenuation</th><th>caller</th><th>state</th><th>minted by</th><th></th></tr>
%s
</table>
</section>|}
       prim_options rows)

let mint pool user req =
  Web.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      match List.assoc_opt "prim" fields with
      | None -> L.err_page ~user "missing prim"
      | Some prim -> (
          let prim = String.trim prim in
            if not (Prims.mintable prim) then
              L.err_page ~user
                (Printf.sprintf "unknown prim %S (v1 set: %s, plus \"*\")" prim
                   (String.concat ", " Prims.names))
            else
              let attenuation =
                Option.value (List.assoc_opt "args_attenuation" fields) ~default:"{}"
                |> String.trim
              in
              (* an empty box is the form's spelling for "no scope": a form
                 input cannot express absence any other way, unlike the JSON
                 surface, where an explicit "" is refused *)
              let prefix =
                match List.assoc_opt "path_prefix" fields with
                | None -> Ok None
                | Some p -> (
                    let p = String.trim p in
                    if p = "" then Ok None
                    else Tree_prims.grant_prefix "path_prefix" (Some p))
              in
              match prefix with
              | Error e -> L.err_page ~user e
              | Ok path_prefix ->
                  (try
                     J.from_string attenuation |> ignore;
                     S.mint_grant pool ~prim ~args_attenuation:attenuation
                         ~path_prefix:path_prefix ~caller:user.S.i_id
                       ~minted_by:(Some user.S.i_id) ()
                     >>= fun g ->
                     let html =
                       Printf.sprintf
                         {|<span class="okmsg">minted grant <code>%s</code> (%s%s)</span>|}
                         (L.esc g.S.g_id) (L.esc prim)
                         (match g.S.g_path_prefix with
                          | Some p -> ", scope " ^ L.esc p
                          | None -> ", unscoped")
                     in
                     if L.is_htmx req then Web.html html
                     else Web.redirect req "/grants"
                   with Yojson.Json_error _ ->
                     L.err_page ~user "args_attenuation must be valid JSON")))
  | _ -> L.err_page ~user "bad form submission"

let revoke pool user req =
  let id = Web.param req "id" in
  S.fetch_grant pool id
  >>= function
  | None -> L.not_found ~user "unknown grant id"
  | Some g -> (
      if g.S.g_caller <> user.S.i_id && not user.S.i_is_admin then
        L.err_page ~code:403 ~user "grant belongs to another identity"
      else
        S.revoke_grant pool id
        >>= fun () ->
        if L.is_htmx req then
          Web.html
            (Printf.sprintf
                 {|<tr id="grant-%s"><td><code>%s</code></td><td>%s</td><td>%s</td><td><code>%s</code></td><td><code>%s</code></td><td>%s</td><td></td><td></td></tr>|}
                 (L.esc g.S.g_id)
                 (L.esc (L.short_hash g.S.g_id))
                 (L.esc g.S.g_prim)
                 (match g.S.g_path_prefix with
                  | Some p -> Printf.sprintf "<code>%s</code>" (L.esc p)
                  | None -> {|<span class="muted">unscoped</span>|})
                 (L.esc g.S.g_args_attenuation)
                 (L.esc (L.short_hash g.S.g_caller))
                 (L.badge "bad" "revoked"))
        else Web.redirect req "/grants")
