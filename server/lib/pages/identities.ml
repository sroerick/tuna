(* Tuna_server.Pages.Identities: the admin identity-minting page
   (pp-slice T2) over the same Store accessors the JSON API uses.
   Admin-only: a non-admin sees an explanation, never the form.  htmx
   enhances the mint in place; plain forms redirect back.

   The bearer token is shown ONCE in the mint result (only its sha256
   lands in the DB); the operator copies it to the agent or, when an
   initial password is given, the human signs in at /login. *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout

let view pool user _req =
  S.list_identities pool ()
  >>= fun ids ->
  let row (i : S.identity) =
    Printf.sprintf
      {|<tr><td><code>%s</code></td><td>%s</td><td>%s</td><td>%s</td></tr>|}
      (L.esc i.S.i_id)
      (L.esc i.S.i_name)
      (if i.S.i_is_admin then L.badge "warn" "admin" else L.badge "muted" "member")
      (L.esc (L.short_hash i.S.i_id))
  in
  let rows = String.concat "" (List.map row ids) in
  let roster =
    Printf.sprintf
      {|<section><h3>identities</h3>
<table>
<tr><th>id</th><th>name</th><th>role</th><th></th></tr>
%s
</table></section>|}
      rows
  in
  let mint_form =
    if not user.S.i_is_admin then
      {|<section><h3>mint</h3>
<p class="err">only an admin identity may mint identities.</p></section>|}
    else
      {|<section><h3>mint</h3>
<form hx-post="/identities/mint" hx-target="#mint-result" method="post" action="/identities/mint">
<p><label>name <input name="name" style="width:24ch"/></label>
<label>initial password <input type="password" name="password" style="width:24ch"/></label>
<label><input type="checkbox" name="is_admin" value="true"/> admin</label></p>
<button type="submit">mint</button>
</form>
<div id="mint-result"></div>
<p class="muted">The bearer token is shown once, below. Leave password blank for a
bearer-only agent identity.</p>
</section>|}
  in
  L.page ~user ~title:"tuna — identities" (roster ^ mint_form)

let mint_result ~user i token =
  L.page ~user ~title:"tuna — identities"
    (Printf.sprintf
       {|<h2>minted %s</h2><p class="okmsg">bearer token (shown once):</p><pre class="code">%s</pre><p><a href="/identities">back to identities</a></p>|}
       (L.esc i.S.i_name) (L.esc token))

let mint pool user req =
  if not user.S.i_is_admin then
    L.err_page ~code:403 ~user "only an admin identity may mint identities"
  else
    Dream.form ~csrf:false req
    >>= function
    | `Ok fields -> (
        match List.assoc_opt "name" fields with
        | None -> L.err_page ~user "missing name"
        | Some name -> (
            let name = String.trim name in
            if name = "" then L.err_page ~user "name must be non-empty"
            else
              let is_admin =
                match List.assoc_opt "is_admin" fields with
                | Some _ -> true
                | None -> false
              in
              S.fetch_identity_by_name pool name
              >>= (function
                    | Some _ ->
                        L.err_page ~code:409 ~user
                          (Printf.sprintf "identity %S already exists" name)
                    | None ->
                        let token = Tokens.random_token_hex () in
                        S.mint_identity pool ~is_admin ~name ~token ()
                        >>= fun i ->
                        let pw =
                          Option.value (List.assoc_opt "password" fields)
                            ~default:""
                          |> String.trim
                        in
                        (if pw = "" then Lwt.return ()
                         else S.set_password pool ~identity_id:i.S.i_id ~password:pw)
                        >>= fun () ->
                        if L.is_htmx req then
                          Dream.html
                            (Printf.sprintf
                               {|<span class="okmsg">minted <code>%s</code>. bearer token (shown once): <code>%s</code></span>|}
                               (L.esc i.S.i_name) (L.esc token))
                        else mint_result ~user i token)))
    | _ -> L.err_page ~user "bad form submission"
