(* Tuna_server.Pages.Auth: human session auth over the ONE credential
   store (identities.token_hash).  The session cookie carries the
   identity's own token; page requests verify it exactly like a bearer
   request — no separate session table, no second secret class (v0
   stance recorded in the plan; upgrade path: keyed/signature cookies). *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout

let cookie_name = "tuna_session"

(* Resolve the page identity from the session cookie (None = anonymous). *)
let identity_of_req pool req : S.identity option Lwt.t =
  match Dream.cookie req ~decrypt:false cookie_name with
  | None -> Lwt.return None
  | Some token -> S.verify_token pool token

(* Pages are behind the session; anonymous requests are sent to login. *)
let require_auth pool handler req =
  identity_of_req pool req
  >>= function
  | None -> Dream.redirect req "/login"
  | Some user -> handler user req

let set_session resp req token =
  Dream.set_cookie resp req ~encrypt:false ~http_only:true
    ~same_site:(Some `Lax) ~path:(Some "/") cookie_name token

let drop_session resp req = Dream.drop_cookie resp req cookie_name

let login_body =
  {|<h2>login</h2>
<p>sign in with an identity token (the same credential accepted as an
API bearer token).</p>
<form method="post" action="/login">
<label>identity token<br/><input type="password" name="token" style="width:32rem"/></label><br/><br/>
<button type="submit">sign in</button>
</form>|}

let login_get _req = L.page ~title:"tuna — login" login_body

let login_post pool req =
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      match List.assoc_opt "token" fields with
      | None -> L.err_page "missing token field"
      | Some token -> (
          S.verify_token pool (String.trim token)
          >>= function
          | None -> L.err_page ~code:401 ?user:None "unknown identity token"
          | Some _ ->
              Dream.redirect req "/" >>= fun resp ->
              set_session resp req (String.trim token);
              Lwt.return resp))
  | _ -> L.err_page "bad form submission"

let logout_get req =
  Dream.redirect req "/login" >>= fun resp ->
  drop_session resp req;
  Lwt.return resp
