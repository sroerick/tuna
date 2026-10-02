(* Tuna_server.Pages.Auth: human session auth (pp-slice accounts tier).

   The browser tier over the one identity store: login takes
   username+password (kind='password' credentials, migration 0013) or,
   as the alternate form, a bearer token — either path MINTS an opaque
   session row (auth_sessions) and the cookie carries only that session
   token; page requests verify it by sha256 lookup.  The bearer token
   typed into the login form never lives in the cookie (the v0
   cookie-carries-the-bearer stance is superseded by this chapter;
   re-login is required after upgrade).  Expiry + revoke-on-logout are
   the PP auth shape; hashing is INTERIM sha256$salt$digest, recorded
   honestly in both READMEs with argon2id named as planned.

   Agents' bearer API auth is untouched — Api.authenticate now ALSO
   accepts the session cookie as a fallback (browser clients may drive
   /api/* from a logged-in session, PP's same stance; CSRF risk is the
   recorded limitation, mitigated only by SameSite=Lax for now). *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout

let cookie_name = "tuna_session"

(* 14 days, PP's browser TTL *)
let session_ttl_seconds = 60 * 60 * 24 * 14

(* Resolve the page identity from the session cookie (None = anonymous). *)
let identity_of_req pool req : S.identity option Lwt.t =
  match Dream.cookie req ~decrypt:false cookie_name with
  | None -> Lwt.return None
  | Some token -> S.verify_session pool token

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

(* Mint a session row and hand it out through the cookie; returns the
   (already-redirecting) response with the cookie attached. *)
let start_session pool resp req ~identity_id =
  S.mint_session pool ~identity_id ~ttl_seconds:session_ttl_seconds
  >>= fun token ->
  set_session resp req token;
  Lwt.return resp

let login_body =
  {|<h2>login</h2>
<p>sign in with username + password.  Or paste an identity bearer token
(the credential accepted as an API <code>Authorization: Bearer</code>)
and a session is minted for it.</p>
<form method="post" action="/login" style="margin-bottom:1rem">
<label>username<br/><input name="username" style="width:32rem"/></label><br/><br/>
<label>password<br/><input type="password" name="password" style="width:32rem"/></label><br/><br/>
<button type="submit">sign in</button>
</form>
<p>— or —</p>
<form method="post" action="/login">
<label>bearer token<br/><input type="password" name="token" style="width:32rem"/></label><br/><br/>
<button type="submit">sign in by token</button>
</form>|}

let login_get _req = L.page ~title:"tuna — login" login_body

let login_post pool req =
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      let field name = List.assoc_opt name fields in
      match (field "username", field "password") with
      | Some u, Some pw when String.trim u <> "" && pw <> "" -> (
          S.verify_password pool ~username:u ~password:pw
          >>= function
          | None -> L.err_page ~code:401 "unknown identity or wrong password"
          | Some i ->
              Dream.redirect req "/"
              >>= fun resp -> start_session pool resp req ~identity_id:i.S.i_id)
      | _ -> (
          match field "token" with
          | None -> L.err_page "missing username/password (or token) fields"
          | Some token -> (
              S.verify_token pool (String.trim token)
              >>= function
              | None -> L.err_page ~code:401 "unknown identity or token"
              | Some i ->
                  Dream.redirect req "/"
                  >>= fun resp ->
                  start_session pool resp req ~identity_id:i.S.i_id)))
  | _ -> L.err_page "bad form submission"

let logout_get pool req =
  match Dream.cookie req ~decrypt:false cookie_name with
  | None -> Dream.redirect req "/login"
  | Some token ->
      S.revoke_session pool token
      >>= fun () ->
      Dream.redirect req "/login"
      >>= fun resp ->
      drop_session resp req;
      Lwt.return resp
