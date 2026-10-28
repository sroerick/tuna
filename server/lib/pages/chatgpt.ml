(* Tuna_server.Pages.Chatgpt: "Sign in with ChatGPT" (OIDC, 0018).

   The human tier's agentless path: a human signs in with their ChatGPT
   account, and tuna provisions the identity itself — no agent-minted
   credential in the middle (the /api/signup "ask your agent" flow is
   bypassed entirely).  DORMANT by default: enabled only when both
   TUNA_CHATGPT_CLIENT_ID and TUNA_CHATGPT_CLIENT_SECRET are set; the
   routes answer 404 and the login page shows no button otherwise.

   Flow (authorization code + PKCE S256; token endpoint auth =
   client_secret_post):

     GET /login/chatgpt          store a one-shot state row (oidc_logins:
                                 nonce + code_verifier, 10-minute expiry)
                                 and 303 to OpenAI's authorize endpoint.
     GET /login/chatgpt/callback consume the state (atomic one-shot),
                                 exchange the code at the token endpoint
                                 over TLS, validate the id_token claims
                                 (iss/aud/exp/iat/nonce), cross-check the
                                 userinfo sub, find-or-create the identity
                                 (name law + garden grant, exactly the
                                 signup path), mint a 14-day session
                                 (Pages.Auth.start_session).

   Honest v1 limitation: the id_token SIGNATURE is not checked against
   the JWKS.  This flow never accepts a token from the front channel —
   the token comes from the back-channel TLS exchange, authenticated by
   the client secret — and the userinfo cross-check pins the access
   token to the id_token's sub.  JWKS/RS256 verification is the named
   follow-up (same stance as the interim sha256 password hashes of
   0013).  Also v1: no request timeout on the outbound TLS calls (the
   TLS edge's own timeout bounds the damage).

   Env: TUNA_CHATGPT_CLIENT_ID, TUNA_CHATGPT_CLIENT_SECRET,
   TUNA_CHATGPT_ISSUER (default https://auth.openai.com),
   TUNA_CHATGPT_REDIRECT_URI (optional exact override; the default
   derives from the request headers exactly like the login page's
   origin, so the operator registers <public-origin>/login/chatgpt/
   callback with OpenAI). *)

open Tuna_store.Direct

(* the monad module named, for Direct.return below (store.ml resolves the
   sibling name inside tuna_store; this library must alias it) *)
module Direct = Tuna_store.Direct

module S = Tuna_store.Store
module L = Layout

(* -- config ---------------------------------------------------------- *)

let env name =
  match Sys.getenv_opt name with
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let client_id () = env "TUNA_CHATGPT_CLIENT_ID"
let client_secret () = env "TUNA_CHATGPT_CLIENT_SECRET"

let issuer () =
  Option.value (env "TUNA_CHATGPT_ISSUER") ~default:"https://auth.openai.com"

let enabled () =
  match (client_id (), client_secret ()) with
  | Some _, Some _ -> true
  | _ -> false

let not_configured_msg =
  "chatgpt sign-in is not configured on this server"

(* Endpoints from the issuer's live discovery document
   (https://auth.openai.com/.well-known/openid-configuration, fetched
   2026-10-27): response_types = code only, code_challenge_methods =
   S256 only, id_token = RS256, token endpoint auth includes
   client_secret_post.  The issuer is scheme://host (no path). *)
let authorize_path = "/api/accounts/authorize"
let token_path = "/api/accounts/oauth/token"
let userinfo_path = "/api/accounts/oauth/userinfo"

let issuer_host iss =
  let stripped =
    if String.starts_with ~prefix:"https://" iss then
      String.sub iss 8 (String.length iss - 8)
    else iss
  in
  match String.index_opt stripped '/' with
  | Some i -> String.sub stripped 0 i
  | None -> stripped

let state_ttl_seconds = 600 (* 10 minutes to finish the flow *)

(* -- base64url + PKCE ------------------------------------------------ *)

let b64url_encode s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let b64url_decode s =
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s with
  | Ok s -> Some s
  | Error _ -> None

(* opaque flow tokens: n random bytes from the kernel, base64url *)
let random_b64url n =
  let ic = open_in_bin "/dev/urandom" in
  let raw = really_input_string ic n in
  close_in ic;
  b64url_encode raw

let sha256_raw s =
  Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string s)

(* RFC 7636: challenge = BASE64URL(SHA256(verifier)) *)
let pkce_challenge verifier = b64url_encode (sha256_raw verifier)

(* -- outbound HTTPS (tls-eio over the boot-provided Eio net) --------- *)

let net : [ `Generic ] Eio.Net.ty Eio.Std.r option ref = ref None

(* tls-eio requires a default generator; seed it from the kernel once. *)
let ensure_rng () =
  match Mirage_crypto_rng.default_generator () with
  | _ -> ()
  | exception Mirage_crypto_rng.No_default_generator ->
      let ic = open_in_bin "/dev/urandom" in
      let seed = really_input_string ic 64 in
      close_in ic;
      Mirage_crypto_rng.set_default_generator
        (Mirage_crypto_rng.create ~seed (module Mirage_crypto_rng.Fortuna))

let boot ~net:(net_ : [ `Generic ] Eio.Net.ty Eio.Std.r) =
  ensure_rng ();
  net := Some net_

(* the system trust bundle: OpenBSD ships it at /etc/ssl/cert.pem *)
let authenticator_result =
  lazy
    (match open_in_bin "/etc/ssl/cert.pem" with
     | ic ->
         let s = really_input_string ic (in_channel_length ic) in
         close_in ic;
         (match X509.Certificate.decode_pem_multiple s with
          | Ok certs ->
              Ok
                (X509.Authenticator.chain_of_trust
                   ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ()))
                   certs)
          | Error (`Msg m) -> Error m
          | Error _ -> Error "could not decode the system CA bundle")
     | exception Sys_error m -> Error m)

(* -- HTTP response parsing ------------------------------------------- *)

let find_sub ?(from = 0) needle hay =
  let n = String.length needle and m = String.length hay in
  let rec go i =
    if i + n > m then None
    else if String.sub hay i n = needle then Some i
    else go (i + 1)
  in
  go from

let unchunk s =
  let b = Buffer.create 1024 in
  let len = String.length s in
  let rec go pos =
    match find_sub "\r\n" s ~from:pos with
    | None -> ()
    | Some nl ->
        let size_str = String.trim (String.sub s pos (nl - pos)) in
        let size_str =
          match String.index_opt size_str ';' with
          | Some i -> String.sub size_str 0 i
          | None -> size_str
        in
        (match int_of_string_opt ("0x" ^ size_str) with
         | None | Some 0 -> ()
         | Some n ->
             let start = nl + 2 in
             if start + n + 2 <= len then (
               Buffer.add_string b (String.sub s start n);
               go (start + n + 2)))
  in
  go 0;
  Buffer.contents b

let parse_http_response raw =
  match find_sub "\r\n\r\n" raw with
  | None -> Error "truncated HTTP response"
  | Some i ->
      let head = String.sub raw 0 i in
      let rest =
        String.sub raw (i + 4) (String.length raw - i - 4)
      in
      let lines =
        String.split_on_char '\n' head |> List.map String.trim
      in
      (match lines with
       | [] -> Error "empty HTTP response"
       | status_line :: hdr_lines ->
           let status =
             match String.split_on_char ' ' status_line with
             | _ :: code :: _ -> (
                 match int_of_string_opt code with Some c -> c | None -> 0)
             | _ -> 0
           in
           let headers =
             List.filter_map
               (fun l ->
                 match String.index_opt l ':' with
                 | Some j ->
                     Some
                       ( String.lowercase_ascii (String.trim (String.sub l 0 j))
                       , String.trim (String.sub l (j + 1) (String.length l - j - 1))
                       )
                 | None -> None)
               hdr_lines
           in
           let hdr name =
             List.find_map (fun (k, v) -> if k = name then Some v else None) headers
           in
           let body =
             if
               String.lowercase_ascii
                 (Option.value (hdr "transfer-encoding") ~default:"")
               = "chunked"
             then unchunk rest
             else
               match hdr "content-length" with
               | Some n -> (
                   match int_of_string_opt n with
                   | Some len when len <= String.length rest ->
                       String.sub rest 0 len
                   | _ -> rest)
               | None -> rest
           in
           Ok (status, body))

(* one HTTPS round trip: Connection: close, read to EOF, decode chunked
   or content-length bodies.  Errors are strings — this is a handler
   surface, not an API contract. *)
let https ~host ~(meth_ : string) ~path ?(headers = []) ?(body = "") () :
    ((int * string), string) result =
  match (!net, Lazy.force authenticator_result) with
  | None, _ -> Error "https client not booted (server restart required)"
  | _, Error m -> Error ("tls trust setup: " ^ m)
  | Some net, Ok auth -> (
      match Tls.Config.client ~authenticator:auth () with
      | Error (`Msg m) -> Error ("tls config: " ^ m)
      | Ok cfg ->
          (try
             Eio.Net.with_tcp_connect ~host ~service:"443" net @@ fun socket ->
             let tls =
               Tls_eio.client_of_flow cfg
                 ?host:(Some (Domain_name.host_exn (Domain_name.of_string_exn host)))
                 socket
             in
             let req =
               Printf.sprintf "%s %s HTTP/1.1\r\nHost: %s\r\n" meth_ path host
               ^ String.concat ""
                   (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers)
               ^ "Connection: close\r\n"
               ^ Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body)
               ^ body
             in
             Eio.Flow.write tls [ Cstruct.of_string req ];
             let buf = Buffer.create 4096 in
             let chunk = Cstruct.create 16384 in
             let rec read_all () =
               match Eio.Flow.single_read tls chunk with
               | 0 -> ()
               | n ->
                   Buffer.add_string buf
                     (Cstruct.to_string (Cstruct.sub chunk 0 n));
                   if Buffer.length buf > 4_000_000 then ()
                   else read_all ()
               | exception End_of_file -> ()
             in
             read_all ();
             parse_http_response (Buffer.contents buf)
           with exn -> Error (Printexc.to_string exn)))


(* -- JSON helpers ---------------------------------------------------- *)

let json_str j k =
  match Yojson.Basic.Util.member k j with
  | `String s -> Some s
  | _ -> None

let json_int j k =
  match Yojson.Basic.Util.member k j with
  | `Int i -> Some i
  | _ -> None

let parse_json s =
  try Ok (Yojson.Basic.from_string s)
  with _ -> Error "response was not JSON"

let form_encode fields =
  String.concat ""
    (List.map
       (fun (k, v) -> Web.url_encode k ^ "=" ^ Web.url_encode v ^ "&")
       fields)

(* -- the OIDC round trip --------------------------------------------- *)

(* back-channel code exchange: client_secret_post (the discovery doc
   lists client_secret_basic/post/none; post keeps us off basic-auth
   header juggling) *)
let exchange_code ~host ~client_id ~client_secret ~code ~verifier ~redirect_uri
    : ((string * string), string) result =
  let body =
    form_encode
      [ ("grant_type", "authorization_code")
      ; ("code", code)
      ; ("redirect_uri", redirect_uri)
      ; ("code_verifier", verifier)
      ; ("client_id", client_id)
      ; ("client_secret", client_secret) ]
  in
  match
    https ~host ~meth_:"POST" ~path:token_path
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
      ~body ()
  with
  | Error m -> Error ("token endpoint: " ^ m)
  | Ok (status, _) when status <> 200 ->
      Error (Printf.sprintf "token endpoint answered %d" status)
  | Ok (_, body) -> (
      match parse_json body with
      | Error m -> Error m
      | Ok j -> (
          match (json_str j "access_token", json_str j "id_token") with
          | Some at, Some it -> Ok (at, it)
          | _ -> Error "token response missing access_token/id_token"))

type claims = { sub : string; email : string option; name : string option }

(* claim validation.  The signature is NOT verified in v1 — see the
   module header: back-channel delivery + client secret + the userinfo
   sub cross-check below carry the same assurance for this flow. *)
let validate_id_token ~iss ~audience ~expected_nonce id_token :
    (claims, string) result =
  match String.split_on_char '.' id_token with
  | [ _hdr; payload; _sig ] -> (
      match b64url_decode payload with
      | None -> Error "id_token payload is not base64url"
      | Some s -> (
          match Yojson.Basic.from_string s with
          | exception _ -> Error "id_token payload is not JSON"
          | j ->
              let now = Unix.time () in
              let ok_iss = (match json_str j "iss" with Some v -> v = iss | None -> false) in
              let ok_aud =
                match Yojson.Basic.Util.member "aud" j with
                | `String s -> s = audience
                | `List xs ->
                    List.exists
                      (function `String s -> s = audience | _ -> false)
                      xs
                | _ -> false
              in
              let ok_exp =
                match json_int j "exp" with
                | Some e -> float_of_int e > now -. 60.0
                | None -> false
              in
              let ok_iat =
                match json_int j "iat" with
                | Some i -> float_of_int i < now +. 300.0
                | None -> false
              in
              let ok_nonce =
                match json_str j "nonce" with Some v -> v = expected_nonce | None -> false
              in
              if not ok_iss then Error "id_token issuer mismatch"
              else if not ok_aud then Error "id_token audience mismatch"
              else if not ok_exp then Error "id_token expired"
              else if not ok_iat then Error "id_token issued in the future"
              else if not ok_nonce then Error "id_token nonce mismatch"
              else
                match json_str j "sub" with
                | Some sub when sub <> "" ->
                    Ok { sub; email = json_str j "email"; name = json_str j "name" }
                | _ -> Error "id_token has no sub"))
  | _ -> Error "id_token is not a JWS"

(* userinfo: the access token's subject must equal the id_token's — the
   v1 stand-in for signature verification (see header). *)
let userinfo_matches_sub ~host ~access_token ~sub :
    (unit, string) result =
  match
    https ~host ~meth_:"GET" ~path:userinfo_path
      ~headers:[ ("Authorization", "Bearer " ^ access_token) ] ()
  with
  | Error m -> Error ("userinfo: " ^ m)
  | Ok (status, _) when status <> 200 ->
      Error (Printf.sprintf "userinfo answered %d" status)
  | Ok (_, body) -> (
      match parse_json body with
      | Error m -> Error m
      | Ok j -> (
          match json_str j "sub" with
          | Some s when s = sub -> Ok ()
          | _ -> Error "userinfo subject does not match the id_token"))

(* -- identity provisioning ------------------------------------------- *)

(* the identity name law (store.ml): a-z, 0-9, '-', no leading/trailing
   dash, <= name_max_len (32) *)
let sanitize_name s =
  let s = String.lowercase_ascii (String.trim s) in
  let mapped =
    String.map
      (fun c ->
        if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-' then c
        else '-')
      s
  in
  let b = Buffer.create (String.length mapped) in
  let prev_dash = ref true (* leading dashes dropped *) in
  String.iter
    (fun c ->
      if c = '-' then (
        if not !prev_dash then Buffer.add_char b '-';
        prev_dash := true)
      else (
        Buffer.add_char b c;
        prev_dash := false))
    mapped;
  let name = Buffer.contents b in
  let name =
    if String.length name > 32 then String.sub name 0 32 else name
  in
  let name =
    if String.length name > 0 && name.[String.length name - 1] = '-' then
      String.sub name 0 (String.length name - 1)
    else name
  in
  if name = "" then "human" else name

let derived_name claims =
  match claims.name with
  | Some n when sanitize_name n <> "human" -> sanitize_name n
  | _ -> (
      match claims.email with
      | Some e ->
          let local =
            match String.index_opt e '@' with
            | Some i -> String.sub e 0 i
            | None -> e
          in
          sanitize_name local
      | None -> "human")

(* find-or-create the identity behind (provider='chatgpt', subject).
   New identities ride the signup path — name law + garden grant in one
   transaction — with a deterministic retry suffix on name collisions.
   The filler bearer token satisfies the schema; it is never revealed
   (OIDC humans are the browser tier). *)
let provision pool ~claims =
  let create_new () =
    let base = derived_name claims in
    let filler_token = random_b64url 32 in
    let rec attempt i =
      let candidate =
        if i = 0 then base else Printf.sprintf "%s-%d" base i
      in
      S.signup_oidc_identity pool ~provider:"chatgpt" ~subject:claims.sub
        ~name:candidate ~token:filler_token ()
      >>= function
      | Ok identity -> Direct.return (Ok identity)
      | Error `Exists when i < 50 -> attempt (i + 1)
      | Error `Exists ->
          Direct.return
            (Error "every name variant is taken; an admin can rename later")
      | Error (`Invalid_name m | `Name_conflict m) -> Direct.return (Error m)
    in
    attempt 0
  in
  S.find_oidc_identity pool ~provider:"chatgpt" ~subject:claims.sub
  >>= (function
        | Some id -> (
            S.fetch_identity pool id >>= function
            | Some i -> Direct.return (Ok i)
            | None -> create_new ())
        | None -> create_new ())

(* -- handlers -------------------------------------------------------- *)

let redirect_uri_of_req req =
  match env "TUNA_CHATGPT_REDIRECT_URI" with
  | Some u -> u
  | None -> (
      match Agent.base_url_of_headers (Web.all_headers req) with
      | Some base -> base ^ "/login/chatgpt/callback"
      | None -> "http://localhost:18090/login/chatgpt/callback")

let start pool req =
  if not (enabled ()) then L.err_page ~code:404 not_configured_msg
  else
    let state = random_b64url 32 in
    let nonce = random_b64url 16 in
    let verifier = random_b64url 32 in
    S.put_oidc_login pool ~state ~nonce ~code_verifier:verifier
      ~ttl_seconds:state_ttl_seconds
    >>= fun () ->
    let iss = issuer () in
    let q =
      form_encode
        [ ("client_id", Option.value (client_id ()) ~default:"")
        ; ("redirect_uri", redirect_uri_of_req req)
        ; ("response_type", "code")
        ; ("scope", "openid profile email")
        ; ("state", state)
        ; ("nonce", nonce)
        ; ("code_challenge", pkce_challenge verifier)
        ; ("code_challenge_method", "S256") ]
    in
      Web.redirect req (iss ^ authorize_path ^ "?" ^ q)

let callback pool req =
  if not (enabled ()) then L.err_page ~code:404 not_configured_msg
  else
    let code = Web.query req "code" and state = Web.query req "state" in
    match (code, state) with
    | Some code, Some state when code <> "" && state <> "" -> (
        S.take_oidc_login pool ~state >>= function
        | None ->
            S.log_auth pool ~kind:"chatgpt" ~success:false ();
            L.err_page ~code:400
              "this sign-in attempt expired or was already used - start again from the login page"
        | Some (nonce, verifier) ->
            let iss = issuer () in
            let host = issuer_host iss in
            let audience = Option.value (client_id ()) ~default:"" in
            let outcome =
              match
                exchange_code ~host
                  ~client_id:audience
                  ~client_secret:(Option.value (client_secret ()) ~default:"")
                  ~code ~verifier
                  ~redirect_uri:(redirect_uri_of_req req)
              with
              | Error m -> Error m
              | Ok (access_token, id_token) -> (
                  match
                    validate_id_token ~iss ~audience ~expected_nonce:nonce id_token
                  with
                  | Error m -> Error m
                  | Ok claims -> (
                      match
                        userinfo_matches_sub ~host ~access_token ~sub:claims.sub
                      with
                      | Error m -> Error m
                      | Ok () -> Ok claims))
            in
            match outcome with
            | Error m ->
                S.log_auth pool ~kind:"chatgpt" ~success:false ();
                L.err_page ~code:400 ("sign-in rejected: " ^ m)
            | Ok claims -> (
                provision pool ~claims >>= function
                | Error m ->
                    S.log_auth pool ~kind:"chatgpt" ~success:false ();
                    L.err_page ~code:400
                      ("could not provision the identity: " ^ m)
                | Ok identity ->
                    S.log_auth pool ~identity_id:identity.S.i_id
                      ~kind:"chatgpt" ~success:true ();
                    Web.redirect req "/" >>= fun resp ->
                    Auth.start_session pool resp req
                      ~identity_id:identity.S.i_id))
    | _ -> L.err_page ~code:400 "missing code/state parameters"
