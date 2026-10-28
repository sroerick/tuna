(* chatgpt sign-in (0018): the pure law tests - b64url, PKCE, the name
   sanitizer, id_token claim validation, HTTP response parsing.  No PG.
   The outbound HTTPS path is exercised only when TUNA_TEST_NET=1 (one
   discovery fetch against auth.openai.com). *)

module C = Tuna_server.Chatgpt

let b64url_roundtrip () =
  let strings =
    [ ""; "hello"; "hello, wörld"; "f0=\r\n\xff\xfe"; "+++///___" ]
  in
  List.iter
    (fun s ->
      match C.b64url_decode (C.b64url_encode s) with
      | Some v -> Alcotest.(check string) "b64url roundtrip" s v
      | None -> Alcotest.fail ("decode failed for " ^ s))
    strings;
  (* the padless, url-safe alphabet: no '+', '/', or '=' in the output *)
  let enc = C.b64url_encode "subjects?and/verbs+" in
  Alcotest.(check bool) "url-safe output" true
    (not (String.contains enc '+')
    && not (String.contains enc '/')
    && not (String.contains enc '='))

(* RFC 7636 appendix B *)
let pkce_rfc7636 () =
  let verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" in
  Alcotest.(check string) "pkce s256 challenge"
    "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    (C.pkce_challenge verifier)

let sanitize_cases () =
  Alcotest.(check string) "accents + punct collapse" "r-erick-o-brien"
    (C.sanitize_name "Röerick O'Brien");
  Alcotest.(check string) "edge dashes dropped" "alice"
    (C.sanitize_name " -alice- ");
  Alcotest.(check string) "empty falls back" "human" (C.sanitize_name "***")

let make_token ~iss ~aud ~exp ~iat ~nonce ~sub =
  let payload =
    Yojson.Basic.to_string
      (`Assoc
        ([ ("iss", `String iss)
         ; ("aud", aud)
         ; ("exp", `Int exp)
         ; ("iat", `Int iat)
         ; ("sub", `String sub) ]
        @
        match nonce with
        | Some n -> [ ("nonce", `String n) ]
        | None -> []))
  in
  "e30." ^ C.b64url_encode payload ^ ".c2ln"

let validate_cases () =
  let iss = "https://auth.openai.com" and cid = "my-client" in
  let now = int_of_float (Unix.time ()) in
  let good =
    make_token ~iss ~aud:(`String cid) ~exp:(now + 600) ~iat:now
      ~nonce:(Some "n-123") ~sub:"user-1"
  in
  (match C.validate_id_token ~iss ~audience:cid ~expected_nonce:"n-123" good with
   | Ok cl ->
       Alcotest.(check string) "sub" "user-1" cl.sub;
         Alcotest.(check (option string)) "no email by default" None cl.email
   | Error m -> Alcotest.fail ("good token rejected: " ^ m));
  (* audience may arrive as a list *)
  let list_aud =
    make_token ~iss ~aud:(`List [ `String cid ]) ~exp:(now + 600) ~iat:now
      ~nonce:(Some "n") ~sub:"u"
  in
  Alcotest.(check bool) "aud list accepted" true
    (match
       C.validate_id_token ~iss ~audience:cid ~expected_nonce:"n" list_aud
     with
     | Ok _ -> true
     | Error _ -> false);
  let rejects what tok =
    Alcotest.(check bool) what true
      (match
         C.validate_id_token ~iss ~audience:cid ~expected_nonce:"n" tok
       with
       | Error _ -> true
       | Ok _ -> false)
  in
  rejects "wrong issuer"
    (make_token ~iss:"https://evil.example" ~aud:(`String cid)
       ~exp:(now + 600) ~iat:now ~nonce:(Some "n") ~sub:"u");
  rejects "wrong audience"
    (make_token ~iss ~aud:(`String "other") ~exp:(now + 600) ~iat:now
       ~nonce:(Some "n") ~sub:"u");
  rejects "expired"
    (make_token ~iss ~aud:(`String cid) ~exp:(now - 3600) ~iat:now
       ~nonce:(Some "n") ~sub:"u");
  rejects "future iat"
    (make_token ~iss ~aud:(`String cid) ~exp:(now + 600)
       ~iat:(now + 3600) ~nonce:(Some "n") ~sub:"u");
  rejects "nonce mismatch"
    (make_token ~iss ~aud:(`String cid) ~exp:(now + 600) ~iat:now
       ~nonce:(Some "other") ~sub:"u");
  rejects "missing nonce"
    (make_token ~iss ~aud:(`String cid) ~exp:(now + 600) ~iat:now
       ~nonce:None ~sub:"u")

let http_parse_cases () =
  (match
     C.parse_http_response
       "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 5\r\n\r\nhelloXX"
   with
   | Ok (200, "hello") -> ()
   | _ -> Alcotest.fail "content-length body");
  (match
     C.parse_http_response
       "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n"
   with
   | Ok (200, "hello") -> ()
   | _ -> Alcotest.fail "chunked body");
  (match C.parse_http_response "HTTP/1.1 404 Not Found\r\n\r\n" with
   | Ok (404, "") -> ()
   | _ -> Alcotest.fail "404 empty body")

let discovery_fetch () =
  if Sys.getenv_opt "TUNA_TEST_NET" <> Some "1" then
    Alcotest.skip () (* network test disabled; set TUNA_TEST_NET=1 *)
  else
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      C.boot ~net:(Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r);
    ignore sw;
    match
      C.https ~host:"auth.openai.com" ~meth_:"GET"
        ~path:"/.well-known/openid-configuration" ()
    with
    | Ok (200, body) ->
        (* the doc is pretty-printed; do not assume a flat prefix *)
        if
          String.length body < 10
          || C.find_sub "issuer" body = None
          || C.find_sub "openai.com" body = None
        then Alcotest.fail "discovery body shape"
    | Ok (s, _) -> Alcotest.fail (Printf.sprintf "discovery answered %d" s)
    | Error m -> Alcotest.fail m

let () =
  Alcotest.run "chatgpt"
    [ ( "pure"
      , [ ("b64url roundtrip", `Quick, b64url_roundtrip)
        ; ("pkce rfc7636 vector", `Quick, pkce_rfc7636)
        ; ("name sanitizer", `Quick, sanitize_cases)
        ; ("id_token claims", `Quick, validate_cases)
        ; ("http response parse", `Quick, http_parse_cases) ] )
    ; ( "network"
      , [ ("discovery fetch", `Slow, discovery_fetch) ] ) ]
