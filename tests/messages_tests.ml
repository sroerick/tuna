(* Addressing (migration 0019, todo/c3208d48): the public message log.

   PG-gated like the other store suites (scripts/test-store.sh) and run
   on its own scratch db (tuna_test_messages).  What it pins:

   - a write is signed by the posting key; a payload carrying "from" is
     refused and leaves no row
   - the log is public: an anonymous read returns body plus envelope
   - a poll returns a row exactly once and advances the cursor
   - ?since= replays without moving the cursor
   - status answers while no consumer is running
   - a fresh pool (a restarted process) does not replay consumed rows *)

open Tuna_store.Direct

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Api = Tuna_server.Api
module Web = Tuna_server.Web
module J = Yojson.Basic

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:
      (try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> return p

let auth_of (i : S.identity) =
  { Api.auth_id = i.S.i_id
  ; Api.auth_name = i.S.i_name
  ; Api.auth_is_admin = i.S.i_is_admin
  }

let req ?(meth = "POST") ?(body = "") ?(captures = []) target =
  { Web.meth; target; headers = []; body; captures }

let post body path = req ~meth:"POST" ~body path
let get path captures = req ~meth:"GET" ~captures path
let json (r : Web.resp) = J.from_string r.Web.body

let str j k = match J.Util.member k j with `String s -> s | _ -> ""

let int64_field j k =
  match J.Util.member k j with
  | `Int n -> Some (Int64.of_int n)
  | _ -> None

let messages j = match J.Util.member "messages" j with `List l -> l | _ -> []

let identity p name =
  S.bootstrap_identity p ~is_admin:false ~name ~token:(name ^ "-token") ()

let test_send_records_key_owner () =
  setup () >>= fun p ->
  identity p "gregor" >>= fun gregor ->
  let r =
    Api.post_msg p (auth_of gregor)
      (post {|{"to":"agora","text":"ping from gregor"}|} "/api/msg")
  in
  Alcotest.(check int) "201 created" 201 r.Web.status;
  let j = json r in
  Alcotest.(check string) "sender is the posting key" "gregor" (str j "from");
  Alcotest.(check string) "recipient" "agora" (str j "to");
  Alcotest.(check string) "body" "ping from gregor" (str j "body");
  Alcotest.(check string) "body hash" (Tuna.Hash.hex_of_string "ping from gregor")
    (str j "hash");
  Alcotest.(check bool) "seq assigned" true (int64_field j "seq" <> None);
  return ()

let test_from_refused () =
  setup () >>= fun p ->
  identity p "gregor" >>= fun gregor ->
  let r =
    Api.post_msg p (auth_of gregor)
      (post {|{"from":"agora","to":"nobody-inbox","text":"spoof"}|} "/api/msg")
  in
  Alcotest.(check int) "400" 400 r.Web.status;
  S.list_messages p ~recipient:"nobody-inbox" () >>= fun rows ->
  Alcotest.(check int) "no row was written" 0 (List.length rows);
  return ()

let test_public_read () =
  setup () >>= fun p ->
  identity p "gregor" >>= fun gregor ->
  let sent =
    Api.post_msg p (auth_of gregor)
      (post {|{"to":"publicbox","text":"public hello"}|} "/api/msg")
  in
  Alcotest.(check int) "write is 201" 201 sent.Web.status;
  let r = Api.get_messages p (req ~meth:"GET" "/api/messages?to=publicbox") in
  Alcotest.(check int) "anonymous read is 200" 200 r.Web.status;
  let ms = messages (json r) in
  Alcotest.(check int) "one row" 1 (List.length ms);
  let m = List.hd ms in
  Alcotest.(check string) "body is public" "public hello" (str m "body");
  Alcotest.(check string) "from is public" "gregor" (str m "from");
  Alcotest.(check string) "hash is public"
    (Tuna.Hash.hex_of_string "public hello") (str m "hash");
  return ()

let test_poll_exactly_once () =
  setup () >>= fun p ->
  S.send_message p ~sender:"gregor" ~recipient:"pollbox" ~body:"one" ()
  >>= fun _ ->
  let caps = [ ("name", "pollbox") ] in
  let poll () = Api.get_inbox p (get "/api/inbox/pollbox" caps) in
  let r1 = poll () in
  Alcotest.(check int) "200" 200 r1.Web.status;
  Alcotest.(check int) "first poll returns the row" 1
    (List.length (messages (json r1)));
  let r2 = poll () in
  Alcotest.(check int) "second poll returns nothing" 0
    (List.length (messages (json r2)));
  Api.get_inbox_status p (get "/api/inbox/pollbox/status" caps) >>= fun st ->
  let stj = json st in
  Alcotest.(check (option int64)) "cursor holds the polled seq"
    (int64_field (json r1) "cursor") (int64_field stj "cursor");
  Alcotest.(check (option int64)) "nothing pending" (Some 0L)
    (int64_field stj "pending");
  Alcotest.(check bool) "last_poll_at is set" true
    (J.Util.member "last_poll_at" stj <> `Null);
  return ()

let test_since_replays_without_moving () =
  setup () >>= fun p ->
  S.send_message p ~sender:"gregor" ~recipient:"replaybox" ~body:"two" ()
  >>= fun _ ->
  let caps = [ ("name", "replaybox") ] in
  let r1 = Api.get_inbox p (get "/api/inbox/replaybox" caps) in
  let cursor1 = int64_field (json r1) "cursor" in
  Alcotest.(check bool) "poll advanced the cursor" true (cursor1 <> None);
  let r2 = Api.get_inbox p (get "/api/inbox/replaybox?since=0" caps) in
  Alcotest.(check int) "since=0 replays the row" 1
    (List.length (messages (json r2)));
  Api.get_inbox_status p (get "/api/inbox/replaybox/status" caps) >>= fun st ->
  Alcotest.(check (option int64)) "a replay does not move the cursor" cursor1
    (int64_field (json st) "cursor");
  return ()

let test_status_answers_with_no_consumer () =
  setup () >>= fun p ->
  S.send_message p ~sender:"gregor" ~recipient:"sleepy" ~body:"wake" ()
  >>= fun _ ->
  let r =
    Api.get_inbox_status p
      (get "/api/inbox/sleepy/status" [ ("name", "sleepy") ])
  in
  Alcotest.(check int) "200 with no consumer" 200 r.Web.status;
  let j = json r in
  Alcotest.(check (option int64)) "nothing consumed" (Some 0L)
    (int64_field j "cursor");
  Alcotest.(check (option int64)) "one pending" (Some 1L)
    (int64_field j "pending");
  Alcotest.(check string) "never polled" "null"
    (J.to_string (J.Util.member "last_poll_at" j));
  return ()

let test_refusals () =
  setup () >>= fun p ->
  identity p "gregor" >>= fun gregor ->
  let auth = auth_of gregor in
  let missing = Api.post_msg p auth (post {|{"to":"agora"}|} "/api/msg") in
  Alcotest.(check int) "missing text is 400" 400 missing.Web.status;
  let bad =
    Api.post_msg p auth
      (post {|{"to":"Bad Name","text":"x"}|} "/api/msg")
  in
  Alcotest.(check int) "bad recipient name is 400" 400 bad.Web.status;
  let ok =
    Api.post_msg p auth
      (post {|{"to":"agora","text":"reply","kind":"note","re":"7"}|} "/api/msg")
  in
  Alcotest.(check int) "kind + re accepted" 201 ok.Web.status;
  Alcotest.(check string) "re echoed" "7" (str (json ok) "re");
  return ()

let test_cursor_survives_restart () =
  setup () >>= fun p ->
  S.send_message p ~sender:"gregor" ~recipient:"restartbox" ~body:"once" ()
  >>= fun _ ->
  let caps = [ ("name", "restartbox") ] in
  let r1 = Api.get_inbox p (get "/api/inbox/restartbox" caps) in
  Alcotest.(check int) "consumed once" 1
    (List.length (messages (json r1)));
  (* a fresh pool is a fresh process view of the same database *)
  Db.init (Db.config_from_env ()) >>= fun p2 ->
  let r2 = Api.get_inbox p2 (get "/api/inbox/restartbox" caps) in
  Alcotest.(check int) "a restart does not replay" 0
    (List.length (messages (json r2)));
  return ()

let () =
  match Sys.getenv_opt "TUNA_TEST_PG" with
  | None -> print_endline "messages tests skipped (TUNA_TEST_PG not set)"
  | Some _ ->
      let t name f = Alcotest.test_case name `Quick f in
      Tuna_test_eio.run "messages"
        [ ( "addressing"
          , [ t "a write is signed by the key" test_send_records_key_owner
            ; t "a from field is refused" test_from_refused
            ; t "the log is public" test_public_read
            ; t "a poll returns a row exactly once" test_poll_exactly_once
            ; t "since replays without moving the cursor"
                test_since_replays_without_moving
            ; t "status answers with no consumer"
                test_status_answers_with_no_consumer
            ; t "bad payloads are refused" test_refusals
            ; t "the cursor survives a restart" test_cursor_survives_restart
            ] ) ]
