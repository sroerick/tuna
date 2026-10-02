(* Tuna server entry point (M6).

   Identity bootstrap follows pricklypear's PP_BOOTSTRAP pattern: the
   root bearer token comes from TUNA_BOOTSTRAP_TOKEN, or is generated
   (32 random bytes) and printed ONCE to stdout — the operator copies
   it at first boot; only its sha256 is stored (Store.bootstrap_identity,
   name "root", is_admin).  Re-boots with the same token are no-ops.

   rim-eio: the entry point is an Eio mainloop.  No Lwt_main, no
   bridge; the whole server runs on one Eio domain and the store/HTTP
   layers are direct style. *)

let read_port () =
  match Sys.getenv_opt "TUNA_HTTP_PORT" with
  | Some p -> ( try int_of_string p with _ -> 18090)
  | None -> 18090

let random_token () =
  let ic = open_in_bin "/dev/urandom" in
  let raw = really_input_string ic 32 in
  close_in ic;
  let hex = Buffer.create 64 in
  String.iter
    (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c)))
    raw;
  Buffer.contents hex

let () =
  let port = read_port () in
  let token =
    match Sys.getenv_opt "TUNA_BOOTSTRAP_TOKEN" with
    | Some t when String.trim t <> "" -> String.trim t
    | _ -> random_token ()
  in
  (* printing is serve's job: only a FRESH credential gets printed *)
  Tuna_server.Web.log "boot: http on 127.0.0.1:%d" port;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Tuna_server.Api.serve ~env ~sw ~port ~bootstrap_token:token
