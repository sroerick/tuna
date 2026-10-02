(* eio-pg-probe: prove pure pgx over Eio.Net reaches the tuna unix-socket
   postgres before the rest of the rim rides on it.  Dev-only, deleted
   at the end of the rim-eio series. *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Tuna_store.Pgx_eio.with_ctx
    (Tuna_store.Pgx_eio.Ctx.of_stdenv ~sw env)
    (fun () ->
      let cfg = Tuna_store.Db.config_from_env () in
      let pool = Tuna_store.Db.init cfg in
      let n = Tuna_store.Db.apply_migrations pool ~dir:"migrations" in
      let rows = Tuna_store.Db.q pool "SELECT name, is_admin FROM identities ORDER BY name" in
      Printf.printf "migrations applied this run: %d\n" n;
      List.iter
        (fun row ->
          match row with
          | [ Some n; Some b ] ->
              Printf.printf "identity %s admin=%s\n"
                (Pgx.Value.to_string_exn (Some n))
                (Pgx.Value.to_string_exn (Some b))
          | _ -> Printf.printf "row?\n")
        rows;
      Printf.printf "pgx_eio OK\n")
