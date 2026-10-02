(* Tuna_test_eio: a tiny Alcotest driver that runs the suites under an
   ambient Eio context, so the direct-style store/HTTP layers have the
   switch + net + clock they need (rim-eio §store).  Pure suites do not
   use this. *)

let run (name : string) (tests : unit Alcotest.test list) : unit =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Tuna_store.Pgx_eio.with_ctx
    (Tuna_store.Pgx_eio.Ctx.of_stdenv ~sw env)
    (fun () -> Alcotest.run name tests)
