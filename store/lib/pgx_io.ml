(* Verbatim port of pgx 2.2's pgx_lwt_unix IO module (upstream ships it
   as the separate pgx_lwt_unix package, which we do not vendor; the
   pgx_lwt we DO vendor needs this Unix IO instantiation to reach the
   postgres unix socket at /tmp).  Diff-checked against
   upstream's pgx_lwt_unix/src/pgx_lwt_unix.ml — keep in sync if pgx
   version bumps. *)
open Lwt

module Thread : Pgx_lwt.Io_intf.S = struct
  type sockaddr =
    | Unix of string
    | Inet of string * int

  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  let output_char = Lwt_io.write_char
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let really_input = Lwt_io.read_into_exactly
  let close_in = Lwt_io.close

  (* The unix getlogin syscall can fail *)
  let getlogin () =
    Unix.getuid () |> Lwt_unix.getpwuid >|= fun { Lwt_unix.pw_name; _ } -> pw_name
  ;;

  (* libpq convention (mirrored): a host starting with '/' names a unix
     socket directory — the server socket lives at
     <dir>/.s.PGSQL.<port>.  tuna passes TUNA_DB_HOST (a socket dir)
     through connect's ~host, so no $PGHOST fallback can ever route a
     tuna connection to Inet. *)
  let open_connection sockaddr =
    (match sockaddr with
    | Unix path -> return (Unix.ADDR_UNIX path)
    | Inet (host, port) ->
      if String.length host > 0 && host.[0] = '/' then
        return
          (Unix.ADDR_UNIX (Printf.sprintf "%s/.s.PGSQL.%d" host port))
      else
        Lwt_unix.gethostbyname host
        >|= fun { Lwt_unix.h_addr_list; _ } ->
        let len = Array.length h_addr_list in
        let i = Random.int len in
        let addr = h_addr_list.(i) in
        Unix.ADDR_INET (addr, port))
    >>= Lwt_io.open_connection
  ;;
end

include Pgx_lwt.Make (Thread)
