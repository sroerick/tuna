(* Verbatim port of our Lwt pgx_unix IO instantiation to pure eio.

   rim-eio (borg/rim-eio.borg §store): pgx_lwt departs, pure [pgx]
   arrives over [Eio.Net].  The shape is PP's [Io_eio.Pgx_io]
   (pricklypear image/lib/io_eio.ml, same owner, ISC): a two-way
   [Eio.Flow] as both channels, direct-style monad [type 'a t = 'a],
   and the transaction Sequencer unlocked on exception (never a
   poisoned mutex).

   Socket law is byte-identical to the old module: this is a unix-
   socket dev story — pgx's [Unix] sockaddr branch names
   <dir>/.s.PGSQL.<port>, and open_connection must NOT attempt
   TCP_NODELAY on an AF_UNIX socket (the town-box gotcha).  The eio
   accept path simply connects through [Eio.Net.connect], which never
   touches NODELAY; for Inet we set it best-effort exactly as PP does. *)
module Ctx = struct
  type t = {
    sw : Eio.Switch.t;
    net : [ `Generic ] Eio.Net.ty Eio.Std.r;
    clock : Eio.Time.Mono.ty Eio.Std.r;
  }

  let current : t option ref = ref None

  let get () =
    match !current with
    | Some c -> c
    | None ->
        failwith
          "pgx_eio: no ambient Eio context (call Pgx_eio.with_ctx under Eio_main)"

  let with_ctx (c : t) (f : unit -> 'a) : 'a =
    let prev = !current in
    current := Some c;
    Fun.protect ~finally:(fun () -> current := prev) f

  let of_stdenv ~sw (env : Eio_unix.Stdenv.base) : t =
    {
      sw;
      net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r);
      clock = Eio.Stdenv.mono_clock env;
    }
end

let with_ctx = Ctx.with_ctx

module Thread = struct
  type 'a t = 'a

  let return x = x
  let ( >>= ) v f = f v
  let catch f h = try f () with e -> h e
  let protect f ~finally =
    match f () with
    | v ->
        finally ();
        v
    | exception e ->
        finally ();
        raise e

  type flow =
    [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

  type in_channel = flow
  type out_channel = flow

  type sockaddr =
    | Unix of string
    | Inet of string * int

  let open_connection sockaddr : flow * flow =
    let { Ctx.sw; net; _ } = Ctx.get () in
    let flow =
      match sockaddr with
      | Unix path -> (Eio.Net.connect ~sw net (`Unix path) :> flow)
      | Inet (host, port) ->
          (* libpq convention (mirrored by the old pgx_lwt_unix module):
             a host starting with '/' names a unix socket DIRECTORY —
             the server socket is <dir>/.s.PGSQL.<port>.  tuna passes
             TUNA_DB_HOST (a socket dir) as connect's ~host, so without
             this branch $PGHOST could never hijack a tuna connection
             and the dev cluster's /tmp socket would be unreachable. *)
          if String.length host > 0 && host.[0] = '/' then
            (Eio.Net.connect ~sw net
               (`Unix (Printf.sprintf "%s/.s.PGSQL.%d" host port))
              :> flow)
          else
            let service = string_of_int port in
            (match Eio.Net.getaddrinfo_stream net host ~service with
             | [] ->
                 failwith
                   ("pgx_eio: getaddrinfo empty for " ^ host ^ ":" ^ service)
             | addr :: _ -> (Eio.Net.connect ~sw net addr :> flow))
    in
    (* pagx writes each wire message as a separate small send; without
       TCP_NODELAY Nagle holds segments for the delayed ACK.  libpq
       sets it for exactly this reason.  AF_UNIX has no such option:
       best-effort, swallow. *)
    (match Eio_unix.Resource.fd_opt flow with
     | Some fd ->
         (try
            Eio_unix.Fd.use_exn "pgx_tcp_nodelay" fd (fun u ->
                Unix.setsockopt u Unix.TCP_NODELAY true)
          with _ -> ())
     | None -> ());
    (flow, flow)

  type ssl_config
  let upgrade_ssl = `Not_supported

  let output_char (oc : out_channel) c =
    let b = Bytes.make 1 c in
    Eio.Flow.copy_string (Bytes.unsafe_to_string b) oc

  let output_string (oc : out_channel) s = Eio.Flow.copy_string s oc

  let output_binary_int (oc : out_channel) n =
    let chr = Char.chr in
    output_char oc (chr (n lsr 24));
    output_char oc (chr ((n lsr 16) land 255));
    output_char oc (chr ((n lsr 8) land 255));
    output_char oc (chr (n land 255))

  let flush (_oc : out_channel) = ()

  let input_char (ic : in_channel) =
    let buf = Cstruct.create 1 in
    Eio.Flow.read_exact ic buf;
    Char.chr (Cstruct.get_uint8 buf 0)

  let really_input (ic : in_channel) (b : Bytes.t) pos len =
    if len < 0 || pos < 0 || pos + len > Bytes.length b then
      invalid_arg "Pgx_eio.really_input";
    (* Must blit: Cstruct.of_bytes does not write through. *)
    let cs = Cstruct.create len in
    Eio.Flow.read_exact ic cs;
    Cstruct.blit_to_bytes cs 0 b pos len

  let input_binary_int (ic : in_channel) =
    let b = Bytes.create 4 in
    really_input ic b 0 4;
    let code = Char.code in
    (code (Bytes.get b 0) lsl 24)
    lor (code (Bytes.get b 1) lsl 16)
    lor (code (Bytes.get b 2) lsl 8)
    lor code (Bytes.get b 3)

  let close_in (ic : in_channel) = Eio.Flow.close ic

  let getlogin () =
    try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with _ -> "tuna"

  let debug s = prerr_endline ("[pgx_eio] " ^ s)

  module Sequencer = struct
    type 'a monad = 'a t
    type 'a t = {
      v : 'a;
      m : Eio.Mutex.t;
    }

    let create v = { v; m = Eio.Mutex.create () }

    (* MUST NOT use Eio.Mutex.use_rw: that poisons the mutex when [f]
       raises, killing every later DB op.  Unlock-on-exception plus
       Cancel.protect keeps the wire section non-cancellable without
       destroying the lock (PP image.http-robustness invariant 6). *)
    let enqueue t f =
      Eio.Mutex.lock t.m;
      match Eio.Cancel.protect (fun () -> f t.v) with
      | v ->
          Eio.Mutex.unlock t.m;
          v
      | exception ex ->
          Eio.Mutex.unlock t.m;
          raise ex
  end
end

include Pgx.Make (Thread)
