(* Web error-path regression tests: pin the 10-04 outage class fixed by
   3430f9b ("web: contain the httpun error path").

   10-04 a scanner request hit httpun's error path: the then-no-op
   error_handler left the Reqd dangling in the error state, the next
   report_exn hit the reqd failwith (NYI), and an in-flight
   respond_with_string raised "invalid state, currently handling error"
   inside a fiber on the TOP switch - cancelling the whole process
   (down ~21h, crash-loop masked by a watchdog).

   Each case boots a real in-process Tuna_server.Web.serve on
   127.0.0.1:<ephemeral> and drives raw sockets against it, mirroring
   the 10-05 manual smoke: garbage request line, lying Content-Length,
   truncated/pipelined POST, raising handler, RST burst.  The contract
   under regression: errors are answered, failures stay contained, and
   the server is still serving afterwards.  Pre-fix, the invalid-state
   Failure kills the process and every case fails loudly.

   Client I/O is blocking Unix on eio's systhread pool
   (Eio_unix.run_in_systhread) - the same mechanism eio_posix itself
   uses for unpollable work (getaddrinfo, fsync); blocking Unix on the
   main fiber would starve the eio scheduler that runs the server.
   Every read is select()-bounded, so a misbehaving server fails a
   case in seconds instead of hanging the run.  All servers of a run
   share one long-lived switch: cases never cancel a server (no
   case-end join against httpun's error-state machinery); connection
   fibers finish on their own once clients close, and process exit
   reaps the rest.  No Postgres needed; runs unconditionally in
   @runtest. *)

let env_ref : Eio_unix.Stdenv.base option ref = ref None
let server_sw : Eio.Switch.t option ref = ref None

(* Alcotest under an ambient Eio context (tuna_test_eio without the PG
   ctx: this suite is HTTP plumbing, not store behavior). *)
let run (name : string) (tests : unit Alcotest.test list) : unit =
  Eio_main.run @@ fun env ->
  env_ref := Some env;
  Eio.Switch.run @@ fun sw ->
  server_sw := Some sw;
  (try Alcotest.run name tests
   with exn ->
     Printf.eprintf "[t] alcotest raised: %s\n%!" (Printexc.to_string exn);
     exit 1);
  exit 0

let systhread (f : unit -> 'a) : 'a = Eio_unix.run_in_systhread f

(* -- server lifecycle ------------------------------------------------- *)

(* grab an ephemeral loopback port: bind 0, read, release (serve binds
   with reuse_addr; a lost TOCTOU race fails the case loudly below,
   never hangs) *)
let free_port () : int =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> invalid_arg "expected an inet addr"
  in
  Unix.close s;
  port

(* poll until serve's listener answers, bounded (~5s) so a server that
   never comes up fails the case instead of hanging the battery *)
let wait_for_listener (port : int) : unit =
  let rec go left =
    if left = 0 then failwith "web server did not start listening in time"
    else
      let up =
        systhread (fun () ->
            try
              let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
              Unix.connect s (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
              Unix.close s;
              true
            with Unix.Unix_error _ -> false)
      in
      if up then ()
      else begin
        systhread (fun () -> Unix.sleepf 0.05);
        go (left - 1)
      end
  in
  go 100

(* serve on the run-wide switch: cases never cancel their server, so a
   case end never joins against httpun's error-state machinery; a
   serve that dies early (bind race) is logged and the case fails
   loudly via wait_for_listener *)
let with_server (handler : Tuna_server.Web.handler) (f : int -> unit) : unit =
  let env =
    match !env_ref with Some env -> env | None -> failwith "eio env missing"
  in
  let sw =
    match !server_sw with Some sw -> sw | None -> failwith "server switch missing"
  in
  let port = free_port () in
  Eio.Fiber.fork ~sw (fun () ->
      try
        Tuna_server.Web.serve ~env ~sw ~interface:"127.0.0.1" ~port handler
      with exn ->
        Printf.eprintf "[t] serve exited: %s\n%!" (Printexc.to_string exn));
  wait_for_listener port;
  f port

(* -- raw socket client ------------------------------------------------ *)

let open_conn (port : int) : Unix.file_descr =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (* socket-level backstops; the read bound is the select() deadline *)
  Unix.setsockopt_float s Unix.SO_RCVTIMEO 5.0;
  Unix.setsockopt_float s Unix.SO_SNDTIMEO 5.0;
  Unix.connect s (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  s

let send_all (s : Unix.file_descr) (data : string) : unit =
  let bytes = Bytes.of_string data in
  let len = Bytes.length bytes in
  let rec loop off =
    if off < len then loop (off + Unix.write s bytes off (len - off))
  in
  loop 0

(* half-close: tells the server "no more body is coming" without
   dropping our read side *)
let half_close (s : Unix.file_descr) : unit = Unix.shutdown s Unix.SHUTDOWN_SEND

(* SO_LINGER 0 makes the final close send RST instead of FIN (the
   scanner-burst shape) *)
let arm_rst (s : Unix.file_descr) : unit =
  Unix.setsockopt_optint s Unix.SO_LINGER (Some 0)

let index_of ?(from = 0) (needle : string) (haystack : string) : int option =
  let n = String.length haystack and m = String.length needle in
  if m = 0 then Some 0
  else begin
    let rec go i =
      if i + m > n then None
      else if String.equal (String.sub haystack i m) needle then Some i
      else go (i + 1)
    in
    go from
  end

let header_end (data : string) : int option =
  index_of "\r\n\r\n" data |> Option.map (fun i -> i + 4)

let header_value (head : string) (name : string) : string option =
  let lname = String.lowercase_ascii name in
  String.split_on_char '\n' head
  |> List.filter_map (fun line ->
         match String.index_opt line ':' with
         | None -> None
         | Some i ->
             let key =
               String.lowercase_ascii (String.trim (String.sub line 0 i))
             in
             if String.equal key lname then
               Some
                 (String.trim
                    (String.sub line (i + 1) (String.length line - i - 1)))
             else None)
  |> (function [] -> None | v :: _ -> Some v)

(* true once the response bytes are complete: headers plus a
   content-length body, or a terminal chunked frame *)
let response_complete (data : string) : bool =
  match header_end data with
  | None -> false
  | Some body_start -> (
      let head = String.sub data 0 (body_start - 4) in
      match header_value head "content-length" with
      | Some cl -> (
          match int_of_string_opt (String.trim cl) with
          | Some n -> String.length data - body_start >= n
          | None -> true)
      | None -> (
          match header_value head "transfer-encoding" with
          | Some te
            when String.equal (String.lowercase_ascii (String.trim te)) "chunked"
            -> index_of "0\r\n\r\n" data <> None
          | _ -> false))

(* select()-bounded read: stops on a complete response, EOF, or the
   deadline; returns the bytes that arrived, never raises on timeout
   (httpun error responses may be close-delimited without a prompt
   close - the deadline return keeps assertions working either way) *)
let read_response ?(timeout = 3.0) (s : Unix.file_descr) : string =
  let deadline = Unix.gettimeofday () +. timeout in
  let buf = Buffer.create 1024 in
  let tmp = Bytes.create 4096 in
  let rec loop () =
    if response_complete (Buffer.contents buf) then Buffer.contents buf
    else
      let remain = deadline -. Unix.gettimeofday () in
      if remain <= 0.0 then Buffer.contents buf
      else
        let r, _, _ = Unix.select [ s ] [] [] remain in
        match r with
        | [] -> Buffer.contents buf
        | _ -> (
            match Unix.read s tmp 0 (Bytes.length tmp) with
            | 0 -> Buffer.contents buf
            | n ->
                Buffer.add_subbytes buf tmp 0 n;
                loop ())
  in
  loop ()

(* stderr trace of what arrived (stdout belongs to Alcotest) *)
let trace (tag : string) (resp : string) : unit =
  let n = String.length resp in
  let head = if n > 140 then String.sub resp 0 140 else resp in
  Printf.eprintf "[t] %s: %d bytes %S\n%!" tag n head

(* drive one connection against [port] from a systhread *)
let drive (port : int) (f : Unix.file_descr -> 'a) : 'a =
  systhread (fun () ->
      let s = open_conn port in
      Fun.protect ~finally:(fun () -> try Unix.close s with _ -> ())
        (fun () -> f s))

let get_request (target : string) : string =
  "GET " ^ target
  ^ " HTTP/1.1\r\nHost: web-errorpath-test\r\nConnection: close\r\n\r\n"

(* oversized bodies answer 413 and the server closes mid-send; once
   that close lands, further client writes and reads are expected
   noise, not failures *)
let send_lossy (s : Unix.file_descr) (data : string) : unit =
  try send_all s data with Unix.Unix_error _ -> ()

let read_tolerant (s : Unix.file_descr) : string =
  try read_response s with Unix.Unix_error _ -> ""
(* -- assertions ------------------------------------------------------- *)

let first_line (resp : string) : string =
  match String.index_opt resp '\r' with
  | None -> resp
  | Some i -> String.sub resp 0 i

let check_status_line (label : string) (expected : string) (resp : string) :
    unit =
  Alcotest.(check string) label expected (first_line resp)

let check_contains (label : string) (needle : string) (resp : string) : unit =
  Alcotest.(check bool) label true (index_of needle resp <> None)

(* -- the test server -------------------------------------------------- *)

(* every case runs against this: "/" serves 200; "/boom" raises out of
   the handler like the smoke's "smoke boom" *)
let handler : Tuna_server.Web.handler =
 fun (r : Tuna_server.Web.req) ->
  if r.target = "/boom" then failwith "smoke boom"
  else
    { status = 200
    ; headers = [ ("Content-Type", "text/plain") ]
    ; body = "alive:" ^ r.meth ^ " " ^ r.target }

(* -- cases ------------------------------------------------------------ *)

(* 1. garbage request line: httpun raises Bad_request.  Post-fix the
   error stays inside the connection: an unparseable request line gets
   a clean close (0 bytes), a malformed-but-parsed request gets the
   error handler's 400 - the pre-fix no-op error handler left the Reqd
   dangling in the error state and the next report_exn killed the
   process.  Assert the connection ends contained; the real proof is
   the valid request served right after. *)
let test_garbage_request_line () =
  with_server handler (fun port ->
      drive port (fun s ->
          send_all s "GARBAGE\r\n\r\n";
          let resp = read_response s in
          trace "garbage" resp;
          Alcotest.(check bool)
            "connection ends contained (4xx answer or clean close)" true
            (resp = "" || index_of "HTTP/1.1 4" resp <> None));
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-garbage" resp;
          check_status_line "valid request served after garbage"
            "HTTP/1.1 200 OK" resp))

(* 2. lying Content-Length (declared > sent), then close: the request
   fiber's respond raced the error path pre-fix and escaped as
   "invalid state, currently handling error" onto the top switch.
   Contained = the next request still works. *)
let test_lying_content_length () =
  with_server handler (fun port ->
      drive port (fun s ->
          send_all s
            "POST /trunc HTTP/1.1\r\nHost: t\r\nContent-Length: 100\r\n\r\nshort";
          half_close s;
          (* the error response may or may not land before our close:
             never assert on it, only on containment downstream *)
          trace "lying-cl" (read_response s));
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-lying-cl" resp;
          check_status_line "valid request served after lying content-length"
            "HTTP/1.1 200 OK" resp))

(* 3. truncated pipelined POSTs: two requests in flight, bodies cut
   short, connection dropped - the smoke's pipelined-truncation shape
   that fired the error path twice.  Contained = still serving. *)
let test_truncated_pipelined_post () =
  with_server handler (fun port ->
      drive port (fun s ->
          send_all s
            ( "POST /trunc HTTP/1.1\r\nHost: t\r\nContent-Length: 100\r\n\r\nAAAA"
            ^ "POST /p1 HTTP/1.1\r\nHost: t\r\nContent-Length: 5\r\n\r\nBB" );
          half_close s;
          trace "trunc-pipeline" (read_response s));
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-trunc-pipeline" resp;
          check_status_line "valid request served after truncated pipeline"
            "HTTP/1.1 200 OK" resp))

(* 4. raising handler: the 500 must go to the offending request only,
   and the server must keep serving. *)
let test_raising_handler () =
  with_server handler (fun port ->
      drive port (fun s ->
          send_all s (get_request "/boom");
          let resp = read_response s in
          trace "boom" resp;
          check_status_line "raising handler answered 500"
            "HTTP/1.1 500 Internal Server Error" resp;
          check_contains "500 body present" "internal server error" resp);
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-boom" resp;
          check_status_line "valid request served after raising handler"
            "HTTP/1.1 200 OK" resp))

(* 5. 5x consecutive RST closes: the accept loop must ride the burst
   out and keep serving. *)
let test_rst_burst () =
  with_server handler (fun port ->
      for _ = 1 to 5 do
        drive port (fun s ->
            send_all s (get_request "/");
            (* the drive finally-close now sends RST, not FIN *)
            arm_rst s)
      done;
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-rst-burst" resp;
          check_status_line "accept loop healthy after 5x RST"
            "HTTP/1.1 200 OK" resp))

(* 6. body cap, declared: a Content-Length past the cap answers 413
   and closes without reading the body (only a small prefix is ever
   sent).  Pre-S1 the overflow was swallowed into an empty body and a
   declared-but-unsent body just hung until the client gave up. *)
let test_oversize_body_declared () =
  with_server handler (fun port ->
      drive port (fun s ->
          send_lossy s
            ( "POST /big HTTP/1.1\r\nHost: t\r\nContent-Type: text/plain\r\n"
            ^ "Content-Length: 1073741824\r\n\r\n"
            ^ String.make 65536 'x' );
          let resp = read_tolerant s in
          trace "oversize-declared" resp;
          check_contains "oversize declared body answered 413"
            "HTTP/1.1 413" resp);
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-oversize-declared" resp;
          check_status_line "valid request served after 413"
            "HTTP/1.1 200 OK" resp))

(* 7. body cap, actually-read: a CHUNKED body with no declared length,
   streamed past the cap.  Content-Length framing never delivers more
   than it declares, so chunked is the real read-path vector;
   read_body answers 413 once the read crosses it.  The server's close
   cuts the sender off mid-body, so client-side write errors are
   expected noise, not failures. *)
let test_oversize_body_read () =
  with_server handler (fun port ->
      drive port (fun s ->
          let chunk = String.make 65536 'y' in
          let head =
            "POST /big HTTP/1.1\r\nHost: t\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
          in
          send_lossy s head;
          let rec blast left =
            if left <= 0 then ()
            else
              try
                send_all s ("10000\r\n" ^ chunk ^ "\r\n");
                blast (left - String.length chunk)
              with Unix.Unix_error _ -> ()
          in
          blast (1024 * 1024 + 65536);
          (try half_close s with Unix.Unix_error _ -> ());
          let resp = read_tolerant s in
          trace "oversize-read" resp;
          check_contains "oversize read body answered 413"
            "HTTP/1.1 413" resp);
      drive port (fun s ->
          send_all s (get_request "/");
          let resp = read_response s in
          trace "after-oversize-read" resp;
          check_status_line "valid request served after 413"
            "HTTP/1.1 200 OK" resp))

let () =
  let tc name f = Alcotest.test_case name `Quick f in
  run "web-errorpath"
    [ ( "httpun error-path containment (10-04 outage class)"
      , [ tc "garbage request line -> 400 error, next request served"
            test_garbage_request_line
        ; tc "lying content-length + close -> contained, next request served"
            test_lying_content_length
        ; tc "truncated pipelined POST -> contained, next request served"
            test_truncated_pipelined_post
          ; tc "raising handler -> 500 contained to that request, server serves on"
              test_raising_handler
          ; tc "5x consecutive RST closes -> accept loop still healthy"
              test_rst_burst
          ; tc "oversize body (declared) -> 413, server serves on"
              test_oversize_body_declared
          ; tc "oversize body (actually read) -> 413, server serves on"
              test_oversize_body_read ] ) ]
