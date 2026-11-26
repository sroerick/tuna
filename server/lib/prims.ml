(* Tuna_server.Prims: the v1 prim registry + handlers (M7).  The
   registry and every pure handler live in Tuna_prims.Prim_core (shared
   with the local run host, borg/local.borg L3); this module adds the
   Eio http/get and keeps the server's dispatch signature. *)

open Tuna_store.Direct
include Tuna_prims.Prim_core

(* Minimal HTTP/1.0 GET over Eio.Net.  http only (v0).  DNS resolution
   goes through Eio's getaddrinfo family, which eio_posix runs in a
   systhread, so a slow or blackholed resolver can no longer stall the
   whole Eio domain scheduler (F2, audit 10-28).  The ambient eio ctx
   (installed by the server mainloop) supplies the switch + net. *)
let http_get ~allowlist url : answer =
  match parse_url url with
  | None -> `Error "http/get: only http:// URLs are supported in v0"
  | Some (host, port, path) -> (
      if not (List.exists (fun h -> String.equal h host) allowlist) then
        `Error "http/get: host not in allowlist"
      else
        let ctx = Tuna_store.Pgx_eio.Ctx.get () in
        let addr =
          match
            Eio.Net.getaddrinfo_stream ~service:(string_of_int port)
              ctx.Tuna_store.Pgx_eio.Ctx.net host
          with
          | addr :: _ -> Some addr
          | [] -> None
          | exception Eio.Io _ -> None
        in
        match addr with
        | None -> `Error ("http/get: cannot resolve host " ^ host)
        | Some sockaddr ->
            let flow =
              (Eio.Net.connect ~sw:ctx.Tuna_store.Pgx_eio.Ctx.sw
                 ctx.Tuna_store.Pgx_eio.Ctx.net sockaddr
                :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
            in
            Fun.protect
              ~finally:(fun () -> Eio.Flow.close flow)
              (fun () ->
                let req =
                  Printf.sprintf
                    "GET %s HTTP/1.0\r\nHost: %s\r\nConnection: close\r\nUser-Agent: tuna-prim/1\r\n\r\n"
                    path host
                in
                Eio.Flow.copy_string req flow;
                let raw =
                  Eio.Buf_read.parse_exn ~max_size:(payload_cap + 65536)
                    Eio.Buf_read.take_all flow
                in
                let body =
                  let sep = "\r\n\r\n" in
                  let slen = String.length sep in
                  let rec find i =
                    if i + slen > String.length raw then None
                    else if String.sub raw i slen = sep then Some i
                    else find (i + 1)
                  in
                  match find 0 with
                  | Some i ->
                      String.sub raw (i + slen) (String.length raw - i - slen)
                  | None -> raw
                in
                if String.length body > payload_cap then
                  `Error "http/get: body exceeds the journal payload cap"
                else `Ok (str body)))

let dispatch ~name ~args ~kv ~allowlist : answer =
  Tuna_prims.Prim_core.dispatch ~name ~args ~kv ~http_get:(http_get ~allowlist)
