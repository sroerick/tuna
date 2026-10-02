(* Tuna_server.Web: the direct-style HTTP floor (rim-eio §http).

   Dream departs; [httpun_eio] arrives.  This module is the bounded
   adaptation layer the chapter names: request/response records,
   form/cookie/query decoding, the router table, and the native Eio
   accept loop.  Handlers are DIRECT STYLE — [req -> resp], no monad,
   no promise, no bridge (family law).  The shapes are ported from PP's
   http_kit / http_core family (same owner, ISC); the Dream surface this
   replaces was already enumerated and small.

   The htmx pages render byte-equivalently: this is plumbing, not a
   redesign.  Body reads are capped (the PP max_body_bytes stance). *)

(* -- leaf text helpers (ported from PP http_kit.ml) ------------------ *)

let hex_nibble (c : char) : int option =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

(* strict percent-decoding: '+'=>space, malformed escapes preserved *)
let pct_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '%' && i + 2 < len then (
      match (hex_nibble s.[i + 1], hex_nibble s.[i + 2]) with
      | Some a, Some b ->
          Buffer.add_char buf (Char.chr ((a lsl 4) lor b));
          loop (i + 3)
      | _ ->
          Buffer.add_char buf s.[i];
          loop (i + 1))
    else if s.[i] = '+' then (
      Buffer.add_char buf ' ';
      loop (i + 1))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

let url_encode (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~') as c ->
          Buffer.add_char buf c
      | ' ' -> Buffer.add_string buf "%20"
      | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

(* case-insensitive header lookup *)
let header_ci (headers : (string * string) list) (name : string) : string option =
  let lower = String.lowercase_ascii name in
  List.find_map
    (fun (k, v) -> if String.lowercase_ascii k = lower then Some v else None)
    headers

let html_escape (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* -- request / response --------------------------------------------- *)

type req = {
  meth : string;                     (* uppercase HTTP method *)
  target : string;
  headers : (string * string) list;
  body : string;
  mutable captures : (string * string) list;
}

type resp = {
  mutable status : int;
  mutable headers : (string * string) list;
  body : string;
}

let meth_code (r : req) = r.meth

(* Dream-compatible shims: [method_] returns the string and
   [method_to_string] is the identity (Dream's had a variant type). *)
let method_ (r : req) = r.meth
let method_to_string (s : string) = s

let target (r : req) = r.target
let all_headers (r : req) = r.headers
let header (r : req) name = header_ci r.headers name
let body (r : req) = r.body

let param (r : req) name =
  match List.assoc_opt name r.captures with Some v -> v | None -> ""

(* first query value for [key] in the request target's query *)
let query (r : req) (key : string) : string option =
  let target = r.target in
  match String.index_opt target '?' with
  | None -> None
  | Some i ->
      let q = String.sub target (i + 1) (String.length target - i - 1) in
      let pairs = String.split_on_char '&' q in
      List.find_map
        (fun pair ->
          match String.split_on_char '=' pair with
          | k :: rest when pct_decode k = key ->
              Some (pct_decode (String.concat "=" rest))
          | _ -> None)
        pairs

(* -- responses ------------------------------------------------------ *)

let respond ?(code = 200) ?(headers = []) body =
  { status = code; headers; body }

let html ?(code = 200) body =
  { status = code
  ; headers = [ ("Content-Type", "text/html; charset=utf-8") ]
  ; body }

let json ?(code = 200) body =
  { status = code
  ; headers = [ ("Content-Type", "application/json") ]
  ; body }

let text ?(code = 200) body =
  { status = code
  ; headers = [ ("Content-Type", "text/plain; charset=utf-8") ]
  ; body }

(* 303 See Other — the browser redirect Dream used *)
let redirect (r : req) (loc : string) =
  ignore r;
  { status = 303
  ; headers = [ ("Location", loc) ]
  ; body = "" }

(* -- cookies -------------------------------------------------------- *)

let cookie (r : req) ~(decrypt : bool) (name : string) : string option =
  ignore decrypt;
  match header_ci r.headers "cookie" with
  | None -> None
  | Some raw ->
      let pairs = String.split_on_char ';' raw in
      List.find_map
        (fun pair ->
          let pair = String.trim pair in
          match String.index_opt pair '=' with
          | None -> None
          | Some i ->
              let k = String.sub pair 0 i in
              let v = String.sub pair (i + 1) (String.length pair - i - 1) in
              if k = name then Some v else None)
        pairs

let set_cookie (resp : resp) (_r : req) ~(encrypt : bool) ~(http_only : bool)
    ~(same_site : [ `Lax | `Strict | `None ] option) ~(path : string option)
    (name : string) (value : string) : unit =
  ignore encrypt;
  let attrs =
    [ (if http_only then "; HttpOnly" else "")
    ; (match same_site with
       | Some `Lax -> "; SameSite=Lax"
       | Some `Strict -> "; SameSite=Strict"
       | Some `None -> "; SameSite=None"
       | None -> "")
    ; (match path with Some p -> "; Path=" ^ p | None -> "")
    ]
  in
  let cookie = name ^ "=" ^ value ^ String.concat "" attrs in
  resp.headers <- ("Set-Cookie", cookie) :: resp.headers

let drop_cookie (resp : resp) (_r : req) (name : string) : unit =
  resp.headers <-
    ("Set-Cookie", name ^ "=; Path=/; Max-Age=0") :: resp.headers

(* -- form decoding -------------------------------------------------- *)

let form ?(csrf = true) (r : req)
    : [ `Ok of (string * string) list | `Wrong_content_type ] =
  ignore csrf;
  let ctype = Option.value (header_ci r.headers "content-type") ~default:"" in
  let base =
    match String.index_opt ctype ';' with
    | Some i -> String.sub ctype 0 i
    | None -> ctype
  in
  if String.trim (String.lowercase_ascii base)
     <> "application/x-www-form-urlencoded"
  then `Wrong_content_type
  else
    let pairs =
      String.split_on_char '&' r.body
      |> List.filter (fun p -> p <> "")
      |> List.map (fun pair ->
             match String.index_opt pair '=' with
             | None -> (pct_decode pair, "")
             | Some i ->
                 ( pct_decode (String.sub pair 0 i)
                 , pct_decode
                     (String.sub pair (i + 1)
                        (String.length pair - i - 1)) ))
    in
    `Ok pairs

(* -- logging -------------------------------------------------------- *)

let log fmt = Printf.ksprintf (fun s -> prerr_endline ("[tuna] " ^ s)) fmt

(* -- router --------------------------------------------------------- *)

type handler = req -> resp

type route = route_method * string * handler

and route_method = RMeth of string | RAny

let get path h = (RMeth "GET", path, h)
let post path h = (RMeth "POST", path, h)
let any path h = (RAny, path, h)

let route_matches meth m =
  match m with RMeth m -> String.equal m meth | RAny -> true

(* match a path template with :name captures and a trailing ** *)
let rec match_path (pat : string) (path : string) (acc : (string * string) list)
    : (string * string) list option =
  if pat = "" then if path = "" then Some (List.rev acc) else None
  else
    let pat_seg, pat_rest =
      match String.index_opt pat '/' with
      | Some i -> (String.sub pat 0 i, String.sub pat (i + 1) (String.length pat - i - 1))
      | None -> (pat, "")
    in
    if pat_seg = "**" then
      Some (List.rev (("**", path) :: acc))
    else
      let path_seg, path_rest =
        match String.index_opt path '/' with
        | Some i -> (String.sub path 0 i, String.sub path (i + 1) (String.length path - i - 1))
        | None -> (path, "")
      in
      let ok =
        if pat_seg = "" && path_seg = "" then true
        else if String.length pat_seg > 0 && pat_seg.[0] = ':' then true
        else String.equal pat_seg path_seg
      in
      if not ok then None
      else
        let acc =
          if String.length pat_seg > 0 && pat_seg.[0] = ':' then
            ( String.sub pat_seg 1 (String.length pat_seg - 1)
            , path_seg )
            :: acc
          else acc
        in
        match_path pat_rest path_rest acc

(* split "/foo/bar" -> ["foo"; "bar"], trimming a leading slash *)
let split_segments (path : string) : string =
  if String.length path > 0 && path.[0] = '/' then
    String.sub path 1 (String.length path - 1)
  else path

let router (routes : route list) (r : req) : resp =
  let path = split_segments (match String.index_opt r.target '?' with
    | Some i -> String.sub r.target 0 i
    | None -> r.target)
  in
  let rec go = function
    | [] ->
        { status = 404; headers = [ ("Content-Type", "text/plain") ]
        ; body = "not found" }
    | (m, pat, h) :: rest ->
        if route_matches r.meth m then
          match match_path (split_segments pat) path [] with
          | Some caps ->
              r.captures <- caps;
              h r
          | None -> go rest
        else go rest
  in
  go routes

let logger (inner : handler) (r : req) : resp =
  let resp = inner r in
  Printf.eprintf "[tuna] %s %s -> %d\n%!" r.meth r.target resp.status;
  resp

(* -- static files --------------------------------------------------- *)

let content_type_of (name : string) : string =
  match String.lowercase_ascii (Filename.extension name) with
  | ".js" -> "application/javascript"
  | ".css" -> "text/css"
  | ".html" -> "text/html; charset=utf-8"
  | ".txt" -> "text/plain; charset=utf-8"
  | ".json" -> "application/json"
  | ".png" -> "image/png"
  | ".svg" -> "image/svg+xml"
  | _ -> "application/octet-stream"

let read_file (path : string) : string option =
  match open_in_bin path with
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> Some (really_input_string ic (in_channel_length ic)))
  | exception Sys_error _ -> None

(* serve /static/** from [dir] (the [**] capture holds the sub-path) *)
let static (dir : string) (r : req) : resp =
  let sub = param r "**" in
  let rel =
    if String.length sub > 0 && sub.[0] = '/' then
      String.sub sub 1 (String.length sub - 1)
    else sub
  in
  (* reject traversal *)
  let bad =
    String.split_on_char '/' rel
    |> List.exists (fun seg -> seg = ".." || seg = "" && rel <> "")
  in
  if bad then { status = 404; headers = []; body = "not found" }
  else
    let file = Filename.concat dir rel in
    match read_file file with
    | Some body ->
        { status = 200; headers = [ ("Content-Type", content_type_of file) ]
        ; body }
    | None -> { status = 404; headers = []; body = "not found" }

(* [from_filesystem dir file] serves exactly one file *)
let from_filesystem (dir : string) (file : string) (_r : req) : resp =
  match read_file (Filename.concat dir file) with
  | Some body ->
      { status = 200; headers = [ ("Content-Type", content_type_of file) ]
      ; body }
  | None -> { status = 404; headers = []; body = "not found" }

(* -- native Eio accept loop ----------------------------------------- *)

exception Body_too_large

let max_body_bytes =
  match Sys.getenv_opt "TUNA_MAX_BODY_BYTES" with
  | Some s -> (
      match int_of_string_opt s with Some n when n > 0 -> n | _ -> 1024 * 1024)
  | None -> 1024 * 1024

let read_body ?(max_bytes = max_body_bytes) (reqd : Httpun.Reqd.t) : string =
  let body = Httpun.Reqd.request_body reqd in
  let promise, resolver = Eio.Promise.create () in
  let buf = Buffer.create 1024 in
  let rec sched () =
    Httpun.Body.Reader.schedule_read body
      ~on_eof:(fun () -> Eio.Promise.resolve resolver (`Ok (Buffer.contents buf)))
      ~on_read:(fun bs ~off ~len ->
        if Buffer.length buf + len > max_bytes then
          Eio.Promise.resolve resolver `Too_large
        else begin
          Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
          sched ()
        end)
  in
  sched ();
  match Eio.Promise.await promise with
  | `Ok s -> s
  | `Too_large -> raise Body_too_large

let is_closed_writer_exn = function
  | Failure msg ->
      String.equal msg "cannot write to closed writer"
      || String.starts_with ~prefix:"cannot write to closed writer" msg
  | _ -> false

let safe_respond (reqd : Httpun.Reqd.t) (r : resp) : unit =
  let headers = Httpun.Headers.of_list (("Content-Length", string_of_int (String.length r.body)) :: r.headers) in
  let status =
    try Httpun.Status.of_code r.status with _ -> `Internal_server_error
  in
  let resp = Httpun.Response.create ~headers status in
  try Httpun.Reqd.respond_with_string reqd resp r.body
  with exn when is_closed_writer_exn exn -> ()

(* [serve] runs the accept loop; blocks the calling fiber forever. *)
let serve ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t)
    ~(interface : string) ~(port : int) (handler : handler) : unit =
  let net = Eio.Stdenv.net env in
  let listen_addr =
    if interface = "" || interface = "0.0.0.0" then
      `Tcp (Eio.Net.Ipaddr.V4.any, port)
    else
      let ip = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string interface) in
      `Tcp (ip, port)
  in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:false ~backlog:128 ~sw net
      listen_addr
  in
  Printf.eprintf "[tuna] serving htmx (httpun-eio) on %s:%d\n%!" interface port;
  let error_handler (_sa : Eio.Net.Sockaddr.stream)
      : Httpun.Server_connection.error_handler =
    fun ?request:_ _e _respond -> ()
  in
  let request_handler (sa : Eio.Net.Sockaddr.stream)
      (g : Httpun.Reqd.t Gluten.reqd) : unit =
    ignore sa;
    let reqd = g.Gluten.reqd in
    let request = Httpun.Reqd.request reqd in
    let body = try read_body reqd with Body_too_large -> "" in
    let headers =
      Httpun.Headers.to_list request.Httpun.Request.headers
    in
    let req =
      { meth = Httpun.Method.to_string request.Httpun.Request.meth
      ; target = request.Httpun.Request.target
      ; headers
      ; body
      ; captures = [] }
    in
    let resp =
      try handler req
      with exn ->
        Printf.eprintf "[tuna] handler error: %s\n%!" (Printexc.to_string exn);
        { status = 500
        ; headers = [ ("Content-Type", "application/json") ]
        ; body = "{\"error\":\"internal server error\"}" }
    in
    safe_respond reqd resp
  in
  let conn_handler =
    Httpun_eio.Server.create_connection_handler
      ~request_handler ~error_handler ~sw
  in
  let on_error ex =
    Printf.eprintf "[tuna] connection handler failed: %s\n%!"
      (Printexc.to_string ex)
  in
  while true do
    Eio.Net.accept_fork socket ~sw ~on_error (fun client_sock client_addr ->
        try conn_handler client_addr client_sock with exn -> on_error exn)
  done
