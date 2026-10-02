(* Tuna_server.Prims: the v1 prim registry + handlers (M7).

   The calculus has no I/O; a run touches the world only through prim
   calls (Tuna.Cprim convention), answered by the host at a boundary
   that checks the grant row live, journals the event, and constrains
   payloads (journal.borg row-schema, grants.grant-token).

   Contract discipline (replay.prim-versioning): every prim pins
   prim_contract "2".  A prim that changes behavior without a contract
   bump is definitionally cheating; replay of contract "2" rows is
   unconditional either way — the journal pins what the host RETURNED.
   v2 = the string codec moved to the binary (Barry Jay) convention
   (Cstr), so every string payload (now/uuid/http-get results, paths,
   error messages) changed tree shape vs contract "1"; "1" rows stay
   self-consistent but are not cross-comparable with "2".

   Handlers here only compute; the boundary (Run.execute) does grant
   checks, timing, journaling, and payload caps around them. *)

open Tuna_store.Direct

type answer = [ `Ok of Tuna.Tree.t | `Error of string ]

let contract = "2"

let names =
  [ "echo"
  ; "now"
  ; "uuid"
  ; "store/get"
  ; "store/put"
  ; "http/get"
    (* M10 substrate prims; handlers live in Tree_prims, dispatched by
       the run boundary *)
  ; "tree/get"
  ; "tree/put"
  ; "tree/cas"
  ; "tree/list"
    (* M11 tree/del: path deletion through the run boundary - tree/put's
       covering-prefix rule with admins exempt; paths only, the value
       store is immutable *)
  ; "tree/del"
  ; "ns/fork"
    (* M11 byte-value prims; handlers live in Value_prims, dispatched by
       the run boundary under byte-values.borg law 2 (reads are
       hash-gated, puts capability-gated outside the grant map) *)
  ; "value/put"
  ; "value/get"
  ; "value/len"
    (* math prim family (borg/math-prims.borg; the F6 consequence):
       host-mirrored law-5 int ops, O(bits) per call, additive under
       contract "2" - no payload shape changes anywhere *)
  ; "math/add"
  ; "math/sub"
  ; "math/mul"
  ; "math/cmp"
  ; "math/neg" ]

let exists name = List.mem name names

(* Journal payload cap: a prim call whose inline payload (args or
   result ternary) exceeds this is journaled as an error instead of a
   blob spill (journal.row-schema's growth open question, v0 stance). *)
let payload_cap = 65_536

(* -- tree/list helpers (args convention: the prim's args tree is a
   LIST — nil = Leaf, cons a rest = Fork (a, rest)) ------------------- *)

let list_of_tree (t : Tuna.Tree.t) : Tuna.Tree.t list =
  let rec go t acc =
    match t with
    | Tuna.Tree.Fork (a, rest) -> go rest (a :: acc)
    | Tuna.Tree.Leaf -> List.rev acc
    | stem -> [ stem ] (* degenerate: not a list, return as sole elem *)
  in
  go t []

let tree_of_list (l : Tuna.Tree.t list) : Tuna.Tree.t =
  List.fold_right (fun a acc -> Tuna.Tree.Fork (a, acc)) l Tuna.Tree.Leaf

let str = Tuna.Cstr.encode

let unstr (t : Tuna.Tree.t) : string option = Tuna.Cstr.decode t

(* -- uuid v4 ---------------------------------------------------------- *)

let uuid () =
  let ic = open_in_bin "/dev/urandom" in
  let raw = really_input_string ic 16 in
  close_in ic;
  let b = Bytes.of_string raw in
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let hex = Buffer.create 36 in
  String.iter
    (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c)))
    (Bytes.to_string b);
  let h = Buffer.contents hex in
  Printf.sprintf "%s-%s-%s-%s-%s" (String.sub h 0 8) (String.sub h 8 4)
    (String.sub h 12 4) (String.sub h 16 4) (String.sub h 20 12)

(* -- http/get (allowlist egress) -------------------------------------- *)

(* TUNA_HTTP_ALLOWLIST: comma-separated host names; unset/empty means
   deny all egress.  Matching is exact host. *)
let allowlist_from_env () =
  match Sys.getenv_opt "TUNA_HTTP_ALLOWLIST" with
  | Some s ->
      String.split_on_char ',' s
      |> List.map String.trim
      |> List.filter (fun h -> h <> "")
  | None -> []

(* Parse "http://host[:port]/path" into (host, port, request-target).
   v0: http only. *)
let parse_url (u : string) : (string * int * string) option =
  let s = String.trim u in
  let prefix = "http://" in
  if String.length s < 8 || String.sub s 0 7 <> prefix then None
  else
    let rest = String.sub s 7 (String.length s - 7) in
    let hostport, path =
      match String.index_opt rest '/' with
      | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
      | None -> (rest, "/")
    in
    if hostport = "" then None
    else
      match String.index_opt hostport ':' with
      | Some i -> (
          let p =
            match int_of_string_opt (String.sub hostport (i + 1) (String.length hostport - i - 1)) with
            | Some p when p > 0 && p <= 65535 -> Some p
            | _ -> None
          in
          match p with
          | Some port ->
              Some (String.sub hostport 0 i, port, path)
          | None -> None)
      | None -> Some (hostport, 80, path)

(* Minimal HTTP/1.0 GET over Eio.Net.  http only (v0); DNS resolution
   is a synchronous stdlib call — v0 accepts the fiber stall for
   allowlisted dev hosts.  The ambient eio ctx (installed by the server
   mainloop) supplies the switch + net. *)
let http_get ~allowlist url : answer =
  match parse_url url with
  | None -> `Error "http/get: only http:// URLs are supported in v0"
  | Some (host, port, path) -> (
      if not (List.exists (fun h -> String.equal h host) allowlist) then
        `Error "http/get: host not in allowlist"
      else
        let addr =
          (try Some (Unix.gethostbyname host).Unix.h_addr_list.(0)
           with Not_found -> None)
        in
        match addr with
        | None -> `Error ("http/get: cannot resolve host " ^ host)
        | Some inet ->
            let ctx = Tuna_store.Pgx_eio.Ctx.get () in
            let sockaddr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
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

(* -- kv accessors (wired to the store by the boundary) ---------------- *)

type kv = {
  kv_get : Tuna.Tree.t -> Tuna.Tree.t option
    (* None = no value for key *)
; kv_put : key:Tuna.Tree.t -> value:Tuna.Tree.t -> unit
}

(* Dispatch one prim call.  [name] must come from the gate shape
   (Cprim.shape); an unregistered name is an error ANSWER, journaled
   like any denial — never an exception. *)
let dispatch ~name ~args ~kv ~allowlist : answer =
  match name with
  | "echo" -> return (`Ok args)
  | "now" -> return (`Ok (str (Printf.sprintf "%.6f" (Unix.gettimeofday ()))))
  | "uuid" -> return (`Ok (str (uuid ())))
  | "store/get" -> (
      match list_of_tree args with
      | key :: _ -> (
          kv.kv_get key
          >>= function
          | Some value -> return (`Ok value)
          | None ->
              return
                (`Error
                   ("store/get: no value for key "
                    ^ Tuna.Hash.hex_of_tree key)))
      | [] -> return (`Error "store/get: missing key argument"))
  | "store/put" -> (
      match list_of_tree args with
      | [ key; value ] -> (
          if String.length (Tuna.Canon.encode value) > payload_cap then
            return (`Error "store/put: value exceeds the journal payload cap")
          else
            kv.kv_put ~key ~value >>= fun () -> return (`Ok Tuna.Tree.Leaf))
      | _ -> return (`Error "store/put: args must be [key value]"))
  | "http/get" -> (
      match list_of_tree args with
      | url :: _ -> (
          match unstr url with
          | Some u -> http_get ~allowlist u
          | None -> return (`Error "http/get: url must be a string tree"))
      | [] -> return (`Error "http/get: missing url argument"))
  | "math/add" -> (
      match list_of_tree args with
      | [ a; b ] -> return (`Ok (Math_prims.add a b))
      | _ -> return (`Error "math/add: args must be [a b]"))
  | "math/sub" -> (
      match list_of_tree args with
      | [ a; b ] -> return (`Ok (Math_prims.sub a b))
      | _ -> return (`Error "math/sub: args must be [a b]"))
  | "math/mul" -> (
      match list_of_tree args with
      | [ a; b ] -> return (`Ok (Math_prims.mul a b))
      | _ -> return (`Error "math/mul: args must be [a b]"))
  | "math/cmp" -> (
      match list_of_tree args with
      | [ a; b ] -> return (`Ok (Math_prims.cmp a b))
      | _ -> return (`Error "math/cmp: args must be [a b]"))
  | "math/neg" -> (
      match list_of_tree args with
      | [ a ] -> return (`Ok (Math_prims.neg a))
      | _ -> return (`Error "math/neg: args must be [a]"))
  | other -> return (`Error ("unknown prim: " ^ other))
