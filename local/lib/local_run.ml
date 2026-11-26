(* Tuna_local.Local_run: the local run host (borg/local.borg L3).

   It drives the pure step core (Flat_drive, as the server's Run does)
   and answers prim calls from the local registry with the server's own
   handlers (Tuna_prims.Prim_core): echo, now, uuid, store/get and
   store/put over the local directory, http/get only to hosts the caller
   allowed, and the math prims.  Substrate prims (the tree, ns and
   value families) belong to a server store and answer an error here.

   Grants are arguments: the run carries the prim names it may call
   ("*" for any), checked at every call.  A missing grant is a
   journaled error answer, never an exception - the server's denial
   text, so a replay of either reads the same.

   Every call is journaled in the server's row shape (seq from 0,
   genesis 64 zeros, row_hash = Store.row_hash via
   Tuna_deriv.Deriv.Journal.row_fingerprint), and the finished run is
   sealed as a derivation record (Tuna_deriv.Deriv.build) that
   tools/deriv-check verifies with no server.  The record's bytes are
   stored by hash and a put at runs/<run-id> in the op log cites them,
   so the record travels with an L6 push. *)

module D = Tuna_deriv.Deriv
module P = Tuna_prims.Prim_core
module S = Local_store

module Eng = Tuna_interp.Flat_drive.Make (struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let catch f h = try f () with e -> h e
end)

type config = {
  grants : string list;  (* prim names, "*" = any *)
  allow_hosts : string list;  (* http/get egress, exact host *)
  semantics : string;  (* v0 | v1 | fast1-fast3 *)
  fuel : int;
  size_cap : int;
}

type run = {
  record : D.t;
  record_hash : string;  (* sha256 of the record's bytes *)
}

(* prims the local registry answers; the rest are server-store prims *)
let local_prims =
  [ "echo"; "now"; "uuid"; "store/get"; "store/put"; "http/get";
    "math/add"; "math/sub"; "math/mul"; "math/cmp"; "math/neg" ]

let semantics_ok s =
  s = "v0" || s = "v1" || Tuna_interp.Accel.registry_of_semantics s <> None

(* -- http/get over a plain socket (HTTP/1.0, http only, as the server) -- *)

let http_get ~allow_hosts url : P.answer =
  match P.parse_url url with
  | None -> `Error "http/get: only http:// URLs are supported in v0"
  | Some (host, port, path) -> (
      if not (List.mem host allow_hosts) then `Error "http/get: host not in allowlist"
      else
        (* resolution and socket creation can fail too: they are inside
           the same answer-not-raise boundary as the exchange *)
        try
          match
            Unix.getaddrinfo host (string_of_int port)
              [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
          with
          | [] -> `Error ("http/get: cannot resolve host " ^ host)
          | ai :: _ -> (
              let sock = Unix.socket ai.Unix.ai_family Unix.SOCK_STREAM 0 in
              Fun.protect ~finally:(fun () -> Unix.close sock) @@ fun () ->
              Unix.setsockopt_float sock Unix.SO_RCVTIMEO 10.;
              Unix.setsockopt_float sock Unix.SO_SNDTIMEO 10.;
              Unix.connect sock ai.Unix.ai_addr;
              let req =
                Printf.sprintf
                  "GET %s HTTP/1.0\r\nHost: %s\r\nConnection: close\r\nUser-Agent: tuna-prim/1\r\n\r\n"
                  path host
              in
              ignore (Unix.write_substring sock req 0 (String.length req));
              let max = P.payload_cap + 65536 in
              let buf = Buffer.create 4096 and chunk = Bytes.create 4096 in
              let rec read () =
                let n = Unix.read sock chunk 0 (Bytes.length chunk) in
                if n > 0 && Buffer.length buf <= max then (
                  Buffer.add_subbytes buf chunk 0 n;
                  read ())
              in
              read ();
              let raw = Buffer.contents buf in
              let body =
                let sep = "\r\n\r\n" in
                let rec find i =
                  if i + 4 > String.length raw then None
                  else if String.sub raw i 4 = sep then Some i
                  else find (i + 1)
                in
                match find 0 with
                | Some i -> String.sub raw (i + 4) (String.length raw - i - 4)
                | None -> raw
              in
              if String.length body > P.payload_cap then
                `Error "http/get: body exceeds the journal payload cap"
              else `Ok (P.str body))
        with Unix.Unix_error (e, _, _) ->
          `Error ("http/get: " ^ Unix.error_message e))

(* -- the run --------------------------------------------------------- *)

let status_of = function
  | Eng.Normal (t, s) -> (D.Normal, Some (Tuna.Canon.encode t), s)
  | Eng.Loop s -> (D.Loop, None, s)
  | Eng.Fuel_exhausted s -> (D.Fuel_exhausted, None, s)
  | Eng.Size_exhausted s -> (D.Size_exhausted, None, s)
  | Eng.Deadline_exceeded _ ->
      (* the local host sets no deadline: evaluation is untimed *)
      failwith "deadline_exceeded without a deadline"

(* what one evaluation did, before any record is built *)
type evaluated = {
  ev_run_id : string;
  ev_status : D.status;
  ev_result : Tuna.Tree.t option;
  ev_result_ternary : string option;
  ev_steps : int;
  ev_journal : D.Journal.t list;
}

(* [tags]: the compiled artifact's (tree path, IR id) provenance, so a
   journal row names its callsite as the server's does; [] for a raw
   ternary program (the server's "" as well).  The repl calls this
   directly: its rounds answer prims under grants but seal no record, so
   L2's op-log shape is unchanged. *)
let eval_run st (cfg : config) ?(tags = []) ~program inputs : evaluated =
  if not (semantics_ok cfg.semantics) then
    failwith (Printf.sprintf "unknown semantics %S" cfg.semantics);
  let run_id = P.uuid () in
  let site_path site =
    match List.find_opt (fun (_, id) -> id = site) tags with
    | Some (path, _) -> String.concat "" (List.map string_of_int path)
    | None -> ""
  in
  let kv = { P.kv_get = S.kv_get st; kv_put = (fun ~key ~value -> S.kv_put st ~key ~value) } in
  let granted name = List.mem name cfg.grants || List.mem "*" cfg.grants in
  let grant_id name =
    if List.mem name cfg.grants then "local:" ^ name else "local:*"
  in
  let journal = ref [] and prev = ref D.genesis and seq = ref 0 in
  let host ~site ~name ~args =
    let t0 = Unix.gettimeofday () in
    let args_ternary = Tuna.Canon.encode args in
    let answer, gid, args_inline =
      if String.length args_ternary > P.payload_cap then
        (`Error (Printf.sprintf "prim %s: args exceed the journal payload cap" name), None, None)
      else if not (granted name) then
        (`Error (Printf.sprintf "grant denial: no live grant for prim %s" name), None,
         Some args_ternary)
      else if not (List.mem name local_prims) then
        (`Error (Printf.sprintf "prim %s: not available in the local host" name),
         Some (grant_id name), Some args_ternary)
      else
        ( P.dispatch ~name ~args ~kv ~http_get:(http_get ~allow_hosts:cfg.allow_hosts),
          Some (grant_id name), Some args_ternary )
    in
    let result_ternary = match answer with `Ok t -> Some (Tuna.Canon.encode t) | `Error _ -> None in
    let row0 =
      { D.Journal.j_seq = !seq; j_callsite_path = site_path site; j_prim = name;
        j_prim_contract = P.contract; j_grant_id = gid; j_args_ternary = args_inline;
        j_args_hash = Option.map Tuna.Hash.hex_of_string args_inline;
        j_result_ternary = result_ternary;
        j_result_hash = Option.map Tuna.Hash.hex_of_string result_ternary;
        j_error = (match answer with `Ok _ -> None | `Error e -> Some e);
        j_wall_ms = Some (int_of_float ((Unix.gettimeofday () -. t0) *. 1000.));
        j_host_build = "tuna-local"; j_prev_hash = !prev; j_row_hash = "" }
    in
    let row = { row0 with D.Journal.j_row_hash = D.Journal.row_fingerprint ~run_id row0 } in
    journal := row :: !journal;
    prev := row.D.Journal.j_row_hash;
    incr seq;
    answer
  in
  let accel = Tuna_interp.Accel.registry_of_semantics cfg.semantics in
  let mode = if cfg.semantics = "v1" then Eng.Sharing else Eng.Canonical in
  let outcome =
    Eng.eval ~host ~mode ?accel ~fuel:cfg.fuel ~size_cap:cfg.size_cap ~program inputs
  in
  let status, result_ternary, steps = status_of outcome in
  let result = match outcome with Eng.Normal (t, _) -> Some t | _ -> None in
  { ev_run_id = run_id; ev_status = status; ev_result = result;
    ev_result_ternary = result_ternary; ev_steps = steps;
    ev_journal = List.rev !journal }

(* [parent_run_id] links a run to the one it continues (a repl session's
   previous round, on the server's model) *)
let execute st (cfg : config) ?(tags = []) ?parent_run_id ~program inputs : run =
  let e = eval_run st cfg ~tags ~program inputs in
  let program_ternary = Tuna.Canon.encode program in
  let input_ternaries = List.map Tuna.Canon.encode inputs in
  let record =
    D.build ~run_id:e.ev_run_id ~semantics:cfg.semantics
      ~program_hash:(Tuna.Hash.hex_of_string program_ternary) ~program_ternary
      ~input_hashes:(List.map Tuna.Hash.hex_of_string input_ternaries)
      ~input_ternaries ~fuel:cfg.fuel ~size_cap:cfg.size_cap ~status:e.ev_status
      ~result_ternary:e.ev_result_ternary
      ~result_hash:(Option.map Tuna.Hash.hex_of_string e.ev_result_ternary)
      ~step_count:e.ev_steps ~parent_run_id ~journal:e.ev_journal
  in
  (* seal: the record's bytes by hash, cited from the op log *)
  let bytes = D.to_string record in
  let record_hash =
    S.with_lock st (fun () ->
        ignore (S.put_value st program);
        List.iter (fun t -> ignore (S.put_value st t)) inputs;
        let h = S.put_bytes st bytes in
        ignore (S.append st ~path:("runs/" ^ e.ev_run_id) ~op:"put" ~value_hash:(Some h) ());
        h)
  in
  { record; record_hash }
