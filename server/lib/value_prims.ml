(* Tuna_server.Value_prims: the M11 byte-value boundary prims
   (borg/byte-values.borg).

   value/put, value/get and value/len address the BYTE value store
   (byte_values, migration 0008): raw bytes content-addressed by sha256
   over the exact bytes - the same address law as tree_values, a
   different payload kind (ONE NAMESPACE, TWO KINDS).  Byte payloads
   travel base64 at this surface (printable-ASCII discipline, like
   paths); value/get kind=tree returns the ternary text of a tree value.

   Law 2 (HASH-GATED READS, CAPABILITY-GATED WRITES): value/get and
   value/len need only a valid bearer identity - the hash is the
   capability (unguessable, 256-bit); value/put requires the caller to
   hold at least one unrevoked grant, admins exempt.  That check is
   LIVE in the handler, NOT the run's grant map: the run boundary
   special-cases value prims for exactly this (Run.execute), so a run
   carrying no value/put grant can still put when its caller qualifies.

   Law 3 (CAPS, NOT EXCEPTIONS): the payload cap (TUNA_VALUE_MAX_BYTES,
   default 1 MiB) is a journaled denial answer, never a raised error.

   Contract discipline (replay.prim-versioning): prim_contract "1".
   Handlers journal their own SUCCESS op rows into the tree_ops chain
   (op names value-put / value-get / value-len; path = the hash); every
   ERROR answer (cap denials, law-2 denials, absent reads, bad base64)
   is journaled by the run boundary as a NULL-effect row, and the JSON
   surface journals its own rows the same way - acceptance 4: every
   value op journals, denial rows carry NULL effect.  Results above the
   journal payload cap (Prims.payload_cap) are error answers, like
   tree/list (the API surface serves big payloads out-of-band). *)

open Lwt.Infix

module S = Tuna_store.Store

type answer = [ `Ok of Tuna.Tree.t | `Error of string ]

let names = [ "value/put"; "value/get"; "value/len" ]

let exists name = List.mem name names

(* the tree_ops op kind of a value prim, None for everything else *)
let op_of_name = function
  | "value/put" -> Some "value-put"
  | "value/get" -> Some "value-get"
  | "value/len" -> Some "value-len"
  | _ -> None

(* the path a denial op row records: value/get + value/len address the
   hash (their first argument); value/put's hash does not exist until
   the payload decodes and stores, so its denial rows carry "". *)
let op_path_of_args name args =
  match name with
  | "value/put" -> ""
  | _ -> (
      match Prims.list_of_tree args with
      | t :: _ -> ( match Prims.unstr t with Some s -> s | None -> "" )
      | [] -> "" )

(* -- journaling (success rows only; the boundary journals denials) ----- *)

let journal p ~op ~path ~value_hash ~actor =
  S.op_append p ~op ~path ~value_hash ~prev_version:None ~version:None ~actor
  >>= fun _ -> Lwt.return ()

let b64_decode s = match Base64.decode s with Ok b -> Some b | Error _ -> None

let b64_encode s =
  match Base64.encode s with Ok b64 -> b64 | Error (`Msg m) -> failwith ("base64 encode: " ^ m)

(* result shape: [hash kind len payload] as string trees; payload is
   base64 for kind bytes, the ternary text for kind tree *)
let result_tree ~hash ~kind ~len ~payload =
  Prims.tree_of_list
    [ Prims.str hash
    ; Prims.str kind
    ; Prims.str (Int64.to_string len)
    ; Prims.str payload ]

(* law 2, write side: the caller holds at least one unrevoked grant,
   admins exempt; live check (revocation bites on the next call) *)
let put_allowed p actor =
  S.is_admin p actor >>= function
  | true -> Lwt.return true
  | false -> S.has_live_grant p actor

(* -- value/put: [base64-bytes] -> [hash kind len] ---------------------- *)

let value_put p ~actor args =
  match Prims.list_of_tree args with
  | [ payload_t ] -> (
      match Prims.unstr payload_t with
      | None -> Lwt.return (`Error "value/put: payload must be a base64 string tree")
      | Some b64 -> (
          match b64_decode b64 with
          | None -> Lwt.return (`Error "value/put: payload is not valid base64")
          | Some bytes -> (
              let len = Int64.of_int (String.length bytes) in
              let cap = Int64.of_int (S.value_max_bytes ()) in
              if len > cap then
                Lwt.return
                  (`Error
                     (Printf.sprintf
                        "value/put: payload of %Ld bytes exceeds the cap of %Ld" len cap))
              else
                put_allowed p actor
                >>= function
                | false ->
                    Lwt.return
                      (`Error "value/put: caller holds no unrevoked grant")
                | true ->
                    let hash = S.byte_hash bytes in
                    S.byte_value_put p ~hash ~bytes
                    >>= fun () ->
                    journal p ~op:"value-put" ~path:hash
                      ~value_hash:(Some hash) ~actor
                    >>= fun () ->
                    Lwt.return
                      (`Ok
                        (Prims.tree_of_list
                           [ Prims.str hash
                           ; Prims.str "bytes"
                           ; Prims.str (Int64.to_string len) ])))))
  | _ -> Lwt.return (`Error "value/put: args must be [base64-bytes]")

(* -- value/get: [hash] or [hash kind] -> [hash kind len payload] ------- *)

let fetch_kind p ~hash ~kind : S.probe option Lwt.t =
  match kind with
  | Some "bytes" ->
      S.byte_value_fetch p hash
      >>= (function Some b -> Lwt.return (Some (S.Bytes b)) | None -> Lwt.return None)
  | Some "tree" ->
      S.value_fetch p hash
      >>= (function Some t -> Lwt.return (Some (S.Tree t)) | None -> Lwt.return None)
  | _ -> S.probe_value p hash

let get p ~actor hash kind =
  match hash with
  | None -> Lwt.return (`Error "value/get: hash must be a string tree")
  | Some hash -> (
      let hash = Tuna.Hash.normalize_hex hash in
      match kind with
      | Some k when k <> "bytes" && k <> "tree" ->
          Lwt.return (`Error "value/get: kind must be \"bytes\" or \"tree\"")
      | _ -> (
          fetch_kind p ~hash ~kind
          >>= function
          | None -> Lwt.return (`Error ("value/get: no value with hash " ^ hash))
          | Some (S.Bytes bytes) ->
              let len = Int64.of_int (String.length bytes) in
              if len > Int64.of_int Prims.payload_cap then
                Lwt.return (`Error "value/get: value exceeds the journal payload cap")
              else
                journal p ~op:"value-get" ~path:hash ~value_hash:None ~actor
                >>= fun () ->
                Lwt.return
                  (`Ok
                    (result_tree ~hash ~kind:"bytes" ~len
                       ~payload:(b64_encode bytes)))
          | Some (S.Tree ternary) ->
              let len = Int64.of_int (String.length ternary) in
              if len > Int64.of_int Prims.payload_cap then
                Lwt.return (`Error "value/get: value exceeds the journal payload cap")
              else
                journal p ~op:"value-get" ~path:hash ~value_hash:None ~actor
                >>= fun () ->
                Lwt.return (`Ok (result_tree ~hash ~kind:"tree" ~len ~payload:ternary))))

let value_get p ~actor args =
  match Prims.list_of_tree args with
  | [ hash_t ] -> get p ~actor (Prims.unstr hash_t) None
  | [ hash_t; kind_t ] -> get p ~actor (Prims.unstr hash_t) (Prims.unstr kind_t)
  | _ -> Lwt.return (`Error "value/get: args must be [hash] or [hash kind]")

(* -- value/len: [hash] -> len ------------------------------------------ *)

let value_len p ~actor args =
  match Prims.list_of_tree args with
  | [ hash_t ] -> (
      match Prims.unstr hash_t with
      | None -> Lwt.return (`Error "value/len: hash must be a string tree")
      | Some hash ->
          let hash = Tuna.Hash.normalize_hex hash in
          S.probe_len p hash
          >>= function
          | None -> Lwt.return (`Error ("value/len: no value with hash " ^ hash))
          | Some len ->
              journal p ~op:"value-len" ~path:hash ~value_hash:None ~actor
              >>= fun () -> Lwt.return (`Ok (Prims.str (Int64.to_string len))))
  | _ -> Lwt.return (`Error "value/len: args must be [hash]")

(* -- dispatch ----------------------------------------------------------- *)

let dispatch ~pool ~actor ~name ~args : answer Lwt.t =
  match name with
  | "value/put" -> value_put pool ~actor args
  | "value/get" -> value_get pool ~actor args
  | "value/len" -> value_len pool ~actor args
  | other -> Lwt.return (`Error ("unknown prim: " ^ other))
