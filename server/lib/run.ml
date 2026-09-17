(* Tuna_server.Run: the run boundary (M7).

   v0 semantics (SPEC.md §6, journal.borg, grants.borg): POST /api/runs
   inserts the run row, then evaluates SYNCHRONOUSLY under the
   Prim_eval engine — the one verbatim triage port, instantiated in the
   Lwt monad so the host can answer prim calls at the boundary:

     - every prim call is checked against the run's grant ids LIVE
       (Store.check_grant: exists, unrevoked, belongs to caller —
       re-checked per call, so mid-run revocation bites), and the
       grant's args_attenuation predicate is interpreted (v0: null/{}
       admits everything; {"max_ternary": N} caps the encoded args
       length).  Denial is a JOURNALED ERROR ANSWER — the program
       receives the canonical error tree and keeps computing; nothing
       raises out of the calculus (AGENTS.md rule 7).
     - every boundary event is journaled (Store.append_journal) with
       the callsite path resolved from the program's provenance tags
       (call-sites.provenance): the prim's site is the IR node id of
       the (prim ...) form; the compiled artifact's tags map it to a
       tree path.  A call that cannot be resolved (bare-ternary
       program, provenance-less row) journals callsite_path "".
     - payloads above Prims.payload_cap are never inlined: the call is
       answered (and journaled) as an error instead (journal.row-schema
       growth stance, v0).

   The default host in a pure Eval run answers nothing — prim calls
   only exist inside a run. *)

open Lwt.Infix

module J = Yojson.Basic
module Eng = Tuna_interp.Prim_eval.Make (Lwt)
module S = Tuna_store.Store

(* -- provenance: site (IR node id) -> callsite tree path -------------- *)

(* The ir column carries {"kind":"compiled","steps":n,"tags":[
   {"path":"<digits>","ir":<node-id>,"span":{"off":..,"len":..}}, ...]} —
   one entry per compiled-tree node.  The gate literal of a prim call is
   tagged with the Prim IR node's id, so the callsite tree path is the
   path of the first entry carrying it (bracket abstraction may
   duplicate a callsite; v0 takes the first). *)
let site_paths (ir_json : string option) : (int * string) list =
  match ir_json with
  | None -> []
  | Some s -> (
      match J.from_string s with
      | `Assoc kvs -> (
          match List.assoc_opt "tags" kvs with
          | Some (`List tags) ->
              List.filter_map
                (function
                  | `Assoc kv -> (
                      let path =
                        match List.assoc_opt "path" kv with
                        | Some (`String p) -> Some p
                        | _ -> None
                      in
                      let ir =
                        match List.assoc_opt "ir" kv with
                        | Some (`Int i) -> Some i
                        | _ -> None
                      in
                      match (path, ir) with
                      | Some p, Some i -> Some (i, p)
                      | _ -> None)
                  | _ -> None)
                tags
          | _ -> [])
      | _ -> []
      | exception _ -> [])

(* -- grants by prim -------------------------------------------------- *)

(* Grant rows for the run: prim name -> (grant id, attenuation json
   text).  First id minted for a prim wins; submission already
   validated every id live against the grants table. *)
  let grant_map pool grant_ids =
    let rec go acc = function
      | [] -> Lwt.return acc
      | gid :: rest -> (
          S.fetch_grant pool gid
          >>= function
          | None -> go acc rest
          | Some g -> go ((g.S.g_prim, (gid, g.S.g_args_attenuation)) :: acc) rest)
    in
    go [] grant_ids

  (* M11 routes.borg law 3: route dispatch mints prim-"*" grants scoped
     to a record's grant_prefix - they answer ANY prim, still live-
     checked (unrevoked, caller, path prefix) at every call like any
     other grant. *)
  let grant_for gmap name =
    match List.assoc_opt name gmap with
    | Some g -> Some g
    | None -> List.assoc_opt "*" gmap

(* v0 attenuation predicate interpretation (grants.grant-token: host
   policy, stored not computed). *)
let attenuation_ok (args_attenuation : string) (args_ternary : string) : bool =
  match J.from_string args_attenuation with
  | `Null -> true
  | `Assoc kvs -> (
      match List.assoc_opt "max_ternary" kvs with
      | Some (`Int n) -> String.length args_ternary <= n
      | _ -> true (* unknown predicate shapes are admitted; review matter *))
  | _ -> true
  | exception _ -> true

(* -- kv wiring for store/get + store/put ----------------------------- *)

let kv_of_pool pool : Prims.kv =
  { Prims.kv_get =
      (fun key ->
        S.prim_get pool (Tuna.Canon.encode key)
        >>= function
        | Some (_, value) -> (
            match Tuna.Canon.of_string value with
            | Ok v -> Lwt.return (Some v)
            | Error _ -> Lwt.return None (* corrupted kv row reads as absent *))
        | None -> Lwt.return None)
  ; Prims.kv_put =
      (fun ~key ~value ->
        S.prim_put pool
          ~key_ternary:(Tuna.Canon.encode key)
          ~value_ternary:(Tuna.Canon.encode value)) }

(* Run wall-clock cap (operator policy, not a calculus budget): every
   run gets TUNA_RUN_MAX_SECONDS seconds of wall clock (default 10;
   0/negative/unparsable disables).  Fuel is client-set and by itself
   unbounded - a 400M-fuel run must not pin the single-threaded
   runtime.  A deadline abort finalizes the run row as status
   deadline_exceeded, journaled like any other exhaustion. *)
let run_max_seconds () =
  match Sys.getenv_opt "TUNA_RUN_MAX_SECONDS" with
  | None -> Some 10.0
  | Some s -> (
      match float_of_string_opt s with
      | Some v when v > 0.0 -> Some v
      | _ -> None)

let deadline_now () =
  match run_max_seconds () with
  | Some secs -> Unix.gettimeofday () +. secs
  | None -> Float.infinity

(* Compile-time reduction is request-boundary work too (repl eval/def
   and source program upload): same shape as the run cap, its own knob
   so a big run budget does not silently license a big compile.
   TUNA_COMPILE_MAX_SECONDS (default 10; 0/negative/unparsable
   disables); fuel stays the coarse ceiling on top. *)
let compile_max_seconds () =
  match Sys.getenv_opt "TUNA_COMPILE_MAX_SECONDS" with
  | None -> Some 10.0
  | Some s -> (
      match float_of_string_opt s with
      | Some v when v > 0.0 -> Some v
      | _ -> None)

let compile_deadline_now () =
  match compile_max_seconds () with
  | Some secs -> Unix.gettimeofday () +. secs
  | None -> Float.infinity

(* -- the host -------------------------------------------------------- *)

(* Execute a run: insert the row, evaluate with the prim host, journal
   every boundary event, update the row, return (row, journals).
   Input trees are content-addressed into the programs table so replay
   can recover them from their hashes (run rows store input HASHES). *)
let execute pool ~caller ~grant_ids ~program_hash ~program ~ir_json ~inputs
    ?(parent_run_id = None) ~fuel ~size_cap () =
  let input_hashes = List.map Tuna.Hash.hex_of_tree inputs in
  let rec store_inputs = function
    | [] -> Lwt.return ()
    | t :: rest ->
        let ternary = Tuna.Canon.encode t in
        let hash = Tuna.Hash.hex_of_string ternary in
        S.upsert_program pool ~hash ~ternary ~ir:None ~created_by:(Some caller)
        >>= fun _ -> store_inputs rest
  in
  store_inputs inputs
  >>= fun () ->
  S.insert_run pool ~program_hash ~inputs:input_hashes ~caller:(Some caller)
    ~parent_run_id ~fuel ~size_cap ()
  >>= fun run_id ->
  grant_map pool grant_ids
  >>= fun gmap ->
  let sites = site_paths ir_json in
  let callsite_path site =
    match List.assoc_opt site sites with Some p -> p | None -> ""
  in
  let kv = kv_of_pool pool in
  let allowlist = Prims.allowlist_from_env () in
  let deadline = deadline_now () in
  let host ~site ~name ~args =
    let t0 = Unix.gettimeofday () in
    let args_ternary = Tuna.Canon.encode args in
    (* M10 substrate prims: op kind + the paths the call would touch
       (for the live grant prefix check).  M11 byte-value prims (law 2,
       byte-values.borg): reads are hash-gated, puts capability-gated
       live in the handler - neither spends the run's grant map.  Every
       substrate-prim ERROR answer is journaled as a no-effect tree_ops
       row below; effects are journaled by the handlers themselves. *)
    let tree_op = Tree_prims.op_of_name name in
    let value_op = Value_prims.op_of_name name in
    let op_path =
      match tree_op with
      | Some _ -> Tree_prims.op_path_of_args args
      | None -> (
          match value_op with
          | Some _ -> Value_prims.op_path_of_args name args
          | None -> "")
    in
    let grant_paths =
      match tree_op with
      | Some _ -> Option.value (Tree_prims.grant_paths name args) ~default:[]
      | None -> []
    in
    let journal_substrate_denial () =
      match (tree_op, value_op) with
      | Some op, _ | None, Some op ->
          S.op_append pool ~op ~path:op_path ~value_hash:None ~prev_version:None
            ~version:None ~actor:caller
          >>= fun _ -> Lwt.return ()
      | None, None -> Lwt.return ()
    in
    (if String.length args_ternary > Prims.payload_cap then
       (* never inline oversized payloads: journal hash-free, answer error *)
       Lwt.return
         ( `Error
             (Printf.sprintf "prim %s: args exceed the journal payload cap" name)
         , None
          , None )
        else
         (if name = "tree/del" then S.is_admin pool caller else Lwt.return false)
         >>= function
         | true ->
             (* M11 tree/del (operator-approved pin): the covering-prefix
                rule is tree/put's (a live grant covering the path), with
                ADMINS EXEMPT - a live admin identity dispatches without
                spending the run's grant map; the journal row carries
                grant_id NULL.  Everyone else falls through to the
                standard grant check below. *)
             Tree_prims.dispatch ~pool ~actor:caller ~name ~args
             >>= fun a -> Lwt.return (a, None, Some args_ternary)
         | false ->
           match grant_for gmap name with
         | None ->
             Lwt.return
               ( `Error
                   (Printf.sprintf "grant denial: no live grant for prim %s" name)
               , None
                , Some args_ternary )
           | Some (gid, attenuation) -> (
             S.check_grant pool ~id:gid ~caller ~paths:grant_paths ()
             >>= function
             | `Ok ->
                 if attenuation_ok attenuation args_ternary then
                   (if value_op <> None then
                      Value_prims.dispatch ~pool ~actor:caller ~name ~args
                    else if tree_op <> None then
                      Tree_prims.dispatch ~pool ~actor:caller ~name ~args
                    else Prims.dispatch ~name ~args ~kv ~allowlist)
                   >>= fun a -> Lwt.return (a, Some gid, Some args_ternary)
               else
                 Lwt.return
                   ( `Error
                       (Printf.sprintf
                          "grant denial: prim %s args exceed attenuation" name)
                   , Some gid
                   , Some args_ternary )
               | (`Revoked | `Wrong_caller | `Unknown | `Prefix_denied) as denial ->
                Lwt.return
                  ( `Error
                      (Printf.sprintf "grant denial (%s) for prim %s"
                         (match denial with
                          | `Revoked -> "revoked"
                          | `Wrong_caller -> "wrong caller"
                          | `Unknown -> "unknown"
                          | `Prefix_denied -> "path outside grant prefix")
                         name)
                   , Some gid
                   , Some args_ternary )))
    >>= fun (answer, grant_id, args_inline) ->
    let wall_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.) in
    let ev : S.journal_event =
      { S.e_callsite_path = callsite_path site
      ; e_prim = name
      ; e_prim_contract = Prims.contract
      ; e_grant_id = grant_id
      ; e_args_ternary = args_inline
      ; e_result_ternary =
          (match answer with `Ok t -> Some (Tuna.Canon.encode t) | `Error _ -> None)
      ; e_error = (match answer with `Ok _ -> None | `Error e -> Some e)
      ; e_wall_ms = Some wall_ms }
    in
    S.append_journal pool ~run_id ev
    >>= fun _ ->
    (match (tree_op, value_op, answer) with
     | (Some _, _, `Error _) | (_, Some _, `Error _) -> journal_substrate_denial ()
     | _ -> Lwt.return ())
      >>= fun () -> Lwt.return answer
  in
  Eng.eval ~host ~fuel ~size_cap ~deadline ~program inputs
  >>= fun result ->
  let status, result_ternary, steps =
    match result with
    | Eng.Normal (t, s) ->
        ( S.Run_status.Normal
        , Some (Tuna.Canon.encode t)
        , s )
      | Eng.Fuel_exhausted s -> (S.Run_status.Fuel_exhausted, None, s)
      | Eng.Size_exhausted s -> (S.Run_status.Size_exhausted, None, s)
      | Eng.Deadline_exceeded s -> (S.Run_status.Deadline_exceeded, None, s)
  in
  S.update_run_result pool ~id:run_id ~status ?result_ternary:result_ternary
    ?step_count:(Some steps) ()
  >>= fun () ->
  S.fetch_run pool run_id
  >>= function
  | None -> Lwt.fail (Failure "run row vanished")
  | Some row -> S.fetch_journals pool run_id >>= fun js -> Lwt.return (row, js)

(* Shared synchronous execution: up-front grant validation, program
   fetch, run row + boundary execution.  Lives in Run (not Api) so the
   page layer can use it without a module cycle (Program -> Run, while
   Api -> Pages -> Program).  Used by post_run, the M8 run form, and
   the M9 REPL's journaled rounds (parent_run_id chains the per-session
   transcript). *)
let execute_run pool ~caller ~program_hash ~input_trees ~grant_ids ~fuel
    ?(parent_run_id = None) ~size_cap () :
    (S.run * S.journal list, int * string) result Lwt.t =
  if fuel < 1 || size_cap < 1 then
    Lwt.return (Error (400, "fuel and size_cap must be >= 1"))
  else
      let rec check gs =
        match gs with
        | [] -> Lwt.return None
        | gid :: rest -> (
            S.check_grant pool ~id:gid ~caller ()
            >>= function
            | `Ok -> check rest
            | `Revoked -> Lwt.return (Some (Printf.sprintf "grant %s is revoked" gid))
            | `Wrong_caller ->
                Lwt.return
                  (Some (Printf.sprintf "grant %s does not belong to caller" gid))
            | `Unknown -> Lwt.return (Some (Printf.sprintf "grant %s not found" gid))
            | `Prefix_denied ->
                Lwt.return
                  (Some
                     (Printf.sprintf
                        "grant %s does not cover this call's paths" gid)))
    in
    check grant_ids
    >>= (function
          | Some msg -> Lwt.return (Error (403, msg))
          | None -> (
              S.fetch_program pool program_hash
              >>= (function
                    | None -> Lwt.return (Error (404, "unknown program hash"))
                    | Some prog -> (
                        match Tuna.Canon.of_string prog.S.p_ternary with
                        | Error (off, msg) ->
                            Lwt.return
                              (Error
                                 ( 500,
                                   Printf.sprintf
                                     "stored program unparseable (offset %d: %s)"
                                     off msg ))
                        | Ok program ->
                            execute pool ~caller ~grant_ids ~program_hash
                              ~program ~ir_json:prog.S.p_ir ~inputs:input_trees
                              ~parent_run_id ~fuel ~size_cap ()
                            >>= fun (row, js) -> Lwt.return (Ok (row, js))))))

