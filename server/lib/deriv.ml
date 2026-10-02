(* Tuna_server.Deriv: build a portable derivation record for a run
   (borg/deriv.borg).

   The record is the run's checkable receipt: run facts, the program
   and input trees, the full hash-chained journal, and the claimed
   outcome, sealed by Tuna_deriv (canon + sha256 id). The builder
   assembles honestly from store rows; the checker - online in
   tests, offline in tools/deriv-check - is what turns a record into
   a verdict.

   Served verbatim by GET /api/runs/:id/deriv so a third party can
   pipe it straight into deriv-check. Refusals, by policy:

   - running: no outcome exists yet;
   - deadline_exceeded: a wall-clock abort is operator timing, not a
     calculus fact (the same ruling Replay.verify applies);
   - unknown run / missing program or input rows: the record would
     name bytes the store cannot show. *)
open Tuna_store.Direct

module Store = Tuna_store.Store
module D = Tuna_deriv.Deriv

let err code msg = (code, "{\"error\":" ^ Yojson.Basic.to_string (`String msg) ^ "}")

let status_of_run (r : Store.run) : (D.status, string) result =
  match r.Store.r_status with
  | Store.Run_status.Normal -> Ok D.Normal
  | Store.Run_status.Loop -> Ok D.Loop
  | Store.Run_status.Fuel_exhausted -> Ok D.Fuel_exhausted
  | Store.Run_status.Size_exhausted -> Ok D.Size_exhausted
  | Store.Run_status.Deadline_exceeded ->
      Error
        "deadline_exceeded run: a wall-clock abort is operator timing, not a \
         calculus fact; no derivation record exists for it"
  | Store.Run_status.Error ->
      Error
        "error run: the recorded outcome was a replay divergence, not a \
         calculus result; nothing to re-derive"
  | Store.Run_status.Running -> Error "run is still running"

let journal_of_store (j : Store.journal) : D.Journal.t =
  { D.Journal.j_seq = j.Store.j_seq
  ; j_callsite_path = j.Store.j_callsite_path
  ; j_prim = j.Store.j_prim
  ; j_prim_contract = j.Store.j_prim_contract
  ; j_grant_id = j.Store.j_grant_id
  ; j_args_ternary = j.Store.j_args_ternary
  ; j_args_hash = j.Store.j_args_hash
  ; j_result_ternary = j.Store.j_result_ternary
  ; j_result_hash = j.Store.j_result_hash
  ; j_error = j.Store.j_error
  ; j_wall_ms = j.Store.j_wall_ms
  ; j_host_build = j.Store.j_host_build
  ; j_prev_hash = j.Store.j_prev_hash
  ; j_row_hash = j.Store.j_row_hash }

(* (code, body) like the fed core: 404 unknown, 409 no record. *)
let of_run pool ~(run_id : string) : (int * string) =
  Store.fetch_run pool run_id
  >>= (function
        | None -> return (err 404 "unknown run id")
        | Some run -> (
            match status_of_run run with
            | Error msg -> return (err 409 msg)
            | Ok status -> (
                Store.fetch_program pool run.Store.r_program_hash
                >>= (function
                      | None ->
                          return
                            (err 404
                               (Printf.sprintf "program %s missing from the store"
                                  run.Store.r_program_hash))
                      | Some prog -> (
                          let rec inputs acc = function
                            | [] -> return (Ok (List.rev acc))
                            | h :: rest -> (
                                Store.fetch_program pool h
                                >>= (function
                                      | None ->
                                          return
                                            (Error
                                               ( 404,
                                                 Printf.sprintf
                                                   "input %s missing from the \
                                                    store" h ))
                                      | Some ip -> inputs (ip.Store.p_ternary :: acc) rest))
                          in
                          inputs [] run.Store.r_input_hashes
                          >>= (function
                                | Error (code, msg) -> return (err code msg)
                                | Ok input_ternaries ->
                                    Store.fetch_journals pool run.Store.r_id
                                    >>= fun js ->
                                    let d =
                                      D.build
                                        ~run_id:run.Store.r_id
                                        ~semantics:run.Store.r_semantics
                                        ~program_hash:run.Store.r_program_hash
                                        ~program_ternary:prog.Store.p_ternary
                                        ~input_hashes:run.Store.r_input_hashes
                                        ~input_ternaries
                                        ~fuel:run.Store.r_fuel
                                        ~size_cap:run.Store.r_size_cap
                                        ~status
                                        ~result_ternary:run.Store.r_result_ternary
                                        ~result_hash:run.Store.r_result_hash
                                        ~step_count:
                                          (Option.value run.Store.r_step_count
                                             ~default:0)
                                        ~parent_run_id:run.Store.r_parent_run_id
                                        ~journal:(List.map journal_of_store js)
                                    in
                                    return (200, D.to_string d)))))))
