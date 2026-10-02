(* Tuna_server.Replay: faithful replay + verification (M7; borg/replay).

   Faithful replay re-executes program + inputs under the canonical
   strategy (the SAME Prim_eval engine, so evaluation order and step
   accounting cannot drift) with every prim call answered SEQUENTIALLY
   from the run's journal rows (journal.recorded-environment): the
   host, network, clock, and Postgres state need not exist.  A run
   verifies iff:

     1. the journal hash chain is intact (Store.verify_chain)
     2. every prim call of the replay finds its next recorded answer —
        right prim name, recorded args equal the replayed args
        (per-seq match), row well-formed (result or error present)
     3. no journal row is left unconsumed
     4. the replayed outcome (status, result hash, step count) equals
        the recorded run row (SPEC.md §6.2 replay identity)

   Divergence is reported as ADDRESSED STRUCTURE, not prose
   (replay.divergence-surface): first mismatch seq, callsite path, prim
   name, recorded vs replayed hashes, and a first_diff_path — a
   structural subtree-hash walk between the differing trees, "" when
   the divergence is at the root or not tree-shaped (step-count only).

   Verification never re-checks grants and never touches the live
   world: the journal answers, not the grant table (grants.invocation). *)

open Tuna_store.Direct

module Eng = Tuna_interp.Flat_drive.Make (Tuna_store.Direct)
module S = Tuna_store.Store

type outcome = Eng.result =
  | Normal of Tuna.Tree.t * int
  | Loop of int
  | Fuel_exhausted of int
  | Size_exhausted of int
  | Deadline_exceeded of int

(* replay.divergence-surface: one addressed diff, not a wall of text *)
type divergence = {
  div_seq : int option; (* journal seq of the first mismatch; None = run-row level *)
  callsite_path : string;
  prim : string;
  reason : string;
  first_diff_path : string; (* "" = root / not applicable *)
  recorded_hash : string option;
  replayed_hash : string option;
  (* replay.prim-versioning: the pinned contract of the diverged row and
     the CURRENT contract, so an upgrade (contract mismatch) is
     distinguishable from a regression.  Faithful replay itself never
     depends on these (recorded answers are data); they are reported. *)
  recorded_contract : string option;
  current_contract : string option;
  contract_mismatch : bool;
}

exception Diverged of divergence

type verdict =
  | Verified of outcome
  | Bad_chain of string
  | Diverged of divergence
  | Unverifiable of string
  | Gone of string (* journal.retention-gc: GC tombstone cited *)

let outcome_status = function
  | Normal _ -> "normal"
  | Loop _ -> "loop"
  | Fuel_exhausted _ -> "fuel_exhausted"
  | Size_exhausted _ -> "size_exhausted"
  | Deadline_exceeded _ -> "deadline_exceeded"

let outcome_ternary = function
  | Normal (t, _) -> Some (Tuna.Canon.encode t)
  | Loop _ | Fuel_exhausted _ | Size_exhausted _ | Deadline_exceeded _ -> None

let outcome_steps = function
  | Normal (_, s)
  | Loop s
  | Fuel_exhausted s
  | Size_exhausted s
  | Deadline_exceeded s -> s

let outcome_hash o = Option.map Tuna.Hash.hex_of_string (outcome_ternary o)

(* wall-clock aborts are operator policy, never a calculus fact *)
let is_deadline = function Deadline_exceeded _ -> true | _ -> false

(* Structural walk: descend where child subtree hashes disagree, stop
   at the first.  Path convention: 0 = stem child, 1 = fork left,
   2 = fork right; "" = the roots themselves differ (patch first-diff
   convention). *)
let first_diff_path (a : Tuna.Tree.t) (b : Tuna.Tree.t) : string =
  let rec go acc a b =
    match (a, b) with
    | Tuna.Tree.Leaf, Tuna.Tree.Leaf ->
        String.concat "" (List.rev_map string_of_int acc)
    | Tuna.Tree.Stem x, Tuna.Tree.Stem y -> go (0 :: acc) x y
    | Tuna.Tree.Fork (a1, a2), Tuna.Tree.Fork (b1, b2) ->
        if Tuna.Hash.hex_of_tree a1 <> Tuna.Hash.hex_of_tree b1 then
          go (1 :: acc) a1 b1
        else if Tuna.Hash.hex_of_tree a2 <> Tuna.Hash.hex_of_tree b2 then
          go (2 :: acc) a2 b2
        else String.concat "" (List.rev_map string_of_int acc)
    | _ -> String.concat "" (List.rev_map string_of_int acc)
  in
  go [] a b

(* -- journal-fed execution ------------------------------------------- *)

(* Execute [program] against [inputs] with prim calls answered
   sequentially from [rows].  Answers come from the journal (accepted
   results consumed in order); no grant check, no live host.  Returns
   (outcome, rows consumed). *)
let execute_fed ?(deadline = Float.infinity) ?(mode = Eng.Canonical) ~program
    ~inputs ~fuel ~size_cap (rows : S.journal list) : (outcome * int) =
  let arr = Array.of_list rows in
  let next = ref 0 in
  let host ~site:_ ~name ~args =
    if !next >= Array.length arr then
      raise
        (Diverged
           { div_seq = Some !next
           ; callsite_path = ""
           ; prim = name
           ; reason =
               Printf.sprintf
                 "replay ran past the journal: no recorded answer for prim %s \
                  at seq %d"
                 name !next
           ; first_diff_path = ""
           ; recorded_hash = None
           ; replayed_hash = None
           ; recorded_contract = None
           ; current_contract = Some Prims.contract
           ; contract_mismatch = false })
    else
      let row = arr.(!next) in
      let diverge ?(first_diff_path = "") ~recorded_hash ~replayed_hash reason =
        raise
          (Diverged
             { div_seq = Some row.S.j_seq
             ; callsite_path = row.S.j_callsite_path
             ; prim = name
             ; reason
             ; first_diff_path
             ; recorded_hash
             ; replayed_hash
             ; recorded_contract = Some row.S.j_prim_contract
             ; current_contract = Some Prims.contract
             ; contract_mismatch = row.S.j_prim_contract <> Prims.contract })
      in
      if row.S.j_prim <> name then
        diverge ~recorded_hash:(Some row.S.j_prim) ~replayed_hash:(Some name)
          (Printf.sprintf "journal seq %d records prim %S but the run called %S"
             row.S.j_seq row.S.j_prim name)
      else (
        (match row.S.j_args_ternary with
        | Some recorded -> (
            let actual = Tuna.Canon.encode args in
            match Tuna.Canon.of_string recorded with
            | Ok rt when Tuna.Hash.hex_of_string recorded <> Tuna.Hash.hex_of_string actual ->
                diverge
                  ~first_diff_path:(first_diff_path rt args)
                  ~recorded_hash:row.S.j_args_hash
                  ~replayed_hash:(Some (Tuna.Hash.hex_of_tree args))
                  (Printf.sprintf
                     "recorded args at seq %d differ from the replayed call"
                     row.S.j_seq)
            | _ -> ())
        | None -> () (* oversized-args row: args were never inlined *));
        let answer =
          match (row.S.j_result_ternary, row.S.j_error) with
          | Some t, _ -> (
              match Tuna.Canon.of_string t with
              | Ok tr -> `Ok tr
              | Error (off, msg) ->
                  diverge ~recorded_hash:row.S.j_result_hash ~replayed_hash:None
                    (Printf.sprintf
                       "recorded result_ternary at seq %d is unparseable at \
                        offset %d: %s"
                       row.S.j_seq off msg))
          | None, Some e -> `Error e
          | None, None ->
              diverge ~recorded_hash:None ~replayed_hash:None
                (Printf.sprintf
                   "journal seq %d has neither a result nor an error (cleared \
                    or corrupted row)"
                   row.S.j_seq)
        in
        incr next;
        return answer)
  in
  Eng.eval ~host ~mode ~fuel ~size_cap ~deadline ~program inputs
  >>= fun outcome -> return (outcome, !next)

(* -- prim contract versioning (replay.prim-versioning) --------------- *)

(* A row pins the contract it was recorded under.  Current = the
   build's Prims.contract.  A mismatch means the prim layer changed
   semantics since the row was journaled: reported FIRST-CLASS so an
   upgrade is distinguishable from a regression. *)
type contract_mismatch = {
  cm_seq : int;
  cm_prim : string;
  cm_recorded_contract : string;
  cm_current_contract : string;
  cm_recorded_build : string;
}

let contract_mismatches (rows : S.journal list) : contract_mismatch list =
  List.filter_map
    (fun (j : S.journal) ->
      if j.S.j_prim_contract = Prims.contract then None
      else
        Some
          { cm_seq = j.S.j_seq
          ; cm_prim = j.S.j_prim
          ; cm_recorded_contract = j.S.j_prim_contract
          ; cm_current_contract = Prims.contract
          ; cm_recorded_build = j.S.j_host_build })
    rows

let has_contract_mismatch rows = contract_mismatches rows <> []

(* -- journal-versus-journal world diff (replay.live) ------------------ *)

(* The operations analog of program first-diff: align the recorded and
   live journals BY SEQ and compare the result of each accepted call
   (result hash, or the error text).  The first seq whose answer
   changed is the world diff; extra/missing rows also diff. *)
type world_diff = {
  wd_seq : int;
  wd_kind : string; (* result_hash | error | extra | missing | prim | args *)
  wd_prim : string;
  wd_recorded_hash : string option;
  wd_live_hash : string option;
  wd_recorded_error : string option;
  wd_live_error : string option;
  wd_first_diff_path : string;
}

let answer_of (j : S.journal) : [ `Hash of string | `Error of string | `None ] =
  match (j.S.j_result_hash, j.S.j_error) with
  | Some h, _ -> `Hash h
  | None, Some e -> `Error e
  | None, None -> `None

let diff_journals (recorded : S.journal list) (live : S.journal list) :
    world_diff option =
  let ra = Array.of_list recorded and la = Array.of_list live in
  let n = max (Array.length ra) (Array.length la) in
  let rec go i =
    if i >= n then None
    else if i >= Array.length la then
      let r = ra.(i) in
      Some
        { wd_seq = r.S.j_seq
        ; wd_kind = "missing"
        ; wd_prim = r.S.j_prim
        ; wd_recorded_hash = r.S.j_result_hash
        ; wd_live_hash = None
        ; wd_recorded_error = r.S.j_error
        ; wd_live_error = None
        ; wd_first_diff_path = "" }
    else if i >= Array.length ra then
      let l = la.(i) in
      Some
        { wd_seq = l.S.j_seq
        ; wd_kind = "extra"
        ; wd_prim = l.S.j_prim
        ; wd_recorded_hash = None
        ; wd_live_hash = l.S.j_result_hash
        ; wd_recorded_error = None
        ; wd_live_error = l.S.j_error
        ; wd_first_diff_path = "" }
    else
      let r = ra.(i) and l = la.(i) in
      if r.S.j_prim <> l.S.j_prim then
        Some
          { wd_seq = r.S.j_seq
          ; wd_kind = "prim"
          ; wd_prim = r.S.j_prim
          ; wd_recorded_hash = r.S.j_result_hash
          ; wd_live_hash = l.S.j_result_hash
          ; wd_recorded_error = r.S.j_error
          ; wd_live_error = l.S.j_error
          ; wd_first_diff_path = "" }
      else
        match (answer_of r, answer_of l) with
        | `Hash rh, `Hash lh when rh <> lh ->
            let first_diff_path =
              match (r.S.j_result_ternary, l.S.j_result_ternary) with
              | Some rt, Some lt -> (
                  match (Tuna.Canon.of_string rt, Tuna.Canon.of_string lt) with
                  | Ok a, Ok b -> first_diff_path a b
                  | _ -> "")
              | _ -> ""
            in
            Some
              { wd_seq = r.S.j_seq
              ; wd_kind = "result_hash"
              ; wd_prim = r.S.j_prim
              ; wd_recorded_hash = Some rh
              ; wd_live_hash = Some lh
              ; wd_recorded_error = None
              ; wd_live_error = None
              ; wd_first_diff_path = first_diff_path }
        | `Error re, `Error le when re <> le ->
            Some
              { wd_seq = r.S.j_seq
              ; wd_kind = "error"
              ; wd_prim = r.S.j_prim
              ; wd_recorded_hash = None
              ; wd_live_hash = None
              ; wd_recorded_error = Some re
              ; wd_live_error = Some le
              ; wd_first_diff_path = "" }
        | `Hash _, `Error _ | `Error _, `Hash _ | `Hash _, `None
        | `None, `Hash _ | `Error _, `None | `None, `Error _ ->
            Some
              { wd_seq = r.S.j_seq
              ; wd_kind = "result_hash"
              ; wd_prim = r.S.j_prim
              ; wd_recorded_hash = r.S.j_result_hash
              ; wd_live_hash = l.S.j_result_hash
              ; wd_recorded_error = r.S.j_error
              ; wd_live_error = l.S.j_error
              ; wd_first_diff_path = "" }
        | `Hash _, `Hash _ | `Error _, `Error _ | `None, `None -> go (i + 1)
  in
  go 0

(* -- verification ----------------------------------------------------- *)

(* Load a run's program tree + input trees + journal rows.  Input hashes
   resolve through the programs table (the boundary stores input trees
   content-addressed at run time). *)
let load_run_parts pool ~(run : S.run) :
    (Tuna.Tree.t * Tuna.Tree.t list * S.journal list, string) result =
  S.fetch_program pool run.S.r_program_hash
  >>= (function
        | None -> return (Error "program row missing from the store")
        | Some prog -> (
            match Tuna.Canon.of_string prog.S.p_ternary with
            | Error (off, msg) ->
                return
                  (Error
                     (Printf.sprintf "stored program unparseable at offset %d: %s"
                        off msg))
            | Ok program -> (
                let rec load_inputs = function
                  | [] -> return (Ok [])
                  | h :: rest -> (
                      S.fetch_program pool h
                      >>= (function
                            | None ->
                                return
                                  (Error
                                     (Printf.sprintf
                                        "input tree %s missing from the store" h))
                            | Some ip -> (
                                match Tuna.Canon.of_string ip.S.p_ternary with
                                | Ok t -> (
                                    load_inputs rest
                                    >>= (function
                                          | Ok is -> return (Ok (t :: is))
                                          | Error e -> return (Error e)))
                                | Error (off, msg) ->
                                    return
                                      (Error
                                         (Printf.sprintf
                                            "stored input %s unparseable at \
                                             offset %d: %s"
                                            h off msg)))))
                in
                load_inputs run.S.r_input_hashes
                >>= (function
                      | Error _ as e -> return e
                      | Ok inputs ->
                          S.fetch_journals pool run.S.r_id
                          >>= fun js -> return (Ok (program, inputs, js))))))

(* Live replay (replay.live): re-execute the parent's program + inputs
   against the CURRENT world under FRESH grants.  It mints its own run
   row (parent_run_id = the parent) and journal via Run.execute; the
   result is never verification state (the new row is unverified).  The
   world diff is the journal-versus-journal answer comparison; contract
   mismatches between the parent's pinned contract and the current build
   are reported alongside. *)
let live_replay pool ~caller ~grant_ids ~(parent : S.run) ~fuel ~size_cap () :
    ((S.run * S.journal list * world_diff option * contract_mismatch list),
     string)
    result
    =
  if parent.S.r_status = S.Run_status.Running then
    return (Error "run is still running")
  else
    load_run_parts pool ~run:parent
    >>= (function
          | Error msg -> return (Error msg)
          | Ok (program, inputs, recorded) ->
              S.fetch_program pool parent.S.r_program_hash
              >>= (function
                    | None -> return (Error "program row missing")
                    | Some prog ->
                        Run.execute pool ~caller ~grant_ids
                          ~program_hash:parent.S.r_program_hash ~program
                          ~ir_json:prog.S.p_ir ~inputs
                          ~parent_run_id:(Some parent.S.r_id)
                          ~semantics:parent.S.r_semantics ~fuel ~size_cap ()
                        >>= fun (row, live) ->
                        let diff = diff_journals recorded live in
                        let mismatches = contract_mismatches recorded in
                        return (Ok (row, live, diff, mismatches))) )

(* Verify one run row against its journal (no state written). *)
let verify pool ?(deadline = Float.infinity) ~(run : S.run) () : verdict =
  if run.S.r_status = S.Run_status.Running then
    return (Unverifiable "run is still running")
  else if run.S.r_status = S.Run_status.Deadline_exceeded then
    (* replay re-runs the calculus, not the clock: a wall-clock abort
       records operator timing, not a computational fact *)
    return
      (Unverifiable
         "deadline_exceeded run: wall-clock abort, not a calculus fact")
  else
    (* retention tombstone first: a GC'd journal is GONE, never VERIFIED
       and never FAILED-on-merits — the answer is unknown because the
       recorded environment was retired under a cited policy. *)
    S.fetch_gc_tombstone pool run.S.r_id
    >>= (function
          | Some policy ->
              return
                (Gone
                   (Printf.sprintf
                      "journal gced; replay answers are unknown (retention \
                       policy: %s)"
                      (Option.value policy ~default:"uncited")))
          | None ->
    S.fetch_journals pool run.S.r_id
    >>= fun js ->
    match S.verify_chain js with
    | `Bad reason -> return (Bad_chain reason)
    | `Ok -> (
        load_run_parts pool ~run
        >>= (function
              | Error msg -> return (Unverifiable msg)
                | Ok (program, inputs, js') -> (
                      (* per-version replay (borg/sharing.borg): the row
                         names the accounting law that produced its
                         numbers; this build re-executes under it, or
                         refuses a future version as unverifiable. *)
                      let mode =
                        match run.S.r_semantics with
                        | "v0" -> Ok Eng.Canonical
                        | "v1" -> Ok Eng.Sharing
                        | s -> Error s
                      in
                      let nrows = List.length js' in
                      (match mode with
                       | Error s ->
                           return
                             (Unverifiable
                                (Printf.sprintf
                                   "unknown semantics version %S for this build"
                                   s))
                       | Ok mode ->
                      catch
                        (fun () ->
                            execute_fed ~mode ~program ~inputs ~fuel:run.S.r_fuel
                              ~size_cap:run.S.r_size_cap ~deadline js'
                            >>= fun (outcome, consumed) ->
                          if is_deadline outcome then
                            (* a replay-side clock abort is an operator
                               budget, not evidence of divergence *)
                            return
                              (Unverifiable
                                 "replay exceeded the wall-clock budget \
                                  (TUNA_RUN_MAX_SECONDS): raise it or disable \
                                  it (0) to verify this run")
                          else if consumed < nrows then
                          let rows = Array.of_list js' in
                          let row = rows.(consumed) in
                          return
                            (Diverged
                               { div_seq = Some row.S.j_seq
                               ; callsite_path = row.S.j_callsite_path
                               ; prim = row.S.j_prim
                               ; reason =
                                   "journal holds unconsumed rows after the \
                                    replay"
                               ; first_diff_path = ""
                               ; recorded_hash = row.S.j_result_hash
                               ; replayed_hash = outcome_hash outcome
                               ; recorded_contract = Some row.S.j_prim_contract
                               ; current_contract = Some Prims.contract
                               ; contract_mismatch =
                                   row.S.j_prim_contract <> Prims.contract })
                        else
                          (* replay identity: status + result + steps *)
                          let recorded_hash = run.S.r_result_hash in
                          let replayed = outcome_hash outcome in
                          if recorded_hash <> replayed then
                            let first_diff_path =
                              match (run.S.r_result_ternary, outcome_ternary outcome)
                              with
                              | Some rt, Some ot -> (
                                  match
                                    (Tuna.Canon.of_string rt, Tuna.Canon.of_string ot)
                                  with
                                  | Ok a, Ok b -> first_diff_path a b
                                  | _ -> "")
                              | _ -> ""
                            in
                            return
                              (Diverged
                                 { div_seq = None
                                 ; callsite_path = ""
                                 ; prim = ""
                                 ; reason =
                                     "replayed result differs from the \
                                      recorded run row"
                                 ; first_diff_path
                                 ; recorded_hash
                                 ; replayed_hash = replayed
                                 ; recorded_contract = None
                                 ; current_contract = Some Prims.contract
                                 ; contract_mismatch = false })
                          else if
                            outcome_status outcome
                            <> S.Run_status.to_string run.S.r_status
                          then
                            return
                              (Diverged
                                 { div_seq = None
                                 ; callsite_path = ""
                                 ; prim = ""
                                 ; reason =
                                     "replayed status differs from the \
                                      recorded run row"
                                 ; first_diff_path = ""
                                 ; recorded_hash
                                 ; replayed_hash = replayed
                                 ; recorded_contract = None
                                 ; current_contract = Some Prims.contract
                                 ; contract_mismatch = false })
                          else if
                            (not run.S.r_demand_sharing)
                            && Some (outcome_steps outcome) <> run.S.r_step_count
                          then
                            return
                              (Diverged
                                 { div_seq = None
                                 ; callsite_path = ""
                                 ; prim = ""
                                 ; reason =
                                     "replayed step count differs from the \
                                      recorded run row"
                                 ; first_diff_path = ""
                                 ; recorded_hash
                                 ; replayed_hash = replayed
                                 ; recorded_contract = None
                                 ; current_contract = Some Prims.contract
                                 ; contract_mismatch = false })
                          else return (Verified outcome))
                      (function
                        | Diverged d -> return (Diverged d)
                          | e -> fail e))))))

(* Re-execute a (derived) run against its own journal and WRITE the
   outcome into the run row: the counterfactual execution behind fork.
   A divergent edit leaves the run in status Error with the divergence
   recorded in the returned verdict (never raised). *)
let reexecute pool ?(deadline = Float.infinity) ~run_id () : verdict =
  S.fetch_run pool run_id
  >>= (function
        | None -> fail (Failure "unknown run")
        | Some run when run.S.r_status = S.Run_status.Deadline_exceeded ->
            return
              (Unverifiable
                 "deadline_exceeded run: wall-clock abort, not a calculus \
                  fact; the fork counterfactual re-runs the clock too")
        | Some run ->
            S.fetch_journals pool run_id
            >>= fun js ->
            (match S.verify_chain js with
             | `Bad reason -> return (Bad_chain reason)
             | `Ok -> (
                 load_run_parts pool ~run
                 >>= (function
                       | Error msg -> return (Unverifiable msg)
                         | Ok (program, inputs, js') ->
                             (* the counterfactual re-executes under the
                                row's own law, same as verify *)
                             let mode =
                               match run.S.r_semantics with
                               | "v0" -> Ok Eng.Canonical
                               | "v1" -> Ok Eng.Sharing
                               | s -> Error s
                             in
                             (match mode with
                              | Error s ->
                                  return
                                    (Unverifiable
                                       (Printf.sprintf
                                          "unknown semantics version %S for this build"
                                          s))
                              | Ok mode ->
                             let nrows = List.length js' in
                             catch
                               (fun () ->
                                  execute_fed ~mode ~program ~inputs ~fuel:run.S.r_fuel
                                    ~size_cap:run.S.r_size_cap ~deadline js'
                                  >>= fun (outcome, consumed) ->
                                  if consumed < nrows then
                                 let rows = Array.of_list js' in
                                 let row = rows.(consumed) in
                                 let d =
                                   { div_seq = Some row.S.j_seq
                                   ; callsite_path = row.S.j_callsite_path
                                   ; prim = row.S.j_prim
                                   ; reason =
                                       "journal holds unconsumed rows after \
                                        the replay"
                                   ; first_diff_path = ""
                                   ; recorded_hash = row.S.j_result_hash
                                   ; replayed_hash = outcome_hash outcome
                                   ; recorded_contract = Some row.S.j_prim_contract
                                   ; current_contract = Some Prims.contract
                                   ; contract_mismatch =
                                       row.S.j_prim_contract <> Prims.contract }
                                 in
                                 S.update_run_result pool ~id:run_id
                                   ~status:S.Run_status.Error ()
                                 >>= fun () -> return (Diverged d)
                               else
                                 let status =
                                   match outcome with
                                   | Normal _ -> S.Run_status.Normal
                                   | Loop _ -> S.Run_status.Loop
                                   | Fuel_exhausted _ ->
                                       S.Run_status.Fuel_exhausted
                                  | Deadline_exceeded _ ->
                                      S.Run_status.Deadline_exceeded
                                  | Size_exhausted _ ->
                                      S.Run_status.Size_exhausted
                                 in
                                   S.update_run_result pool ~id:run_id ~status
                                     ?result_ternary:(outcome_ternary outcome)
                                     ?step_count:(Some (outcome_steps outcome))
                                     ()
                                   >>= fun () ->
                                   if is_deadline outcome then
                                     (* the counterfactual re-ran the clock
                                        too; finalize the fork row the same
                                        way the run boundary would *)
                                     return
                                       (Unverifiable
                                          "counterfactual exceeded the \
                                           wall-clock budget \
                                           (TUNA_RUN_MAX_SECONDS); the fork \
                                           row is finalized as \
                                           deadline_exceeded")
                                   else return (Verified outcome))
                             (function
                               | Diverged d ->
                                   S.update_run_result pool ~id:run_id
                                     ~status:S.Run_status.Error ()
                                   >>= fun () -> return (Diverged d)
                                 | e -> fail e))))))

(* Verify and WRITE verify_status ("verified" / "failed" / "gced"). *)
let verify_and_record pool ?(deadline = Float.infinity) ~run_id () =
  S.fetch_run pool run_id
  >>= (function
        | None -> fail (Failure "unknown run")
        | Some run ->
              verify pool ~run ~deadline ()
            >>= fun v ->
            let status =
              match v with
              | Verified _ -> "verified"
              | Gone _ -> "gced"
              | Bad_chain _ | Diverged _ | Unverifiable _ -> "failed"
            in
            S.update_verify_status pool ~id:run_id ~verify_status:status ()
            >>= fun () -> return v)
