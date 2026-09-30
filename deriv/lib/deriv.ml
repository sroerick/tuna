(* Tuna_deriv: portable derivation records + an offline checker
   (borg/deriv.borg).

   A derivation record is a run's checkable receipt: the run facts,
   the program and input trees it executed, its full hash-chained
   effect journal, and the outcome it claims - everything a third
   party needs to re-derive the claimed value WITHOUT the producing
   store - plus one sha256 seal over the canonical encoding of it all.

   The seal is a record IDENTITY, not a proof. A record verifies iff
   the checker's five tests hold:

     1. canon(record fields) = record.deriv_canon   (byte consistency)
     2. sha256(deriv_canon)  = record.deriv_id      (the record id)
     3. every embedded journal row re-hashes to its stored row_hash
        and links prev_hash (genesis = 64 '0' for seq 0, previous
        row_hash otherwise) - the SAME field encoding Store.row_hash
        uses, so server-side chain walks and this check agree
     4. program_ternary and every input_ternary hash to the ids the
        record names; a claimed result_ternary hashes to result_hash
     5. the pure engine replays program+inputs with prim calls
        answered sequentially from the embedded journal and
        reproduces status, result hash, and step count
        (replay identity, SPEC.md s6.2)

   1-4 are byte checks; 5 is the calculus. The checker needs only
   this library: no server, no Postgres, no grants - a hostile or
   dead store changes nothing.

   Seal scope: the canonical string covers run facts, program and
   input trees, the claimed outcome, and each journal row's
   (seq, prev_hash, row_hash). row_hash already covers every row
   field except created_at (host provenance, not a calculus fact),
   so the seal covers the whole chain transitively. wall_ms is
   carried for the audit trail and excluded from row re-hashing,
   exactly like Store.row_hash.

   Honesty boundary (journal.borg recorded-environment, applied to
   records): a fully re-sealed record - tampered rows WITH recomputed
   row_hashes and a fresh outer seal - is a VALID record of its own
   content, because the journal answers the replay verbatim. The
   record attests the derivation, not the world. Binding a deriv_id
   to a real-world event is the producer's job: the store serves the
   canonical deriv_id for a run over an authenticated API, and a
   presented record must match THAT id. *)

module J = Yojson.Basic

let genesis = String.make 64 '0'

module Journal = struct
  (* One boundary event: Store.journal minus run_id (the run facts
     carry it) and created_at (host provenance). *)
  type t = {
    j_seq : int
  ; j_callsite_path : string
  ; j_prim : string
  ; j_prim_contract : string
  ; j_grant_id : string option
  ; j_args_ternary : string option
  ; j_args_hash : string option
  ; j_result_ternary : string option
  ; j_result_hash : string option
  ; j_error : string option
  ; j_wall_ms : int option
  ; j_host_build : string
  ; j_prev_hash : string
  ; j_row_hash : string
  }

  let opt_str = function Some s -> `String s | None -> `Null
  let opt_int = function Some w -> `Int w | None -> `Null

  let to_json (j : t) : J.t =
    `Assoc
      [ ("seq", `Int j.j_seq)
      ; ("callsite_path", `String j.j_callsite_path)
      ; ("prim", `String j.j_prim)
      ; ("prim_contract", `String j.j_prim_contract)
      ; ("grant_id", opt_str j.j_grant_id)
      ; ("args_ternary", opt_str j.j_args_ternary)
      ; ("args_hash", opt_str j.j_args_hash)
      ; ("result_ternary", opt_str j.j_result_ternary)
      ; ("result_hash", opt_str j.j_result_hash)
      ; ("error", opt_str j.j_error)
      ; ("wall_ms", opt_int j.j_wall_ms)
      ; ("host_build", `String j.j_host_build)
      ; ("prev_hash", `String j.j_prev_hash)
      ; ("row_hash", `String j.j_row_hash) ]

  let of_json (j : J.t) : (t, string) result =
    match j with
    | `Assoc kvs ->
        let get k = List.assoc_opt k kvs in
        let str k = match get k with Some (`String s) -> Some s | _ -> None in
        let int k = match get k with Some (`Int i) -> Some i | _ -> None in
        let need k =
          match str k with
          | Some s -> Ok s
          | None -> Error (Printf.sprintf "journal row: missing %S" k)
        in
        (match (need "prim", need "prim_contract", need "host_build",
                need "prev_hash", need "row_hash", int "seq") with
         | Ok prim, Ok contract, Ok host_build, Ok prev, Ok row_hash,
           Some seq ->
             Ok
               { j_seq = seq
               ; j_callsite_path = Option.value (str "callsite_path") ~default:""
               ; j_prim = prim
               ; j_prim_contract = contract
               ; j_grant_id = str "grant_id"
               ; j_args_ternary = str "args_ternary"
               ; j_args_hash = str "args_hash"
               ; j_result_ternary = str "result_ternary"
               ; j_result_hash = str "result_hash"
               ; j_error = str "error"
               ; j_wall_ms = int "wall_ms"
               ; j_host_build = host_build
               ; j_prev_hash = prev
               ; j_row_hash = row_hash }
         | _, _, _, _, _, None -> Error "journal row: missing seq"
         | (Error m, _, _, _, _, _)
         | (_, Error m, _, _, _, _)
         | (_, _, Error m, _, _, _)
         | (_, _, _, Error m, _, _)
         | (_, _, _, _, Error m, _) -> Error m)
    | _ -> Error "journal row: not an object"

  (* Reproduce Store.row_hash byte-for-byte: US-joined fields
     (run_id, seq, callsite, prim, contract, grant, args, result,
     error, wall_ms-or-empty, prev_hash). *)
  let row_fingerprint ~run_id (j : t) : string =
    let field = function Some s -> s | None -> "" in
    Tuna.Hash.hex_of_string
      (String.concat "\x1f"
         [ run_id
         ; string_of_int j.j_seq
         ; j.j_callsite_path
         ; j.j_prim
         ; j.j_prim_contract
         ; field j.j_grant_id
         ; field j.j_args_ternary
         ; field j.j_result_ternary
         ; field j.j_error
         ; (match j.j_wall_ms with Some w -> string_of_int w | None -> "")
         ; j.j_prev_hash ])
end

type status = Normal | Loop | Fuel_exhausted | Size_exhausted | Error

let status_to_string = function
  | Normal -> "normal"
  | Loop -> "loop"
  | Fuel_exhausted -> "fuel_exhausted"
  | Size_exhausted -> "size_exhausted"
  | Error -> "error"

let status_of_string = function
  | "normal" -> Ok Normal
  | "loop" -> Ok Loop
  | "fuel_exhausted" -> Ok Fuel_exhausted
  | "size_exhausted" -> Ok Size_exhausted
  | "error" -> Ok Error
  | s -> Error (Printf.sprintf "unknown status %S" s)

type t = {
  d_run_id : string
  ; d_semantics : string  (* v0 | v1: the accounting law the steps obey *)
  ; d_program_hash : string
  ; d_program_ternary : string
  ; d_input_hashes : string list
  ; d_input_ternaries : string list
  ; d_fuel : int
  ; d_size_cap : int
  ; d_status : status
  ; d_result_ternary : string option
  ; d_result_hash : string option
  ; d_step_count : int
  ; d_parent_run_id : string option
  ; d_journal : Journal.t list
  ; d_deriv_canon : string
  ; d_deriv_id : string
}

let opt_str = function Some s -> `String s | None -> `Null

let to_json (d : t) : J.t =
  `Assoc
    [ ("v", `Int 1)
    ; ("run_id", `String d.d_run_id)
    ; ("semantics", `String d.d_semantics)
    ; ("program_hash", `String d.d_program_hash)
    ; ("program_ternary", `String d.d_program_ternary)
    ; ("input_hashes", `List (List.map (fun s -> `String s) d.d_input_hashes))
    ; ("input_ternaries", `List (List.map (fun s -> `String s) d.d_input_ternaries))
    ; ("fuel", `Int d.d_fuel)
    ; ("size_cap", `Int d.d_size_cap)
    ; ("status", `String (status_to_string d.d_status))
    ; ("result_ternary", opt_str d.d_result_ternary)
    ; ("result_hash", opt_str d.d_result_hash)
    ; ("step_count", `Int d.d_step_count)
    ; ("parent_run_id", opt_str d.d_parent_run_id)
    ; ("journal", `List (List.map Journal.to_json d.d_journal))
    ; ("deriv_canon", `String d.d_deriv_canon)
    ; ("deriv_id", `String d.d_deriv_id) ]

let to_string d = J.to_string (to_json d)

let of_json (j : J.t) : (t, string) result =
  let ( let* ) = Result.bind in
  match j with
  | `Assoc kvs ->
      let get k = List.assoc_opt k kvs in
      let str k = match get k with Some (`String s) -> Some s | _ -> None in
      let int k = match get k with Some (`Int i) -> Some i | _ -> None in
      let lst k = match get k with Some (`List l) -> Some l | _ -> None in
      (* stdlib has no Result.all: two tiny folds, first error wins *)
      let str_list items msg =
        let rec go acc = function
          | [] -> Ok (List.rev acc)
          | `String s :: rest -> go (s :: acc) rest
          | _ :: _ -> Error msg
        in
        go [] items
      in
      let j_list rows =
        let rec go acc = function
          | [] -> Ok (List.rev acc)
          | r :: rest -> (
              match Journal.of_json r with
              | Ok j -> go (j :: acc) rest
              | Error e -> Error e)
        in
        go [] rows
      in
      let need_str k =
        match str k with
        | Some s -> Ok s
        | None -> Error (Printf.sprintf "deriv: missing %S" k)
      in
      let need_int k =
        match int k with
        | Some s -> Ok s
        | None -> Error (Printf.sprintf "deriv: missing %S" k)
      in
      let* _version =
        match int "v" with
        | Some 1 -> Ok 1
        | Some v -> Error (Printf.sprintf "deriv: unsupported version %d" v)
        | None -> Error "deriv: missing version"
      in
      let* run_id = need_str "run_id" in
      let* semantics = need_str "semantics" in
      let* program_hash = need_str "program_hash" in
      let* program_ternary = need_str "program_ternary" in
      let* input_hashes =
        match lst "input_hashes" with
        | Some items -> str_list items "deriv: bad input_hashes"
        | None -> Error "deriv: missing input_hashes"
      in
      let* input_ternaries =
        match lst "input_ternaries" with
        | Some items -> str_list items "deriv: bad input_ternaries"
        | None -> Error "deriv: missing input_ternaries"
      in
      let* fuel = need_int "fuel" in
      let* size_cap = need_int "size_cap" in
      let* status =
        match str "status" with
        | Some s -> status_of_string s
        | None -> Error "deriv: missing status"
      in
      let* step_count = need_int "step_count" in
      let* journal =
        match lst "journal" with
        | Some rows -> j_list rows
        | None -> Error "deriv: missing journal"
      in
      let* deriv_canon = need_str "deriv_canon" in
      let* deriv_id = need_str "deriv_id" in
      Ok
        { d_run_id = run_id
        ; d_semantics = semantics
        ; d_program_hash = program_hash
        ; d_program_ternary = program_ternary
        ; d_input_hashes = input_hashes
        ; d_input_ternaries = input_ternaries
        ; d_fuel = fuel
        ; d_size_cap = size_cap
        ; d_status = status
        ; d_result_ternary = str "result_ternary"
        ; d_result_hash = str "result_hash"
        ; d_step_count = step_count
        ; d_parent_run_id = str "parent_run_id"
        ; d_journal = journal
        ; d_deriv_canon = deriv_canon
        ; d_deriv_id = deriv_id }
  | _ -> Error "deriv: not an object"

let of_string s =
  try of_json (J.from_string s) with e -> Error (Printexc.to_string e)

(* -- canonical encoding ---------------------------------------------- *)

(* Fixed field order, US (\x1f) separated; lists RS (\x1e) separated.
   The journal contributes (seq, prev_hash, row_hash) triplets:
   row_hash already covers every other row field, so the seal covers
   the whole chain transitively. Host provenance (created_at) and
   audit timing (wall_ms) stay out of the canon by design. *)
let canon
    ~(run_id : string)
    ~(semantics : string)
    ~(program_hash : string)
    ~(program_ternary : string)
    ~(input_ternaries : string list)
    ~(fuel : int)
    ~(size_cap : int)
    ~(status : status)
    ~(result_ternary : string option)
    ~(result_hash : string option)
    ~(step_count : int)
    ~(parent_run_id : string option)
    ~(journal : Journal.t list) : string =
  let field = function Some s -> s | None -> "" in
  let inputs = String.concat "\x1e" input_ternaries in
  let rows =
    let one (j : Journal.t) =
      String.concat "\x1f"
        [ string_of_int j.j_seq; j.j_prev_hash; j.j_row_hash ]
    in
    String.concat "\x1e" (List.map one journal)
  in
  String.concat "\x1f"
    [ "tuna-deriv-v1"
    ; run_id
    ; semantics
    ; program_hash
    ; program_ternary
    ; string_of_int (List.length input_ternaries)
    ; inputs
    ; string_of_int fuel
    ; string_of_int size_cap
    ; status_to_string status
    ; field result_ternary
    ; field result_hash
    ; string_of_int step_count
    ; field parent_run_id
    ; string_of_int (List.length journal)
    ; rows ]

let canon_of (d : t) : string =
  canon ~run_id:d.d_run_id ~semantics:d.d_semantics
    ~program_hash:d.d_program_hash ~program_ternary:d.d_program_ternary
    ~input_ternaries:d.d_input_ternaries ~fuel:d.d_fuel ~size_cap:d.d_size_cap
    ~status:d.d_status ~result_ternary:d.d_result_ternary
    ~result_hash:d.d_result_hash ~step_count:d.d_step_count
    ~parent_run_id:d.d_parent_run_id ~journal:d.d_journal

let id_of_canon c = Tuna.Hash.hex_of_string c

(* -- build (producer side) ------------------------------------------- *)

(* Assemble + seal from store rows. The checker re-runs every byte
   check; the builder only assembles honestly. *)
let build
    ~run_id ~semantics ~program_hash ~program_ternary ~input_hashes
    ~input_ternaries ~fuel ~size_cap ~status ~result_ternary ~result_hash
    ~step_count ~parent_run_id ~journal =
  let c =
    canon ~run_id ~semantics ~program_hash ~program_ternary
      ~input_ternaries ~fuel ~size_cap ~status ~result_ternary ~result_hash
      ~step_count ~parent_run_id ~journal
  in
  { d_run_id = run_id
  ; d_semantics = semantics
  ; d_program_hash = program_hash
  ; d_program_ternary = program_ternary
  ; d_input_hashes = input_hashes
  ; d_input_ternaries = input_ternaries
  ; d_fuel = fuel
  ; d_size_cap = size_cap
  ; d_status = status
  ; d_result_ternary = result_ternary
  ; d_result_hash = result_hash
  ; d_step_count = step_count
  ; d_parent_run_id = parent_run_id
  ; d_journal = journal
  ; d_deriv_canon = c
  ; d_deriv_id = id_of_canon c }

(* -- the offline checker --------------------------------------------- *)

exception Check_failed of string

(* Journal-fed engine over the identity monad: ONE Prim_eval instance,
   same evaluation order and step accounting as the server boundary.
   Prim calls are answered sequentially from the embedded journal -
   recorded args must equal replayed args, recorded results are
   returned verbatim, no grant check, no live host. *)
module Eng = Tuna_interp.Prim_eval.Make (struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let catch f h = try f () with e -> h e
end)

let outcome_string = function
  | Eng.Normal _ -> "normal"
  | Eng.Loop _ -> "loop"
  | Eng.Fuel_exhausted _ -> "fuel_exhausted"
  | Eng.Size_exhausted _ -> "size_exhausted"
  | Eng.Deadline_exceeded _ -> "deadline_exceeded"

let outcome_ternary = function
  | Eng.Normal (t, _) -> Some (Tuna.Canon.encode t)
  | Eng.Loop _ | Eng.Fuel_exhausted _ | Eng.Size_exhausted _
  | Eng.Deadline_exceeded _ -> None

let outcome_steps = function
  | Eng.Normal (_, s) | Eng.Loop s | Eng.Fuel_exhausted s
  | Eng.Size_exhausted s | Eng.Deadline_exceeded s -> s

let outcome_hash o = Option.map Tuna.Hash.hex_of_string (outcome_ternary o)

(* The five tests. Returns () on success; raises Check_failed with the
   first broken invariant. *)
let check (d : t) : unit =
  (* 1. canon consistency *)
  let c = canon_of d in
  if c <> d.d_deriv_canon then
    raise (Check_failed "canonical encoding does not match deriv_canon");
  (* 2. the id *)
  if id_of_canon c <> d.d_deriv_id then
    raise (Check_failed "sha256(deriv_canon) does not match deriv_id");
  (* 3. journal chain: linkage + per-row re-hash (Store.row_hash
     encoding, genesis anchor for seq 0) *)
  let rows = Array.of_list d.d_journal in
  Array.iteri
    (fun i (j : Journal.t) ->
      if j.Journal.j_seq <> i then
        raise
          (Check_failed
             (Printf.sprintf "journal seq %d out of order at position %d"
                j.Journal.j_seq i));
      let expected_prev = if i = 0 then genesis else rows.(i - 1).Journal.j_row_hash in
      if j.Journal.j_prev_hash <> expected_prev then
        raise
          (Check_failed
             (Printf.sprintf "journal seq %d: prev_hash linkage broken"
                j.Journal.j_seq));
      if Journal.row_fingerprint ~run_id:d.d_run_id j <> j.Journal.j_row_hash
      then
        raise
          (Check_failed
             (Printf.sprintf "journal seq %d: row_hash mismatch" j.Journal.j_seq)))
    rows;
  (* 4. content addressing *)
  if Tuna.Hash.hex_of_string d.d_program_ternary <> d.d_program_hash then
    raise (Check_failed "program_ternary does not hash to program_hash");
  if List.length d.d_input_hashes <> List.length d.d_input_ternaries then
    raise (Check_failed "input_hashes/input_ternaries count mismatch");
  List.iteri
    (fun i t ->
      if Tuna.Hash.hex_of_string t <> List.nth d.d_input_hashes i then
        raise
          (Check_failed
             (Printf.sprintf "input %d does not hash to its recorded id" i)))
    d.d_input_ternaries;
  (match (d.d_result_ternary, d.d_result_hash) with
   | Some t, Some h when Tuna.Hash.hex_of_string t <> h ->
       raise (Check_failed "result_ternary does not hash to result_hash")
   | Some _, None | None, Some _ ->
       raise (Check_failed "result_ternary/result_hash must come together")
   | _ -> ());
  (* 5. replay identity over the pure engine *)
  let program =
    match Tuna.Canon.of_string d.d_program_ternary with
    | Ok t -> t
    | Error (off, msg) ->
        raise
          (Check_failed
             (Printf.sprintf
                "program_ternary unparseable at offset %d: %s" off msg))
  in
  let inputs =
    List.map
      (fun s ->
        match Tuna.Canon.of_string s with
        | Ok t -> t
        | Error (off, msg) ->
            raise
              (Check_failed
                 (Printf.sprintf "input unparseable at offset %d: %s" off msg)))
      d.d_input_ternaries
  in
  let next = ref 0 in
  let host ~site:_ ~name ~args =
    if !next >= Array.length rows then
      raise
        (Check_failed
           (Printf.sprintf
              "replay called prim %S past the journal (no recorded answer at \
               position %d)"
              name !next));
    let row = rows.(!next) in
    if row.Journal.j_prim <> name then
      raise
        (Check_failed
           (Printf.sprintf
              "journal position %d (seq %d) records prim %S but the run called \
               %S"
              !next row.Journal.j_seq row.Journal.j_prim name));
    (match row.Journal.j_args_ternary with
     | Some recorded ->
         if recorded <> Tuna.Canon.encode args then
           raise
             (Check_failed
                (Printf.sprintf
                   "journal seq %d: recorded args differ from the replayed call"
                   row.Journal.j_seq))
     | None -> ());
    let answer =
      match (row.Journal.j_result_ternary, row.Journal.j_error) with
      | Some t, _ -> (
          match Tuna.Canon.of_string t with
          | Ok tr -> `Ok tr
          | Error (off, msg) ->
              raise
                (Check_failed
                   (Printf.sprintf
                      "journal seq %d: recorded result unparseable at offset \
                       %d: %s"
                      row.Journal.j_seq off msg)))
      | None, Some e -> `Error e
      | None, None ->
          raise
            (Check_failed
               (Printf.sprintf
                  "journal seq %d has neither result nor error"
                  row.Journal.j_seq))
    in
    incr next;
    answer
  in
  let mode =
    match d.d_semantics with
    | "v0" -> Eng.Canonical
    | "v1" -> Eng.Sharing
    | s -> raise (Check_failed (Printf.sprintf "unknown semantics %S" s))
  in
  let outcome =
    try
      Eng.eval ~host ~mode ~fuel:d.d_fuel ~size_cap:d.d_size_cap
        ~deadline:Float.infinity ~program inputs
    with
    | Check_failed _ as e -> raise e
    | e -> raise (Check_failed (Printexc.to_string e))
  in
  if !next < Array.length rows then
    raise
      (Check_failed
         (Printf.sprintf "journal holds %d unconsumed rows after the replay"
            (Array.length rows - !next)));
  if outcome_string outcome <> status_to_string d.d_status then
    raise
      (Check_failed
         (Printf.sprintf "status mismatch: record claims %s, replay produced %s"
            (status_to_string d.d_status) (outcome_string outcome)));
  if outcome_hash outcome <> d.d_result_hash then
    raise
      (Check_failed
         (Printf.sprintf "result mismatch: record claims %s, replay produced %s"
            (match d.d_result_hash with Some h -> h | None -> "none")
            (match outcome_hash outcome with Some h -> h | None -> "none")));
  if outcome_steps outcome <> d.d_step_count then
    raise
      (Check_failed
         (Printf.sprintf
            "step-count mismatch: record claims %d, replay produced %d"
            d.d_step_count (outcome_steps outcome)))

(* Convenience: check + classify for tools and APIs. *)
type verdict = Verified | Failed of string

let verify (d : t) : verdict =
  try check d; Verified with Check_failed msg -> Failed msg
