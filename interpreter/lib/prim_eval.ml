(* Tuna_interp.Prim_eval: the answer-monadic engine.

   ONE verbatim port of the upstream triage rules (same match arms,
   same evaluation order as reference/tree-calculus/implementation/
   ocaml/lib/tree.ml), parameterized over the monad in which the host
   answers prim calls.  The pure engine (Eval) and the journalling
   host boundary (server Run.execute) therefore share a single
   evaluation order and a single step counter — faithful replay is
   an identity by construction, not by discipline.

   Step counting (AGENTS.md rule 4): a step is one firing of a triage
   rule (the Fork cases).  The two wrapper applications and prim
   calls consume no fuel: the boundary is not part of the calculus.
   Prim arguments are reduced first (innermost), under the same
   fuel/size budget; the answer tree is size-checked against the cap.

   Prim convention (Tuna.Cprim): a prim call is the application of a
   gate tree; when [apply] is entered with a gate tree in function
   position, the host answers: [`Ok tree] becomes the value, [`Error
   msg] becomes the canonical error tree (Stem (Cstr.encode msg)) so
   the calculus keeps computing deterministically.  Prim calls may
   also appear purely as data (a gate tree is inert until applied).

   Modes (borg/sharing.borg): one engine, two accounting laws.
   [Canonical] is v0: every firing costs a step, exactly as above.
   [Sharing] is v1, the distinct-work law: a firing is the
   (fun-tree, arg-tree) pair keyed by content digest; the FIRST firing
   of a pair costs a step and records its answer, every later firing
   of the same pair is a free memo hit, and re-entering a pair that is
   still in flight is genuine divergence under leftmost-innermost —
   reported finitely as [Loop], never by burning fuel.  A firing whose
   evaluation answered a prim is never memoized (the dirty rule: grant
   liveness and journal audit need every textual call to re-execute);
   purity is decided by a monotonic host tick, so dirt propagates to
    enclosing firings exactly.  The memo is per-run state — prim
    answers are per-run facts (grants, kv, allowlist), so a cross-run
    table would cache a lie.

    Trace (borg/trace.borg): an optional, capped, per-firing event log
    over the counted firings.  Recording never touches fuel, steps,
    evaluation order, or the clock poll — an event is noted AFTER the
    counting law has spoken, so a traced run and an untraced run of the
    same program+inputs agree on every number.  v1 charge/dirty/loop
    events reuse the digests the memo law already computed (free); pure
    v0 fire events digest small trees only, for a bounded display cost. *)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* [catch run handler] — [run] is a thunk so that synchronous (eager
     monad) raises inside it are seen by the handler too. *)
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module Make (M : MONAD) = struct
  open Tuna.Tree

  (* The host boundary: answers one prim call.  Never raises — denial
     and failure are answers (AGENTS.md rule 7). *)
  type host =
    site:int -> name:string -> args:t -> [ `Ok of t | `Error of string ] M.t

  type result =
    | Normal of t * int  (* normal form, steps taken *)
    | Loop of int  (* v1: firing re-entered in flight; steps taken *)
    | Fuel_exhausted of int  (* steps taken before fuel ran out *)
    | Size_exhausted of int  (* steps taken before size_cap was hit *)
    | Deadline_exceeded of int
        (* wall-clock budget (operator policy at the run boundary), not
           a calculus budget: steps taken before the deadline passed *)

  type mode = Canonical | Sharing

  (* Digests are carried as RAW 32-BYTE STRINGS, not Digestif.SHA256.t:
     the digest module's [t] is abstract behind its signature, so each
     Hashtbl.Make application would mint an incompatible key type and
     the pair key could never flow between the tables.  Strings keep
     the tables plain and the equality content-based. *)
  module Phys_map = Hashtbl.Make (struct
      type t = Tuna.Tree.t

      let equal = ( == )
      let hash = Hashtbl.hash
    end)

  type sharing = {
    node_digests : string Phys_map.t
        (* physical node -> raw content digest, once per object *)
  ; answers : (string * string, t) Hashtbl.t
        (* firing pair -> completed answer *)
  ; inflight : (string * string, int) Hashtbl.t
        (* firing pair -> host tick at entry (loop + purity bookkeeping) *)
  ; mutable host_tick : int  (* monotonic count of host answers *)
  }

  (* -- trace (borg/trace.borg): opt-in, capped firing observability --- *)

  type trace_kind = Kfire | Kcharge | Kdirty | Kloop

  let kind_string = function
    | Kfire -> "fire"
    | Kcharge -> "charge"
    | Kdirty -> "dirty"
    | Kloop -> "loop"

  type trace_event = {
    e_seq : int
  ; e_kind : trace_kind
  ; e_rule : string  (* triage arm; "" when not applicable *)
  ; e_fun : string  (* 16-hex content-digest prefix; "" when n/a *)
  ; e_arg : string  (* 16-hex content-digest prefix; "" when n/a *)
  ; e_note : string
  }

  type trace = {
    mutable tr_cap : int  (* max RECORDED events; counters are uncapped *)
  ; mutable tr_next : int  (* next event seq *)
  ; mutable tr_truncated : bool
  ; mutable tr_raw : int  (* firing-gate entries = raw firings, both laws *)
  ; mutable tr_hits : int  (* v1 memo hits; never recorded as events *)
  ; mutable tr_dirty : int  (* v1 dirty re-executions *)
  ; mutable tr_loop : bool
  ; tr_events : trace_event Queue.t
  ; tr_digests : string Phys_map.t  (* display digest cache, trace-local *)
  }

  let new_trace ?(cap = 50_000) () =
    { tr_cap = cap; tr_next = 0; tr_truncated = false; tr_raw = 0
    ; tr_hits = 0; tr_dirty = 0; tr_loop = false
    ; tr_events = Queue.create (); tr_digests = Phys_map.create 4096 }

  (* Display digest: the SAME structural content law the v1 memo keys
     use, cached per physical node.  v1 events reuse the memo law's
     already-computed digests (free); pure-v0 traces digest on demand
     here, small trees only. *)
  let rec trace_digest tr t =
    match Phys_map.find_opt tr.tr_digests t with
    | Some d -> d
    | None ->
        let raw =
          match t with
          | Leaf -> Digestif.SHA256.digest_string "\x00"
          | Stem a -> Digestif.SHA256.digest_string ("\x01" ^ trace_digest tr a)
          | Fork (a, c) ->
              Digestif.SHA256.digest_string
                ("\x02" ^ trace_digest tr a ^ trace_digest tr c)
        in
        let d = Digestif.SHA256.to_raw_string raw in
        Phys_map.replace tr.tr_digests t d;
        d

  let hex16 raw =
    String.sub (Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string raw)) 0 16

  (* The triage arm [dispatch_firing] will take, recorded on fire/charge
     events so a trace reads as the rule sequence, not just a count. *)
  let triage_name a c =
    match (a, c) with
    | Fork (Leaf, _), _ -> "fork(leaf,_)"
    | Fork (Stem _, _), _ -> "fork(stem,_)"
    | Fork (Fork _, _), Leaf -> "fork(fork,_)/leaf"
    | Fork (Fork _, _), Stem _ -> "fork(fork,_)/stem"
    | Fork (Fork _, _), Fork _ -> "fork(fork,_)/fork"
    | _ -> "wrapper"

  type budget =
    { mutable fuel : int
    ; mutable steps : int
    ; size_cap : int
    ; mutable ops : int  (* applications entered; gates the clock poll *)
    ; deadline : float  (* absolute Unix time; infinity = no deadline *)
    ; sharing : sharing option  (* v1 state; None = canonical v0 *)
    ; trace : trace option  (* borg/trace.borg: None = untraced *)
    }

  exception Fuel_out
  exception Size_out
  exception Deadline_out
  exception Loop_out  (* v1: a firing re-entered while still in flight *)

  (* Wall-clock deadline (AGENTS.md rule 5 addendum): polled every
     [deadline_granularity] applications so the clock read stays off
     the hot path.  Pure evaluation passes no deadline (infinity) and
     pays one integer compare per [deadline_granularity] ops - step
     counts and evaluation order are untouched. *)
  let deadline_granularity = 4096

  let check_deadline b =
    b.ops <- b.ops + 1;
    if b.ops land (deadline_granularity - 1) = 0
       && b.deadline <> Float.infinity
       && Unix.gettimeofday () > b.deadline
    then raise Deadline_out

  let check_size b t = if size t > b.size_cap then raise Size_out

  (* Fire one triage rule: consume fuel or abort. *)
  let fire b =
    if b.fuel = 0 then raise Fuel_out;
    b.fuel <- b.fuel - 1;
    b.steps <- b.steps + 1

  (* Note one trace event if under the cap; past the cap, flip
     [truncated] and record nothing more.  An emit NEVER precedes the
     counting law for the same firing — [fire] (the step) always speaks
     first. *)
  let emit b kind rule fd ad note =
    match b.trace with
    | None -> ()
    | Some tr ->
        if tr.tr_next < tr.tr_cap then begin
          Queue.add
            { e_seq = tr.tr_next
            ; e_kind = kind
            ; e_rule = rule
            ; e_fun = (match fd with Some d -> hex16 d | None -> "")
            ; e_arg = (match ad with Some d -> hex16 d | None -> "")
            ; e_note = note }
            tr.tr_events;
          tr.tr_next <- tr.tr_next + 1
        end
        else tr.tr_truncated <- true

  (* v1 content digest, cached per physical node; raw 32-byte string. *)
  let rec digest_of s t =
    match Phys_map.find_opt s.node_digests t with
    | Some d -> d
    | None ->
        let d =
          match t with
          | Leaf ->
              Digestif.SHA256.digest_string "\x00"
              |> Digestif.SHA256.to_raw_string
          | Stem a ->
              Digestif.SHA256.digest_string
                ("\x01" ^ digest_of s a)
              |> Digestif.SHA256.to_raw_string
          | Fork (a, c) ->
              Digestif.SHA256.digest_string
                ("\x02" ^ digest_of s a ^ digest_of s c)
              |> Digestif.SHA256.to_raw_string
        in
        Phys_map.replace s.node_digests t d;
        d

  (* v1 memo gate on the firing (a, c): [`Hit answer] replays a
     completed pair for free; [`Fresh tick0] means the pair was just
     counted (fuel + step) and marked in flight.  Re-entering a pair
     that is still in flight is divergence (Loop_out) — under
     leftmost-innermost the computation recurses on its own subproblem
     with nothing in between, and v0 could only report that as fuel
     exhaustion after burning the whole budget. *)
  let share_gate b a c =
    (match b.trace with Some tr -> tr.tr_raw <- tr.tr_raw + 1 | None -> ());
    match b.sharing with
    | None ->
        fire b;
        (* v0 display digests: small trees only — the trace must never
           turn a cheap untraced run into an unbounded digest job *)
        (match b.trace with
         | Some tr ->
             let digest_small t =
               if size t <= 4096 then Some (trace_digest tr t) else None
             in
             let note =
               if size a <= 4096 && size c <= 4096 then ""
               else "tree too large to digest (display omitted)"
             in
             emit b Kfire (triage_name a c) (digest_small a) (digest_small c) note
         | None -> ());
        `Fresh 0
    | Some s -> (
        let key = (digest_of s a, digest_of s c) in
        match Hashtbl.find_opt s.answers key with
        | Some answer ->
            (match b.trace with
             | Some tr -> tr.tr_hits <- tr.tr_hits + 1
             | None -> ());
            `Hit answer
        | None -> (
            match Hashtbl.find_opt s.inflight key with
            | Some _ ->
                (* genuine divergence: note the offending pair, then the
                   loop law answers it finitely at the boundary *)
                (match b.trace with
                 | Some tr ->
                     tr.tr_loop <- true;
                     emit b Kloop "" (Some (fst key)) (Some (snd key))
                       "in-flight re-entry"
                 | None -> ());
                raise Loop_out
            | None ->
                fire b;
                Hashtbl.replace s.inflight key s.host_tick;
                emit b Kcharge (triage_name a c) (Some (fst key)) (Some (snd key)) "";
                `Fresh s.host_tick))

  (* v1 finalize: leave the in-flight set; memoize the answer iff the
     firing was clean — no host answer inside it (the dirty rule, the
     prim-exemption law of borg/sharing.borg). *)
  let share_finish b a c ~tick0 r =
    match b.sharing with
    | None -> ()
    | Some s ->
        let key = (digest_of s a, digest_of s c) in
        Hashtbl.remove s.inflight key;
        if s.host_tick = tick0 then Hashtbl.replace s.answers key r
        else
          (* the dirty rule: a prim answered inside, so this firing
             re-executes on every occurrence and never memoizes *)
          (match b.trace with
           | Some tr ->
               tr.tr_dirty <- tr.tr_dirty + 1;
               emit b Kdirty "" (Some (fst key)) (Some (snd key))
                 "prim answered inside; re-executes"
           | None -> ())

  let rec apply b host a c =
    check_deadline b;
    match Tuna.Cprim.shape a with
    | Some (name, site) -> prim_call b host ~site ~name c
    | None -> (
        match a with
        | Leaf ->
            (* application, not a step *)
            let r = Stem c in
            check_size b r;
            M.return r
        | Stem a1 ->
            (* application, not a step *)
            let r = Fork (a1, c) in
            check_size b r;
            M.return r
        | Fork _ -> (
            (* triage firing: v0 counts it and dispatches; v1 asks the
               memo gate first (counting-law). *)
            match share_gate b a c with
            | `Hit answer -> M.return answer
            | `Fresh tick0 ->
                M.bind (dispatch_firing b host a c) (fun r ->
                    share_finish b a c ~tick0 r;
                    M.return r)))

  (* The three triage arms, verbatim (post-gate). *)
  and dispatch_firing b host a c =
    match a with
    | Fork (Leaf, a1) ->
        (* triage rule fork(leaf,x) -> x *)
        M.return a1
    | Fork (Stem a1, a2) ->
        (* triage rule fork(stem,_): inner a1 first, then a2, then
           the outer *)
        M.bind (apply b host a1 c) (fun l ->
            M.bind (apply b host a2 c) (fun r -> apply b host l r))
    | Fork (Fork (a1, a2), a3) ->
        (* triage rule fork(fork,_) *)
        (match c with
         | Leaf -> M.return a1
         | Stem u -> apply b host a2 u
         | Fork (u, v) ->
             M.bind (apply b host a3 u) (fun l -> apply b host l v))
    | _ -> assert false (* wrapper arms are handled in [apply] *)

  (* A prim call: the args tree is passed AS REDUCED BY THE STRATEGY
     (trees are values; there is no separate argument normalizer in
     the calculus).  The host's answer becomes the value of the
     application; errors become the canonical error tree.  Every host
     answer bumps the v1 host tick — the firing turns dirty and its
     answer will not be memoized. *)
  and prim_call b host ~site ~name c =
    M.bind (M.return ()) (fun () ->
        M.bind (host ~site ~name ~args:c) (function
          | `Ok r ->
              (match b.sharing with
               | Some s -> s.host_tick <- s.host_tick + 1
               | None -> ());
              check_size b r;
              M.return r
          | `Error msg ->
              (match b.sharing with
               | Some s -> s.host_tick <- s.host_tick + 1
               | None -> ());
              let e = Stem (Tuna.Cstr.encode msg) in
              check_size b e;
              M.return e))

  (* Evaluate [program] against [args], folding application left to
     right: r0 = program, r1 = apply r0 arg1, r2 = apply r1 arg2, ...
     size_cap bounds the live tree: the program, every argument, and
     every intermediate result are checked.

     Everything that can raise (budget checks, the host) happens
     inside a bind callback so [catch] sees it for both monads. *)
    let eval ?(host : host =
                fun ~site:_ ~name:_ ~args:_ ->
                  M.return (`Error "no prim host at this boundary"))
        ?(deadline = Float.infinity) ?(mode = Canonical) ?trace ~fuel
        ~size_cap ~program args : result M.t =
    let sharing =
      match mode with
      | Canonical -> None
      | Sharing ->
          Some
            { node_digests = Phys_map.create 4096
            ; answers = Hashtbl.create 4096
            ; inflight = Hashtbl.create 64
            ; host_tick = 0 }
    in
      let b =
        { fuel; steps = 0; size_cap; ops = 0; deadline; sharing; trace }
      in
    let rec go acc = function
      | [] -> M.return acc
      | arg :: rest ->
          M.bind (apply b host acc arg) (fun r ->
              check_size b r;
              go r rest)
    in
    M.catch (fun () ->
        check_size b program;
        List.iter (check_size b) args;
        M.bind (go program args) (fun t -> M.return (Normal (t, b.steps))))
      (function
        | Fuel_out -> M.return (Fuel_exhausted b.steps)
        | Loop_out -> M.return (Loop b.steps)
        | Size_out -> M.return (Size_exhausted b.steps)
        | Deadline_out -> M.return (Deadline_exceeded b.steps)
        | e -> M.bind (M.return ()) (fun () -> raise e))
end
