(* Tuna_interp.Flat: the pure-step core (the RUNTIME PURITY THESIS).

   Spec statement lives in tuna.borg's docstring: "tunacore admits no
   scheduler ... the evaluation core's end state is a PURE STEP — a
   value->value state transition, no monad parameter, no scheduler
   coupling, no Lwt/async types anywhere under common/, interpreter/,
   compiler/."  This module is the pure core; `Flat_drive` (a separate
   module) is the host adapter that sequences it in a monad, and the
   server Run/Replay boundaries drive the core through it — so the
   monad lives at the rim, not here.

   SHAPE.  An explicit-stack (CEK-style) abstract machine whose STATE IS
   A VALUE: [machine] is a record of control, continuation frames,
   budget, and optional v1 memo state.  [step] performs exactly one
   elementary reduction (one wrapper application, one triage firing, or
   one prim answer) and returns the successor state; [run] drives it.
   The module is plain OCaml — no Lwt, no monad parameter, no scheduler.
   Because the state is a value, suspension/resumption/inspection are
   just states the host can hold: [run] is sugar over [step].

   PRIM BOUNDARY.  The host answers prims through a plain function
   [host : site:int -> name:string -> args:t -> [`Done of t | `Error of
   string]].  The core never names Lwt; a scheduling host (server Run /
   Replay) drives [step] and may do whatever it likes around each answer.

   STEP-COUNT IDENTITY (the corpus-refereed invariant).  This is an
   INDEPENDENT implementation — it re-implements the counting law rather
   than calling the recursive engine — and tests/flat_tests.ml requires
   it to reproduce the recursive [Prim_eval] step counts bit-for-bit
   across the whole differential corpus under BOTH laws.  The order is
   the verbatim one (AGENTS.md rule 4):

     - wrapper applications (apply Leaf b, apply (Stem a) b) are
       application, not steps; they consume no fuel;
     - every triage firing costs one step and one fuel;
     - fork(stem a1,a2) evaluates a1 c, THEN a2 c, THEN the outer
       apply — the [KStemFirst]/[KStemSecond] frames encode exactly this;
     - fork(fork,a3) dispatches on the argument shape, free;
     - prim calls are fuel-free and step-free; the host answer becomes
       the application's value.

   v1 (Sharing) mirrors borg/sharing.borg exactly: the memo gate runs at
   the same firing point, the dirty rule (a prim answered inside a
   firing means it never memoizes) uses the same monotonic host tick,
   and in-flight re-entry answers Loop.

   DEADLINE.  Deliberately no clock: pure evaluation is untimed
   (AGENTS.md rule 5).  The run boundary enforces its wall-clock cap by
   stepping and checking its own clock between steps — operator policy
   stays at the host, where it belongs. *)

open Tuna.Tree

type frame =
  | KGo of t
      (* fold: after the current value v, reduce apply v arg *)
  | KStemFirst of t * t
      (* fork(stem a1,a2) on c: after a1 c = l, remember (a2,c), eval a2 c *)
  | KStemSecond of t
      (* after a2 c = r, reduce apply l r (l stored) *)
  | KForkAfterFirst of t
      (* fork(fork a1 a2,_) on Fork(u,v): after a3 u = l, reduce apply l v *)
  | KFinish of t * t * int
      (* close a gated firing (f, c, tick0) exactly once its whole
         dispatch subtree has produced an answer *)

type mode = Prim_eval.mode = Canonical | Sharing

type sharing = {
  node_digests : (t, string) Hashtbl.t
      (* physical node -> raw content digest, once per object *)
; answers : (string * string, t) Hashtbl.t
      (* firing pair -> completed answer *)
; inflight : (string * string, int) Hashtbl.t
      (* firing pair -> host tick at entry (loop + purity bookkeeping) *)
; mutable host_tick : int  (* monotonic count of host answers *)
}

type machine = {
  mutable f : t option
      (* Some f = control is "eval apply f x"; None = halted *)
; mutable x : t
; mutable k : frame list
; mutable value : t
; mutable done_ : Prim_eval.result option
; mutable pending_ : (int * string * t) option
      (* a prim call awaiting its host answer (suspension = a state value) *)
; mutable fuel : int
; mutable steps : int
; size_cap : int
; mutable sharing : sharing option
; trace : Prim_eval.trace option
}

(* -- physical-node digest cache (same content law as the memo keys) --- *)

let rec digest_of s (t : t) =
  match Hashtbl.find_opt s.node_digests t with
  | Some d -> d
  | None ->
      let d =
        match t with
        | Leaf ->
            Digestif.SHA256.digest_string "\x00" |> Digestif.SHA256.to_raw_string
        | Stem a ->
            Digestif.SHA256.digest_string ("\x01" ^ digest_of s a)
            |> Digestif.SHA256.to_raw_string
        | Fork (a, c) ->
            Digestif.SHA256.digest_string ("\x02" ^ digest_of s a ^ digest_of s c)
            |> Digestif.SHA256.to_raw_string
      in
      Hashtbl.replace s.node_digests t d;
      d

(* -- start ------------------------------------------------------------- *)

let start ?(mode = Canonical) ?trace ~fuel ~size_cap ~program ~args () =
  let sharing =
    match mode with
    | Canonical -> None
    | Sharing ->
        Some
          { node_digests = Hashtbl.create 4096
          ; answers = Hashtbl.create 4096
          ; inflight = Hashtbl.create 64
          ; host_tick = 0 }
  in
  let m =
    { f = None
    ; x = program
    ; k = []
    ; value = program
    ; done_ = None
    ; pending_ = None
    ; fuel
    ; steps = 0
    ; size_cap
    ; sharing
    ; trace
    }
  in
  if size program > size_cap || List.exists (fun a -> size a > size_cap) args
  then m.done_ <- Some (Prim_eval.Size_exhausted 0)
  else (
    match args with
    | [] ->
        m.value <- program;
        m.f <- None;
        m.done_ <- Some (Prim_eval.Normal (program, 0))
    | a0 :: rest ->
        m.f <- Some program;
        m.x <- a0;
        m.k <- List.map (fun a -> KGo a) rest);
  m

let steps m = m.steps
let value m = m.value
let result m = m.done_

(* boundary abort: finalize the machine as a non-normal result (used by
   the host driver for a wall-clock deadline — operator policy, not a
   calculus fact).  Pure state transition, like every other mutation. *)
let abort m r =
  m.f <- None;
  m.pending_ <- None;
  m.done_ <- Some r

(* a suspended prim call: (site, name, args); None when running/halted.
   Suspension is a VALUE the host can inspect, hold, or answer
   asynchronously — no monad, no scheduler in the core. *)
let pending m = m.pending_

(* -- the sharing gate / finish (verbatim v1 law, no exceptions) ------- *)

(* Note one trace event if under the cap; an emit NEVER precedes the
   counting law for the same firing.  Observation only — it touches no
   fuel, step, or evaluation-order state. *)
let emit m kind rule fd ad note =
  match m.trace with
  | None -> ()
  | Some tr ->
      if tr.Prim_eval.tr_next < tr.Prim_eval.tr_cap then begin
        Queue.add
          { Prim_eval.e_seq = tr.Prim_eval.tr_next
          ; e_kind = kind
          ; e_rule = rule
          ; e_fun = (match fd with Some d -> Prim_eval.hex16 d | None -> "")
          ; e_arg = (match ad with Some d -> Prim_eval.hex16 d | None -> "")
          ; e_note = note }
          tr.Prim_eval.tr_events;
        tr.Prim_eval.tr_next <- tr.Prim_eval.tr_next + 1
      end
      else tr.Prim_eval.tr_truncated <- true

let fire m =
  if m.fuel = 0 then `Fuel
  else begin
    m.fuel <- m.fuel - 1;
    m.steps <- m.steps + 1;
    `Fresh
  end

(* [`Fresh tick0] = counted and in flight; [`Hit answer] = memo replay;
   [`Fuel] = no distinct-work budget left; [`Loop] = in-flight re-entry. *)
let share_gate m a c =
  (match m.trace with
   | Some tr -> tr.Prim_eval.tr_raw <- tr.Prim_eval.tr_raw + 1
   | None -> ());
  match m.sharing with
  | None -> (
      match fire m with
      | `Fuel -> `Fuel
      | `Fresh ->
          (* v0 display digests: small trees only, so a trace never
             turns a cheap run into an unbounded digest job *)
          (match m.trace with
           | Some tr ->
               let digest_small t =
                 if size t <= 4096 then Some (Prim_eval.trace_digest tr t)
                 else None
               in
               let note =
                 if size a <= 4096 && size c <= 4096 then ""
                 else "tree too large to digest (display omitted)"
               in
               emit m Prim_eval.Kfire (Prim_eval.triage_name a c)
                 (digest_small a) (digest_small c) note
           | None -> ());
          `Fresh 0)
  | Some s -> (
      let key = (digest_of s a, digest_of s c) in
      match Hashtbl.find_opt s.answers key with
      | Some answer ->
          (match m.trace with
           | Some tr -> tr.Prim_eval.tr_hits <- tr.Prim_eval.tr_hits + 1
           | None -> ());
          `Hit answer
      | None -> (
          match Hashtbl.find_opt s.inflight key with
          | Some _ ->
              (match m.trace with
               | Some tr ->
                   tr.Prim_eval.tr_loop <- true;
                   emit m Prim_eval.Kloop "" (Some (fst key)) (Some (snd key))
                     "in-flight re-entry"
               | None -> ());
              `Loop
          | None -> (
              match fire m with
              | `Fuel -> `Fuel
              | `Fresh ->
                  Hashtbl.replace s.inflight key s.host_tick;
                  emit m Prim_eval.Kcharge (Prim_eval.triage_name a c)
                    (Some (fst key)) (Some (snd key)) "";
                  `Fresh s.host_tick)))

let share_finish m a c ~tick0 r =
  match m.sharing with
  | None -> ()
  | Some s ->
      let key = (digest_of s a, digest_of s c) in
      Hashtbl.remove s.inflight key;
      if s.host_tick = tick0 then Hashtbl.replace s.answers key r
      else
        (match m.trace with
         | Some tr ->
             tr.Prim_eval.tr_dirty <- tr.Prim_eval.tr_dirty + 1;
             emit m Prim_eval.Kdirty "" (Some (fst key)) (Some (snd key))
               "prim answered inside; re-executes"
         | None -> ())

(* -- deliver a completed value to the frame stack --------------------- *)

(* [deliver] is where the machine's continuation is popped.  It may halt
   (empty stack), schedule the next reduction, or run a KFinish. *)
let rec deliver m r =
  match m.k with
  | [] ->
      m.value <- r;
      m.f <- None;
      m.done_ <- Some (Prim_eval.Normal (r, m.steps))
  | frame :: rest ->
      m.k <- rest;
      (match frame with
       | KGo arg -> set_eval m r arg
       | KStemFirst (a2, xx) ->
           m.k <- KStemSecond r :: rest;
           set_eval m a2 xx
       | KStemSecond l -> set_eval m l r
       | KForkAfterFirst v -> set_eval m r v
       | KFinish (ff, cc, tick0) ->
           share_finish m ff cc ~tick0 r;
           deliver m r)

and set_eval m f x =
  m.f <- Some f;
  m.x <- x

(* size-guarded deliver: if the new value blows the cap, halt instead *)
let deliver_checked m r =
  if size r > m.size_cap then begin
    m.value <- r;
    m.f <- None;
    m.done_ <- Some (Prim_eval.Size_exhausted m.steps)
  end
  else deliver m r

(* -- one elementary step ---------------------------------------------- *)

(* [step] performs exactly ONE elementary reduction: a wrapper
   application, a triage firing, or the SUSPENSION of the machine on a
   prim call.  It returns None when halted or already suspended.  There
   is no host parameter and no scheduling: a prim call parks the machine
   in [pending_], and the driver injects the answer with [answer].  This
   is what makes the state a value. *)
let step m =
  if m.done_ <> None || m.pending_ <> None then None
  else
    match m.f with
    | None -> None  (* halted *)
    | Some f -> (
        match Tuna.Cprim.shape f with
        | Some (name, site) ->
            (* prim call: suspend; the host answers via [answer] *)
            m.pending_ <- Some (site, name, m.x);
            Some m
        | None -> (
            match f with
            | Leaf ->
                (* wrapper: application, not a step *)
                deliver_checked m (Stem m.x);
                Some m
            | Stem a1 ->
                deliver_checked m (Fork (a1, m.x));
                Some m
            | Fork _ -> (
                match share_gate m f m.x with
                | `Fuel ->
                    m.f <- None;
                    m.done_ <- Some (Prim_eval.Fuel_exhausted m.steps);
                    Some m
                | `Loop ->
                    m.f <- None;
                    m.done_ <- Some (Prim_eval.Loop m.steps);
                    Some m
                | `Hit answer ->
                    deliver m answer;
                    Some m
                | `Fresh tick0 -> (
                    (* push the finish frame BELOW the dispatch frames so
                       it runs only after the whole firing completes *)
                    m.k <- KFinish (f, m.x, tick0) :: m.k;
                    match f with
                    | Fork (Leaf, a1) ->
                        (* dispatch returns a1; finish frame does the rest *)
                        deliver m a1;
                        Some m
                    | Fork (Stem a1, a2) ->
                        m.k <- KStemFirst (a2, m.x) :: m.k;
                        set_eval m a1 m.x;
                        Some m
                    | Fork (Fork (a1, a2), a3) -> (
                        match m.x with
                        | Leaf -> deliver m a1; Some m
                        | Stem u ->
                            set_eval m a2 u;
                            Some m
                        | Fork (u, v) ->
                            m.k <- KForkAfterFirst v :: m.k;
                            set_eval m a3 u;
                            Some m)
                    | _ -> assert false))))

(* [answer] injects the host's answer for a suspended prim and resumes.
   `Done r becomes the application's value; `Error msg becomes the
   canonical error tree (Stem (Cstr.encode msg)) so the calculus keeps
   computing deterministically.  The host-tick bump is what makes any
   enclosing firing dirty under v1. *)
let answer m (r : [ `Done of t | `Error of string ]) =
  match m.pending_ with
  | None -> invalid_arg "Flat.answer: no pending prim"
  | Some _ ->
      m.pending_ <- None;
      (match m.sharing with
       | Some s -> s.host_tick <- s.host_tick + 1
       | None -> ());
      (match r with
       | `Done v -> deliver_checked m v
       | `Error msg -> deliver_checked m (Stem (Tuna.Cstr.encode msg)))

(* -- run --------------------------------------------------------------- *)

(* synchronous driver: run to the next suspension or halt.  Returns the
   machine; if [pending] is set the caller must [answer] and continue. *)
let advance m =
  let rec go () =
    if m.done_ <> None || m.pending_ <> None then ()
    else match step m with None -> () | Some _ -> go ()
  in
  go ();
  m

(* convenience: drive to completion with a synchronous host (the same
   shape the recursive engine exposes). *)
let run ~(host : site:int -> name:string -> args:t -> [ `Done of t | `Error of string ])
    m =
  let rec go () =
    ignore (advance m);
    match m.pending_ with
    | None -> ()
    | Some (site, name, args) -> answer m (host ~site ~name ~args); go ()
  in
  go ();
  Option.value m.done_ ~default:(Prim_eval.Normal (m.value, m.steps))
