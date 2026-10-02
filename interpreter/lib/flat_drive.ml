(* Tuna_interp.Flat_drive: drive the pure-step core (Flat) from a monad.

   THE POINT OF THIS MODULE.  `Flat` is the pure core: `step` is one
   elementary reduction, a prim call parks the machine in a `pending`
   VALUE, and `answer` resumes it.  Nothing in `Flat` names a monad or a
   scheduler.  This module is the HOST-side adapter: it sequences `step`
   calls in a monad `M` (Lwt at the server boundary, identity in pure
   tests) so the production engine can be the pure-step core while the
   host keeps its effects.  The monad lives HERE, at the rim — not in the
   core — which is exactly the thesis's "the host, and only the host,
   owns the runtime".

   API parity with `Prim_eval.Make`: [eval ~host ~mode ~fuel ~size_cap
   ~program args] returns a `Prim_eval.result` in `M`.  The host is the
   monadic `Prim_eval.Make(M).host` shape ([`Ok | `Error] M.t), so Run
   and Replay can swap engines without touching their boundary code.

   The core stays pure: this adapter holds no evaluation state itself —
   it only calls `Flat.step` (a pure state transition) and binds the
   host's answer back in via `Flat.answer`. *)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module Make (M : MONAD) = struct
  type result = Prim_eval.result =
    | Normal of Tuna.Tree.t * int
    | Loop of int
    | Fuel_exhausted of int
    | Size_exhausted of int
    | Deadline_exceeded of int

  type mode = Prim_eval.mode = Canonical | Sharing

  (* trace vocabulary re-exported so a caller that reaches it through
     the engine instance (Eng.new_trace, e.Eng.tr_raw, ...) is
     unchanged from the recursive instantiation *)
  type trace_kind = Prim_eval.trace_kind = Kfire | Kcharge | Kdirty | Kloop
  type trace_event = Prim_eval.trace_event =
    { e_seq : int
    ; e_kind : trace_kind
    ; e_rule : string
    ; e_fun : string
    ; e_arg : string
    ; e_note : string
    }
  type trace = Prim_eval.trace =
    { mutable tr_cap : int
    ; mutable tr_next : int
    ; mutable tr_truncated : bool
    ; mutable tr_raw : int
    ; mutable tr_hits : int
    ; mutable tr_dirty : int
    ; mutable tr_loop : bool
    ; tr_events : trace_event Queue.t
    ; tr_digests : string Prim_eval.Phys_map.t
    }

  let new_trace = Prim_eval.new_trace
  let kind_string = Prim_eval.kind_string

  type host =
    site:int -> name:string -> args:Tuna.Tree.t ->
    [ `Ok of Tuna.Tree.t | `Error of string ] M.t

  (* Driver with an optional host-side wall-clock deadline.  Per the
     RUNTIME PURITY THESIS the core is untimed; the boundary owns
     operator policy, so the clock is read HERE, between elementary
     steps, never inside a pure step.  Polled every 256 steps so the
     clock stays off the hot path; a deadline abort returns
     Deadline_exceeded with the steps taken so far. *)
  let drive ?(deadline = Float.infinity) ~host m =
    let polls = ref 0 in
    let rec go () =
      M.bind (M.return ()) (fun () ->
        incr polls;
        if !polls land 0xff = 0 && deadline <> Float.infinity
           && Unix.gettimeofday () > deadline
        then begin
          (* finalize as a wall-clock abort at the boundary *)
          Flat.abort m (Prim_eval.Deadline_exceeded (Flat.steps m));
          M.return ()
        end
        else if Flat.result m <> None then M.return ()
        else
          match Flat.pending m with
          | Some (site, name, args) ->
              M.bind (host ~site ~name ~args) (function
                | `Ok t -> Flat.answer m (`Done t); go ()
                | `Error e -> Flat.answer m (`Error e); go ())
          | None ->
              ignore (Flat.step m);
              go ())
    in
    go ()

  let eval ?(host : host =
             fun ~site:_ ~name:_ ~args:_ ->
               M.return (`Error "no prim host at this boundary"))
      ?(deadline = Float.infinity) ?(mode = Canonical) ?trace ~fuel ~size_cap
      ~program args : result M.t =
    let m = Flat.start ~mode ?trace ~fuel ~size_cap ~program ~args () in
    M.catch
      (fun () ->
        M.bind (drive ~deadline ~host m) (fun () ->
            M.return
              (Option.value (Flat.result m)
                 ~default:(Prim_eval.Normal (Flat.value m, Flat.steps m)))))
      (fun e -> M.bind (M.return ()) (fun () -> raise e))
end
