(* Tuna.Cprim: the prim-call tree convention (borg/call-sites,
   journal.row-schema).

   The calculus has no I/O (SPEC.md §2 rule 3); a run touches the world
   exclusively through primitive calls.  v0 convention: a prim call is
   the APPLICATION of a gate tree:

     gate       = Stem (Stem (Stem (Stem Leaf)))     ternary "11110"
     callsite   = Fork (Cstr.encode name, Cstr.unary site)

   so the prim fires exactly when the engine reduces

     apply (Fork (gate, Fork (name, site))) args_tree

   The gate is intercepted by the engine BEFORE triage fires, which is
   what makes it inert until applied. [site] is the callsite identity —
   the IR node id of the prim form when compiled from source — and the
   run maps it back to tree/IR paths and spans via the program's
   provenance (journal rows carry the resolved path).

   Programs may also write the call shape by hand out of raw ternary;
   [site] then just needs to be some unary tree. *)

open Tree

let gate : t = Stem (Stem (Stem (Stem Leaf)))

let gate_ternary = "11110"

let call_tree ~name ~site : t =
  Fork (gate, Fork (Cstr.encode name, Cstr.unary site))

(* The (name, site) a tree denotes as a prim call, if any. *)
let shape (t : t) : (string * int) option =
  match t with
  | Fork (g, Fork (name_t, site_t)) when g = gate -> (
      match Cstr.decode name_t with
      | Some name -> (
          match Cstr.unary_of site_t with
          | Some site -> Some (name, site)
          | None -> None)
      | None -> None)
  | _ -> None
