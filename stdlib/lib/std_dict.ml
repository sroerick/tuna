(* Tuna_stdlib.Std_dict: the frozen stdlib v1 as the reader's
   dictionary (borg/local.borg L1).  Offline front ends pass
   [dictionary ()] to Sexp.parse / Bracket.compile_source, so a name
   resolves as it does on a server for an identity with no defs:
   lambda param > std > reader builtins.  Rows come from
   stdlib/v1/trees, the frozen manifest, not from any instance's
   database. *)

let rows = Std_trees.rows

let parse_row (name, _hash, ternary) =
  match Tuna.Canon.of_string ternary with
  | Ok t -> (name, t)
  | Error (off, msg) ->
      failwith
        (Printf.sprintf "stdlib row %s: bad ternary at %d: %s" name off msg)

let dict = lazy (List.map parse_row rows)
let dictionary () = Lazy.force dict
