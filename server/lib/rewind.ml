(* Tuna_server.Rewind: state-as-fold over the tree_ops log (M10).

   The substrate's answer to "what was at path P at seq N": fold the
   effect log, not the live index.  Only rows that CARRIED an effect
   move the fold's index (put/cas with a value_hash and version);
   get/list rows are reads, fork marker rows are superseded by the one
   put row per copied path that ns_fork journals, and no-effect rows
   (denials, CAS conflicts) have no value_hash to apply.  Pure fold:
   the live index is never touched. *)

module S = Tuna_store.Store

open Lwt.Infix

type entry = { path : string; value_hash : string; version : int64 }

(* index under [prefix] as of [at_seq] (inclusive), ascending by path *)
let state pool ~prefix ~at_seq =
  S.ops_fold pool ?prefix:(Some prefix) ~from_seq:0L ~to_seq:at_seq ()
  >>= fun ops ->
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (o : S.tree_op) ->
      match (o.S.o_op, o.S.o_value_hash, o.S.o_version) with
      | ( ( "put" | "cas" )
        , Some value_hash
        , Some version ) ->
          Hashtbl.replace tbl o.S.o_path
            { path = o.S.o_path; value_hash; version }
      | _ -> ())
    ops;
  let entries = Hashtbl.fold (fun _ e acc -> e :: acc) tbl [] in
  let entries = List.sort (fun a b -> compare a.path b.path) entries in
  Lwt.return entries
