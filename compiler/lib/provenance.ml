(* Tuna provenance: the bidirectional map between compiled-tree paths
   and IR nodes, plus span resolution for diagnostics
   (borg/call-sites.borg, provenance; SPEC.md §5).

   Every compiled-tree node carries the id of the IR node responsible
   for it (tags in Bracket.artifact). This module resolves:
     - tree path -> IR node id + IR structural path + source span
     - IR node id -> every tree path it is responsible for

   Recompilation of unchanged source must produce the same program
   hash AND the same tags (compiler determinism is tested). *)

type t = {
  tags : (Bracket.tree_path * int) list;  (* pre-order, from artifact *)
  ir : Ir.t;
  ternary : string;
  hash_hex : string;
  steps : int;
}

let of_artifact (a : Bracket.artifact) : t =
  { tags = a.tags; ir = a.ir; ternary = a.ternary; hash_hex = a.hash_hex; steps = a.steps }

(* tree path -> IR node id (None if the path is out of bounds — the
   tags list covers exactly the compiled tree's nodes). *)
let tag_at (t : t) (p : Bracket.tree_path) : int option =
  match List.assoc_opt p t.tags with Some id -> Some id | None -> None

(* IR node id -> every tree path it is responsible for. *)
let paths_of_tag (t : t) (id : int) : Bracket.tree_path list =
  List.filter_map
    (fun (p, tag) -> if tag = id then Some p else None)
    t.tags

(* IR node id -> IR structural path (root-relative). *)
let ir_path_of_tag (t : t) (id : int) : Ir.path option =
  Ir.find_id t.ir id

(* tree path -> human diagnostic line: IR path + span when resolvable. *)
let describe (t : t) (p : Bracket.tree_path) : string =
  match tag_at t p with
  | None -> Printf.sprintf "tree path %s" (Ir.show_path p)
  | Some id -> (
      match Ir.find_id t.ir id with
      | None -> Printf.sprintf "tree path %s: IR node #%d" (Ir.show_path p) id
      | Some ip ->
          let node = Ir.at_path t.ir ip in
          let sp =
            match node with
            | Some n -> Ir.show_span (Ir.span_of n)
            | None -> "?"
          in
          Printf.sprintf "tree path %s: IR path %s (node #%d), %s"
            (Ir.show_path p)
            (Ir.show_path ip) id sp)
