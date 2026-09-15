(* Structural CAS patch (SPEC.md §4 item 5): a patch addresses a subtree
   by canonical tree path, pins the expected hash of the old subtree,
   and supplies the replacement subtree.

   Application is atomic and content-addressed: a successful patch
   produces a NEW program (new hash); the old program row stays as
   immutable history.  On hash mismatch the patch is rejected with a
   first-diff walk between the stored subtree and the tree the caller
   based its expectation on (only derivable when the caller supplies
   [old_ternary]; otherwise the 409 carries the two subtree hashes at
   the patch path).

   Path convention (matches ternary digits / provenance):
   0 = stem child, 1 = fork left, 2 = fork right.  Optional leading
   slash tolerated ("/012" and "012" are the same path; "" is root). *)

open Tuna.Tree

type outcome =
  | Applied of { ternary : string; hash : string }
  | Conflict of {
      expected_old_hash : string
    ; actual_old_hash : string
    ; first_diff : string option  (* relative to the patch path *)
    }
  | Bad_path of string

let normalize_path path =
  let p =
    if String.length path > 0 && path.[0] = '/' then
      String.sub path 1 (String.length path - 1)
    else path
  in
  if String.for_all (fun c -> c = '0' || c = '1' || c = '2') p then Some p
  else None

(* Navigate by path digits; None when the path escapes the tree. *)
let at_path t path =
  let rec go t = function
    | [] -> Some t
    | d :: ds -> (
        match (t, d) with
        | Stem c, '0' -> go c ds
        | Fork (l, _), '1' -> go l ds
        | Fork (_, r), '2' -> go r ds
        | _ -> None)
  in
  go t path

let replace t new_sub path =
  let rec go t = function
    | [] -> new_sub
    | d :: ds -> (
        match (t, d) with
        | Stem c, '0' -> Stem (go c ds)
        | Fork (l, r), '1' -> Fork (go l ds, r)
        | Fork (l, r), '2' -> Fork (l, go r ds)
        | _ -> invalid_arg "patch.replace: path escapes tree (checked by at_path)")
  in
  go t path

(* Deepest path (relative to the compared roots) where two trees
   diverge in shape.  "" means the roots themselves differ. *)
let first_diff : Tuna.Tree.t -> Tuna.Tree.t -> string option =
 let rec go a b =
    match (a, b) with
    | Leaf, Leaf -> None
    | Stem x, Stem y -> Option.map (fun p -> "0" ^ p) (go x y)
    | Fork (a1, a2), Fork (b1, b2) -> (
        match go a1 b1 with
        | Some p -> Some ("1" ^ p)
        | None -> Option.map (fun p -> "2" ^ p) (go a2 b2))
    | _ -> Some ""
  in
  fun a b ->
    match (a, b) with Leaf, Leaf -> None | _ -> go a b

let digits s = List.init (String.length s) (fun i -> s.[i])

(* [old_tree] is the stored program; [believed] is the tree the caller
   expected (optional, from its own pre-patch copy). *)
let apply_patch ~old_tree ~path ~believed ~expected_old_hash ~new_sub : outcome =
  match normalize_path path with
  | None -> Bad_path path
  | Some p -> (
      match at_path old_tree (digits p) with
      | None -> Bad_path path
      | Some sub ->
          let actual = Tuna.Hash.hex_of_tree sub in
          if String.equal actual expected_old_hash then
            let t' = replace old_tree new_sub (digits p) in
            let ternary = Tuna.Canon.encode t' in
            Applied { ternary; hash = Tuna.Hash.hex_of_tree t' }
          else
            let first_diff =
              match believed with
              | Some b when String.equal (Tuna.Hash.hex_of_tree b) expected_old_hash
                ->
                  (* the caller's believed tree matches its pinned hash:
                     the divergence is between its tree and ours *)
                  first_diff b sub
              | _ -> None
            in
            Conflict { expected_old_hash; actual_old_hash = actual; first_diff })
