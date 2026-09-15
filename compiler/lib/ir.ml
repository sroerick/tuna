(* Tuna IR: named lambda terms with source spans. Every node has a
   unique integer id and a span; the structural path from the root plus
   the id together form the diagnostic coordinate system
   (borg/call-sites.borg, provenance): the compiler's provenance map
   links compiled-tree paths to these ids/paths, and diagnostics carry
   the IR path + span when available. *)

type span = { off : int; len : int }

type t =
  | Var of { id : int; span : span; name : string }
  | Lam of { id : int; span : span; param : string; body : t }
  | App of { id : int; span : span; fn : t; arg : t }
  | Leaf_lit of { id : int; span : span }
  | Tree_lit of { id : int; span : span; tree : Tuna.Tree.t }
  | Prim of { id : int; span : span; name : string; args : t list }
  (* (prim "name" e1 .. en): a boundary call (Tuna.Cprim).  Compiles
     to the application of the gate tree Fork (gate, Fork (name,
     site=id)) to the arg LIST (nil = Leaf, cons = Fork); [id] is the
     callsite identity carried in the tree. *)

(* Structural path from the IR root: 0 = first child (lambda body /
   application fn), 1 = second child (application arg). *)
type path = int list

exception Error of path * string  (* unbound variable, arity error, ... *)

let span_of = function
  | Var { span; _ }
  | Lam { span; _ }
  | App { span; _ }
  | Leaf_lit { span; _ }
  | Tree_lit { span; _ }
  | Prim { span; _ } -> span

let id_of = function
  | Var { id; _ } | Lam { id; _ } | App { id; _ } | Leaf_lit { id; _ }
  | Tree_lit { id; _ }
  | Prim { id; _ } -> id

let show_span { off; len } = Printf.sprintf "offset %d..%d" off (off + len)

let show_path = function
  | [] -> "root"
  | p -> String.concat "." (List.map string_of_int p)

let show_error (p, msg) =
  Printf.sprintf "IR path %s: %s" (show_path p) msg

(* Walk to the node at a structural path (None if the path is invalid
   for this term). *)
let rec at_path (t : t) (p : path) : t option =
  match (p, t) with
  | [], _ -> Some t
  | 0 :: rest, Lam { body; _ } -> at_path body rest
  | 0 :: rest, App { fn; _ } -> at_path fn rest
  | 1 :: rest, App { arg; _ } -> at_path arg rest
  | _ -> None

(* Structural path of the node with the given id (None if absent).
   Every node carries a unique id, so id lookup is the parent-pointer
   mechanism: ancestors of a node = prefixes of its path. *)
let rec find_id (t : t) (id : int) : path option =
  if id_of t = id then Some []
  else
    match t with
    | Lam { body; _ } -> (
        match find_id body id with Some p -> Some (0 :: p) | None -> None)
    | App { fn; arg; _ } -> (
        match find_id fn id with
        | Some p -> Some (0 :: p)
        | None -> (
            match find_id arg id with Some p -> Some (1 :: p) | None -> None))
    | Prim { name = _; args; _ } -> (
        let rec go i p = function
          | [] -> None
          | a :: rest -> (
              match find_id a id with
              | Some q -> Some (p @ [ i ] @ q)
              | None -> go (i + 1) (p @ [ i ]) rest)
        in
        go 0 [] args)
    | _ -> None
