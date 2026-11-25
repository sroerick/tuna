(* Tuna_interp.Accel: accelerators (borg/eval-modes.borg E2).

   An accelerator is a native implementation of a PURE stdlib tree,
   keyed by that tree's sha256.  Under the "fast1" run semantics the
   pure-step core (Flat) consults this module before a triage firing:
   if the function being applied is a registered tree (or a partial
   application of one) and the argument list is complete, the native
   answers and the rewrite never happens.  Everything else rewrites as
   v0 does.  (Urbit calls the same mechanism a "jet".)

   LAW (the chapter's):
   - The five rules stay the semantics.  A native must answer exactly
     what the reference tree answers, on EVERY input, or DECLINE
     (None) - the core then rewrites that application as v0.  Natives
     never guess; e.g. the int family declines non-canonical ints
     (the reference canonicalises them itself; we do not re-derive
     that junk law here).
   - Keyed by hash, never by name.  The registry below is pinned
     forever by its semantics name: a new accelerator, a changed native
     or a changed cost is a NEW semantics version, because replay of a
     fast row must reproduce its step count.
   - Not a prim: no grant, no journal, no effect.

   RECOGNITION.  Trees are not hash-consed and [size] is O(n), so the
   core cannot hash the function at every firing.  Instead [prepare]
   walks the program and inputs ONCE, bottom-up, computing subtree
   sizes; only a subtree whose size equals a registered size is
   hashed, and every match is replaced by ONE physical copy (structure
   unchanged).  Reduction passes subtrees by reference, so from then on
   recognising a registered function is a pointer comparison.  A
   partial application (tree-eq a) is remembered when its value is
   delivered, in a physical-identity table.  Soundness does not depend
   on where a node came from: if physical node P is the value of
   (F a), P is structurally (F a), so wherever P is later applied to b,
   P b = F a b = native [a; b].  A function the walk could not see
   (built at run time) is simply not accelerated - slower, never
   wrong.

   HIGHER-ORDER NATIVES (fast2).  list-fold, list-map and list-filter
   take a function argument the native cannot run itself, so they
   answer a PLAN: the applications the reference would perform, in the
   reference's order, which the core schedules on its own stack (each
   one an ordinary application - prims journal, partials accelerate).
   The orders, read off the compiled stdlib: list-fold f z xs computes
   every partial (f h) left to right on the way down, then applies
   them right to left to z; list-map and list-filter evaluate (g h) /
   (pred h) left to right at partial time.  tests/accel_tests.ml
   checks the orders with a logging prim host: same journal, same
   answer.

   COMPILE-TIME PARTIALS (fast2).  (rec-val #5) or (list-filter pred)
   with a closed argument is reduced by the COMPILER, so the program
   holds the partial tree and the run never sees the registered
   function applied.  [prepare] therefore also recognises partials:
   for each registered function F (its tree embedded from
   stdlib/v1/trees) and each k < arity, F applied to k marker trees is
   reduced once to a SKELETON with holes; a program node that fits a
   skeleton is then CONFIRMED by reducing F on the captured arguments
   with the reference engine and comparing structurally.  The skeleton
   is only a filter; the confirmation makes the match exact.

   COST.  An accelerated call charges [cost] steps (1 for every
   entry; a plan's applications are counted as they run).  Fast-mode step counts are fast-mode facts, recorded on the
   run row by its semantics; they are not v0 counts.  Natives with
   super-linear work (int-mul) decline past a size bound so the native
   side cannot be made unbounded by one call; the reference path is
   fuel-bounded as always. *)

open Tuna.Tree

(* a native's answer: a value, or (higher-order) the applications the
   reference would perform, for the core to schedule *)
type answer = Value of t | Plan of plan

and plan = { fn : t; xs : t list; finish : finish }
    (* apply [fn] to each of [xs] left to right, then [finish] *)

and finish =
  | Build of (t list -> t list -> t)  (* xs, results -> the value *)
  | Fold_back of t
      (* the results are partials g_i; apply them right to left
         starting from this seed *)

type entry = {
  name : string;
  hash : string;  (* sha256 hex of the registered tree *)
  size : int;  (* its node count: the cheap prefilter *)
  arity : int;
  cost : int;  (* steps charged per accelerated call *)
  impl : t list -> answer option;  (* None = decline *)
}

type registry = {
  semantics : string;
  entries : entry list;  (* preference order for a node fitting several *)
  static_partials : bool;  (* recognise compile-time partials (fast2+) *)
}

let value f args = Option.map (fun t -> Value t) (f args)

(* -- list and string helpers ----------------------------------------- *)

(* the elements of a stdlib list: fold-fn stops at the first non-fork
   (leaf or stem), so a malformed tail simply ends the list *)
let elems t =
  let rec go acc = function Fork (h, tl) -> go (h :: acc) tl | _ -> List.rev acc in
  go [] t

let list_of xs ending = List.fold_left (fun acc h -> Fork (h, acc)) ending (List.rev xs)

let rec unary n acc = if n = 0 then acc else unary (n - 1) (Stem acc)

let bool b = if b then Stem Leaf else Leaf

(* -- tree-eq, list-* ------------------------------------------------- *)

let tree_eq = function [ a; b ] -> Some (bool (a = b)) | _ -> None

(* list-append xs ys = list-fold (pair h acc) ys xs *)
let list_append = function [ xs; ys ] -> Some (list_of (elems xs) ys) | _ -> None

(* list-length = list-fold (\h acc. stem acc) leaf *)
let list_length = function [ xs ] -> Some (unary (List.length (elems xs)) Leaf) | _ -> None

(* list-reverse = list-fold (\h acc. append acc [h]) leaf *)
let list_reverse = function
  | [ xs ] -> Some (List.fold_left (fun acc h -> Fork (h, acc)) Leaf (elems xs))
  | _ -> None

(* html-escape = list-fold (\c acc. append (html-esc-char c) acc) leaf;
   html-esc-char compares c against the five char trees *)
let char_of s = match Tuna.Cstr.encode s with Fork (c, _) -> c | _ -> assert false

let escapes =
  List.map
    (fun (c, ent) -> (char_of c, elems (Tuna.Cstr.encode ent)))
    [ ("&", "&amp;"); ("<", "&lt;"); (">", "&gt;"); ("\"", "&quot;"); ("'", "&#39;") ]

let html_escape = function
  | [ s ] ->
      let out =
        List.concat_map
          (fun c ->
            match List.find_opt (fun (k, _) -> k = c) escapes with
            | Some (_, ent) -> ent
            | None -> [ c ])
          (elems s)
      in
      Some (list_of out Leaf)
  | _ -> None

(* -- ints (law 5), canonical inputs only ----------------------------- *)

(* canonical: fork(sign-bool, mag) with mag a bool list (LSB first,
   leaf-terminated) whose last bit is true; zero is fork(false, leaf) *)
let int_of = function
  | Fork (sign, mag) -> (
      let rec bits acc = function
        | Leaf -> Some (List.rev acc)
        | Fork (Leaf, tl) -> bits (0 :: acc) tl
        | Fork (Stem Leaf, tl) -> bits (1 :: acc) tl
        | _ -> None
      in
      match (sign, bits [] mag) with
      | Leaf, Some [] -> Some (false, [])
      | (Leaf | Stem Leaf), Some bs when bs <> [] && List.nth bs (List.length bs - 1) = 1 ->
          Some (sign = Stem Leaf, bs)
      | _ -> None)
  | _ -> None

let strip bs =
  let rec drop = function 0 :: r -> drop r | r -> r in
  List.rev (drop (List.rev bs))

let int_tree (neg, bs) =
  match strip bs with
  | [] -> Fork (Leaf, Leaf)
  | bs -> Fork (bool neg, list_of (List.map (fun b -> bool (b = 1)) bs) Leaf)

let mag_add a b =
  let rec go a b c =
    match (a, b) with
    | [], [] -> if c = 1 then [ 1 ] else []
    | x :: a', [] -> ((x + c) land 1) :: go a' [] ((x + c) lsr 1)
    | [], y :: b' -> ((y + c) land 1) :: go [] b' ((y + c) lsr 1)
    | x :: a', y :: b' ->
        let s = x + y + c in
        (s land 1) :: go a' b' (s lsr 1)
  in
  strip (go a b 0)

(* canonical magnitudes: longer is larger; equal length compares MSB first *)
let mag_cmp a b =
  let la = List.length a and lb = List.length b in
  if la <> lb then compare la lb else compare (List.rev a) (List.rev b)

(* a >= b *)
let mag_sub a b =
  let rec go a b w =
    match (a, b) with
    | [], _ -> []
    | x :: a', [] ->
        let d = x - w in
        if d < 0 then 1 :: go a' [] 1 else d :: go a' [] 0
    | x :: a', y :: b' ->
        let d = x - y - w in
        if d < 0 then (d + 2) :: go a' b' 1 else d :: go a' b' 0
  in
  strip (go a b 0)

let mag_mul a b =
  let rec go acc shifted = function
    | [] -> acc
    | 1 :: r -> go (mag_add acc shifted) (0 :: shifted) r
    | _ :: r -> go acc (0 :: shifted) r
  in
  strip (go [] a b)

let signed_add (sa, a) (sb, b) =
  if sa = sb then (sa, mag_add a b)
  else
    match mag_cmp a b with
    | 0 -> (false, [])
    | c when c > 0 -> (sa, mag_sub a b)
    | _ -> (sb, mag_sub b a)

let int2 f = function
  | [ x; y ] -> (
      match (int_of x, int_of y) with Some a, Some b -> f a b | _ -> None)
  | _ -> None

let int_add = int2 (fun a b -> Some (int_tree (signed_add a b)))

let int_sub =
  int2 (fun a (sb, b) -> Some (int_tree (signed_add a ((not sb) && b <> [], b))))

let mul_bound = 4_000_000

let int_mul =
  int2 (fun (sa, a) (sb, b) ->
      if List.length a * List.length b > mul_bound then None
      else Some (int_tree (sa <> sb, mag_mul a b)))

(* small-nat three-way: 0 lt, 1 eq, 2 gt *)
let int_cmp =
  int2 (fun (sa, a) (sb, b) ->
      let c =
        match (sa, sb) with
        | false, true -> 1
        | true, false -> -1
        | false, false -> mag_cmp a b
        | true, true -> mag_cmp b a
      in
      Some (unary (if c < 0 then 0 else if c = 0 then 1 else 2) Leaf))

(* -- records and higher-order lists (fast2) ---------------------------- *)

let first = function Fork (l, _) -> l | _ -> Leaf
let second = function Fork (_, r) -> r | _ -> Leaf

(* rec-get key rec: the LEFTMOST entry whose first is key (the right
   fold keeps the nearer match) -> some (first (second kv)), else none *)
let rec_find key r = List.find_opt (fun kv -> first kv = key) (elems r)

let rec_get = function
  | [ key; r ] ->
      Some (match rec_find key r with Some kv -> Stem (first (second kv)) | None -> Leaf)
  | _ -> None

let rec_val = function
  | [ key; r ] -> Some (match rec_find key r with Some kv -> first (second kv) | None -> Leaf)
  | _ -> None

let rec_has = function [ key; r ] -> Some (bool (rec_find key r <> None)) | _ -> None

let list_fold = function
  | [ f; z; xs ] -> Some (Plan { fn = f; xs = elems xs; finish = Fold_back z })
  | _ -> None

let list_map = function
  | [ g; xs ] -> Some (Plan { fn = g; xs = elems xs; finish = Build (fun _ rs -> list_of rs Leaf) })
  | _ -> None

(* keep x when (pred x) is a stem; a leaf or a fork drops it *)
let list_filter = function
  | [ pred; xs ] ->
      Some
        (Plan
           { fn = pred;
             xs = elems xs;
             finish =
               Build
                 (fun xs rs ->
                   list_of
                     (List.filter_map
                        (fun (x, r) -> match r with Stem _ -> Some x | _ -> None)
                        (List.combine xs rs))
                     Leaf) })
  | _ -> None

(* -- the registry ------------------------------------------------------ *)

(* fast1: stdlib v1.3 (stdlib/v1/manifest).  PINNED: changing any row
   is a new semantics version. *)
let fast1_entries =
  let e name hash size arity impl = { name; hash; size; arity; cost = 1; impl = value impl } in
  [ e "tree-eq" "fd549d45e1e2e481488d0c0ff4ec9c4de263fca5a511e623bbbba0bd6291de03" 406 2 tree_eq;
    e "list-append" "500eaee618df21a52bd4355ba2d60cf9639d108dd45a39bcb17841abb26a26e2" 307 2 list_append;
    e "list-length" "b0f08c694e75ab698415377cf023c14be878e92b82bf87b3dfb30c3f7e6095e8" 298 1 list_length;
    e "list-reverse" "b524cf23e4300b731fb909f00786f2f607989fb988ba309f6193857d79097ef6" 620 1 list_reverse;
    e "html-escape" "edb068dda9294c4bd3fe3da2ae4fbd0abf24755a52ab9bce7b65f80f01e17b50" 3321 1 html_escape;
    e "int-add" "7f33ee5c4b9c43818a74e359ebce0cbe88343551b3cffb75122498b42077eab3" 22259 2 int_add;
    e "int-sub" "bb2e50fae5b33069d24d87072baf07ddf2d553f761781cda8f2049f756db12af" 22943 2 int_sub;
    e "int-mul" "fb3f462a1b191e5065ebd21018ee85cd7b8116f9693f282fe9527dd13411f5fa" 6213 2 int_mul;
    e "int-cmp" "de0a52cae88f868eea5a6fb128068dd88cd2a3a69898638c0222176ad3be8a40" 5613 2 int_cmp ]

let fast1 = { semantics = "fast1"; entries = fast1_entries; static_partials = false }

(* fast2: fast1's rows plus records and the higher-order list family,
   and compile-time partial recognition.  PINNED like fast1. *)
let fast2 =
  let e name hash size arity impl = { name; hash; size; arity; cost = 1; impl } in
  { semantics = "fast2";
    static_partials = true;
    entries =
      [ e "rec-val" "1132bbd758d195165b345c89dc31b242e20902243ebda70ba7789c25762c034d" 888 2 (value rec_val);
        e "rec-has" "e20bc94f9ebc66df748e4c91aa3505b1fa6afba238692e686ca0f91021d7a52e" 887 2 (value rec_has);
        e "rec-get" "25ae72255db18f143b1b238656b6249e2f1473b97ccefa32d6479a00ebdf760b" 867 2 (value rec_get);
        e "list-filter" "e51fbd4cdae60fa5d2d276001c921beeaae7e6c11a97db87dba927ebc7f7a615" 406 2 list_filter;
        e "list-map" "b0bd1e1934d18dba698eeb2b9e42aca5e68e8c33146666958143dbb0488371c0" 340 2 list_map;
        e "list-fold" "3a4225df7bd383bc2f1a6b3d0fa0a0bb84632f90e1415825a2be683503fabc4f" 326 3 list_fold ]
      @ fast1_entries }

let registry_of_semantics = function
  | "fast1" -> Some fast1
  | "fast2" -> Some fast2
  | _ -> None

(* -- per-run state --------------------------------------------------- *)

module Phys = Hashtbl.Make (struct
  type nonrec t = t

  let equal = ( == )
  let hash = Hashtbl.hash
end)

(* a partial-application skeleton: F applied to k markers, reduced, with
   the markers replaced by holes *)
type skel = SLeaf | SStem of skel | SFork of skel * skel | Hole of int

type state = {
  entries : entry list;
  skels : (entry * int * skel) list;  (* entry, k args, skeleton *)
  trees : (entry * t) list;  (* registered trees available for confirming *)
  mutable roots : (t * entry) list;  (* the one physical copy per found entry *)
  partials : (entry * t list) Phys.t;
  mutable calls : int;
  mutable declines : int;
}

(* distinctive marker trees: deterministic, size 40ish, unlikely to
   occur inside a reduced partial by accident (and harmless if they do:
   every skeleton match is confirmed by reduction) *)
let markers =
  let st = Random.State.make [| 0x7a11; 0xacce1 |] in
  let rec gen d =
    if d = 0 then Leaf
    else
      match Random.State.int st 3 with
      | 0 -> Stem (gen (d - 1))
      | _ -> Fork (gen (d - 1), gen (d - 1))
  in
  Array.init 3 (fun _ -> Fork (Stem (Fork (gen 4, gen 4)), Stem (gen 5)))

let skeleton_of p0 k =
  let found = Array.make k false in
  let rec go t =
    match Array.find_index (fun m -> m = t) (Array.sub markers 0 k) with
    | Some i -> found.(i) <- true; Hole i
    | None -> (
        match t with
        | Leaf -> SLeaf
        | Stem a -> SStem (go a)
        | Fork (a, b) -> SFork (go a, go b))
  in
  let s = go p0 in
  if Array.for_all Fun.id found then Some s else None

let tree_of_hash =
  let tbl =
    lazy
      (let h = Hashtbl.create 128 in
       List.iter (fun (hash, ternary) -> Hashtbl.replace h hash ternary) Accel_trees.all;
       h)
  in
  fun hash ->
    match Hashtbl.find_opt (Lazy.force tbl) hash with
    | None -> None
    | Some ternary -> (
        match Tuna.Canon.of_string ternary with Ok t -> Some t | Error _ -> None)

(* [reduce f args]: the REFERENCE engine (v0, no accelerators, no prim
   host), bounded; None when it does not finish normally *)
(* trees and skeletons are pure functions of the (pinned) registry:
   computed once per process, not per run *)
let derived : (string, (entry * t) list * (entry * int * skel) list) Hashtbl.t =
  Hashtbl.create 4

let start ?reduce (r : registry) =
  let trees, skels =
    match (r.static_partials, reduce) with
    | true, Some _ when Hashtbl.mem derived r.semantics -> Hashtbl.find derived r.semantics
    | true, Some reduce ->
        let trees =
          List.filter_map
            (fun e -> Option.map (fun t -> (e, t)) (tree_of_hash e.hash))
            r.entries
        in
        let skels =
          List.concat_map
            (fun (e, f) ->
              List.filter_map
                (fun k ->
                  match reduce f (Array.to_list (Array.sub markers 0 k)) with
                  | Some p0 -> Option.map (fun s -> (e, k, s)) (skeleton_of p0 k)
                  | None -> None)
                (List.init (e.arity - 1) (fun i -> i + 1)))
            trees
        in
        Hashtbl.replace derived r.semantics (trees, skels);
        (trees, skels)
    | _ -> ([], [])
  in
  { entries = r.entries; skels; trees; roots = []; partials = Phys.create 1024;
    calls = 0; declines = 0 }

(* fit [node] to a skeleton, capturing the holes; structural equality
   for a repeated hole *)
let fit sk node k =
  let caps = Array.make k None in
  let rec m sk node =
    match (sk, node) with
    | Hole i, _ -> (
        match caps.(i) with
        | None -> caps.(i) <- Some node; true
        | Some x -> x == node || x = node)
    | SLeaf, Leaf -> true
    | SStem s, Stem n -> m s n
    | SFork (a, b), Fork (x, y) -> m a x && m b y
    | _ -> false
  in
  if m sk node then Some (Array.to_list (Array.map Option.get caps)) else None

(* the once-per-run walk: replace every registered subtree with one
   physical copy (same structure, so the program's hash is unchanged),
   and (fast2) remember every confirmed compile-time partial *)
let prepare ?reduce st t =
  let confirm node =
    match (node, reduce) with
    | Fork _, Some reduce when st.skels <> [] ->
        let rec try_ = function
          | [] -> ()
          | (e, k, sk) :: rest -> (
              match fit sk node k with
              | None -> try_ rest
              | Some args -> (
                  let f = List.assq e st.trees in
                  match reduce f args with
                  | Some p when p = node -> Phys.replace st.partials node (e, args)
                  | _ -> try_ rest))
        in
        try_ st.skels
    | _ -> ()
  in
  let sizes = List.map (fun e -> e.size) st.entries in
  let check node n =
    if not (List.mem n sizes) then node
    else
      let cands = List.filter (fun e -> e.size = n) st.entries in
      let h = Tuna.Hash.hex_of_tree node in
      match List.find_opt (fun e -> e.hash = h) cands with
      | None -> node
      | Some e -> (
          match List.find_opt (fun (_, e') -> e' == e) st.roots with
          | Some (root, _) -> root
          | None ->
              st.roots <- (node, e) :: st.roots;
              node)
  in
  let rec go t =
    match t with
    | Leaf -> (t, 1)
    | Stem a ->
        let a', na = go a in
        let n = na + 1 in
        (check (if a' == a then t else Stem a') n, n)
    | Fork (a, b) ->
        let a', na = go a in
        let b', nb = go b in
        let n = na + nb + 1 in
        let node = check (if a' == a && b' == b then t else Fork (a', b')) n in
        if not (List.exists (fun (r, _) -> r == node) st.roots) then confirm node;
        (node, n)
  in
  fst (go t)

(* is [f] a registered tree or a remembered partial of one? *)
let lookup st f =
  match List.assq_opt f st.roots with
  | Some e -> Some (e, [])
  | None -> if Phys.length st.partials = 0 then None else Phys.find_opt st.partials f

let remember st value partial = Phys.replace st.partials value partial
