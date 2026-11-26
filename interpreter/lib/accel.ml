(* Tuna_interp.Accel: accelerators (borg/eval-modes.borg E2, E3).

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
   - Gate: `dune build --root . @tests/accel-full` (the full int vector
     set) before any change here; `runtest` runs a smoke subset.

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
   fuel-bounded as always. 
   FAST3 (E3): fast2's rows unchanged, plus Barry Jay's type checker
   (typing.borg; trees in reference/jay-typing-rules/natives, embedded
   as Jay_trees).  New mechanisms, used only by fast3's registry:
   FAMILIES - infer[O], tp[O] and row[m,D,rest] are programs with
   their parameters substituted literally, recognised by a template
   derived from the program at Leaf and at K per parameter (exact, no
   confirmation); CONTINUATION PLANS (Then / Seq answers, KThen frame)
   - a native applies a tree or runs its own call, then continues;
   WRAPPER PARTIALS - a partial whose value is a stem is followed
   through its constructor application; PARTIAL CLOSED FORMS - Jay's
   equal answers at one argument with equal1 p (his equal_eval).  The
   checker natives obey E3's NARROWED LAW: the reference's answer
   whenever the reference terminates (they skip work it discards, so
   they may answer where it diverges inside that work), and they
   decline when a parameter carries a prim. *)

open Tuna.Tree

(* a native's answer: a value, or (higher-order) the applications the
   reference would perform, for the core to schedule *)
type answer =
  | Value of t
  | Plan of plan
  | Then of t * t list * (t -> answer)
      (* fast3 continuation plan: apply the tree to these arguments
         (curried, at least one) on the core's stack, then continue
         with its value *)
  | Seq of answer * (t -> answer)
      (* fast3: run a native's own answer for a call it makes itself
         (no tree application, no partial), then continue *)

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
  partial : (t list -> t option) option;
      (* fast3: the exact value of a PARTIAL application (fewer args
         than arity), proved closed form; None everywhere else *)
}

(* a FAMILY (fast3, borg/eval-modes.borg E3): a program built from
   parameters by literal substitution (Jay's infer O, tp O and
   row m D rest), registered as the program with every parameter Leaf
   and, per parameter, with that one set to K.  The sites where a
   variant differs from the base are that parameter's; a node fits
   when it is the template with one tree at all of a parameter's
   sites.  Recognition is exact: no confirmation is needed. *)
type ctx = { member : string -> t array -> t }
    (* the member of a named family at these parameters, as the one
       physical tree the run uses for it *)

type family = {
  fname : string;
  base : t;
  vary : t array;  (* one program per parameter *)
  farity : int;
  fimpl : ctx -> t array -> t list -> answer option;
}

type registry = {
  semantics : string;
  entries : entry list;  (* preference order for a node fitting several *)
  static_partials : bool;  (* recognise compile-time partials (fast2+) *)
  families : family list;
  wrapper_partials : bool;
      (* follow a partial through a wrapper application (a stem or leaf
         applied: a constructor, not a firing); fast1/fast2 do not *)
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

(* -- Barry Jay's checker (fast3, borg/eval-modes.borg E3) -------------

   Every native below follows a lemma or a clause of his Rocq
   development (reference/jay-typing-rules, typing.v / rows.v /
   tree.v), with the names his code uses.  Values: Lf = Leaf,
   Sm = Stem, Fk = Fork. *)

let jk = Stem Leaf (* 'K *)
let ji = Fork (Stem (Stem Leaf), Stem Leaf) (* 'I *)
let jki = Fork (Leaf, ji) (* 'KI = 'K1 'I *)
let jbt b = if b then jk else jki
let jfst = Fork (Stem ji, Fork (Leaf, Stem Leaf)) (* guard_of = 'fst *)
let jsnd = Fork (Stem ji, Fork (Leaf, Fork (Leaf, ji))) (* elim_of = 'snd *)
let jlabel n = Fork (Stem (Stem Leaf), Fork (Leaf, n))

let label_of = function Fork (Stem (Stem Leaf), Fork (Leaf, n)) -> Some n | _ -> None

let tag_of = function
  | Fork (Stem (Fork (Leaf, f)), Fork (Stem (Stem Leaf), Fork (Leaf, n))) -> Some (f, n)
  | _ -> None

let label_pair u v = match (u, v) with Stem (Stem Leaf), Fork (Leaf, _) -> true | _ -> false

let rec concreteb = function
  | Leaf -> true
  | Stem u -> concreteb u
  | Fork (u, v) -> (not (label_pair u v)) && concreteb u && concreteb v

let reservedb t = tag_of t <> None || label_of t <> None

let jtri a b c = Fork (Fork (a, b), c)
let jk1 x = Fork (Leaf, x)
let js2 u v = Fork (Stem u, v)
let jis_leaf = jtri jk (jk1 jki) (jk1 (jk1 jki))

(* tree.v equal1: equal_eval proves equal @ p ==> equal1 p *)
let rec equal1 = function
  | Leaf -> jis_leaf
  | Stem p1 -> jtri jki (equal1 p1) (jk1 (jk1 jki))
  | Fork (p1, p2) ->
      jtri jki (jk1 jki) (js2 (js2 (equal1 p1) (jk1 (equal1 p2))) (jk1 (jk1 jki)))

(* equal_bt (equal1_eq / equal1_neq), is_concrete_spec,
   get_tagged_spec, get_label_spec *)
let j_equal = function [ p; q ] -> Some (jbt (p = q)) | _ -> None
let j_equal_partial = function [ p ] -> Some (equal1 p) | _ -> None
let j_is_concrete = function [ n ] -> Some (jbt (concreteb n)) | _ -> None

let j_get_tagged = function
  | [ t ] -> Some (match tag_of t with Some (f, n) -> Stem (Fork (f, n)) | None -> Leaf)
  | _ -> None

let j_get_label = function
  | [ t ] -> Some (match label_of t with Some n -> Stem n | None -> Leaf)
  | _ -> None

(* onStem1: continue on Sm v, else Lf *)
let on_stem r k = match r with Stem v -> k v | _ -> Value Leaf
let pass r = Value r

(* row m D rest @ n (rows.v row_head): K1 (Sm D) on a hit; on a miss
   the value of rest @ n.  The tree evaluates rest @ n on a hit too and
   discards it; skipping that is the E3 narrowed law. *)
let j_row _ps_ctx ps args =
  match (ps, args) with
  | [| m; d; rest |], [ n ] ->
      Some (if m = n then Value (Fork (Leaf, Stem d)) else Then (rest, [ n ], pass))
  | _ -> None

(* tp O @ F @ U, clause by clause (typing.v tp_body / ord_cl / abs_cl /
   stem_cl / fk_cl / s_cl / tri_cl; TA and tp_inv) *)
let rec j_tp ctx ps args =
  match (ps, args) with
  | [| o |], [ f; u ] ->
      let tp = ctx.member "tp" [| o |] in
      let ap g x k = Seq (tp_call ctx o g x, k) in
      Some
        (match label_of f with
         | Some n ->
             (* abs_cl: let Sm D = O @ n @ tp in elim_of D @ n @ tp @ U *)
             Then (o, [ n; tp ], fun r -> on_stem r (fun d -> Then (jsnd, [ d; n; tp; u ], pass)))
         | None -> (
             match f with
             | Leaf -> Value (Stem (Stem u)) (* leaf_cl *)
             | Stem g -> Value (if label_pair g u then Leaf else Stem (Fork (g, u))) (* stem_cl *)
             | Fork (x, y) -> (
                 if reservedb x then Value Leaf
                 else
                   match x with
                   | Leaf -> Value (Stem y) (* K1 Y *)
                   | Stem g ->
                       (* s_cl: V = G # U, W = Y # U, V # W *)
                       ap g u (fun r ->
                           on_stem r (fun v -> ap y u (fun r -> on_stem r (fun w -> ap v w pass))))
                   | Fork (f0, f1) -> (
                       (* tri_cl, with F2 = y *)
                       if label_of u <> None then Value Leaf
                       else
                         match u with
                         | Leaf -> Value (Stem f0)
                         | Stem xx -> ap f1 xx pass
                         | Fork (xx, yy) -> ap y xx (fun r -> on_stem r (fun g -> ap g yy pass))))))
  | _ -> None

(* the native's own recursive call: the value of tp O @ g @ x, with no
   partial application (E3 law (d): its partial work is discarded) *)
and tp_call ctx o g x =
  match j_tp ctx [| o |] [ g; x ] with Some a -> a | None -> assert false

(* infer O @ t (typing.v infer_body / tag_cl / plain_cl; INF and
   infer_inv) *)
let rec j_infer ctx ps args =
  match (ps, args) with
  | [| o |], [ t ] ->
      let tp = ctx.member "tp" [| o |] in
      let inf u k = Seq (infer_call ctx o u, k) in
      Some
        (match t with
         | Leaf -> Value (Stem Leaf)
         | Stem u -> inf u (fun r -> on_stem r (fun uu -> Value (Stem (Stem uu))))
         | Fork (u1, u2) -> (
             match tag_of t with
             | Some (f, n) ->
                 (* tag_cl: concrete name, carrier typed, guard admits it *)
                 if not (concreteb n) then Value Leaf
                 else
                   inf f
                     ( fun r ->
                         on_stem r (fun ff ->
                             Then
                               ( o, [ n; tp ],
                                 fun r ->
                                   on_stem r (fun d ->
                                       Then
                                         ( jfst, [ d; ff ],
                                           fun g -> Value (if g = jk then Stem (jlabel n) else Leaf) ))
                               )) )
             | None ->
                 (* plain_cl: U1, U2, then (Sm U1) # U2 *)
                 inf u1 (fun r ->
                     on_stem r (fun a ->
                         inf u2 (fun r -> on_stem r (fun b -> Seq (tp_call ctx o (Stem a) b, pass)))))))
  | _ -> None

and infer_call ctx o u = match j_infer ctx [| o |] [ u ] with Some a -> a | None -> assert false

let jay_tree name =
  match List.assoc_opt name Jay_trees.all with
  | None -> invalid_arg ("Accel: no Jay tree " ^ name)
  | Some ternary -> (
      match Tuna.Canon.of_string ternary with Ok t -> t | Error _ -> invalid_arg name)

let jay_entries =
  lazy
    (List.map
       (fun (name, arity, impl) ->
         let t = jay_tree name in
         { name = "jay-" ^ name; hash = Tuna.Hash.hex_of_tree t; size = size t; arity; cost = 1;
           impl = value impl;
           partial = (if name = "equal" then Some j_equal_partial else None) })
       [ ("equal", 2, j_equal); ("is_concrete", 1, j_is_concrete); ("get_tagged", 1, j_get_tagged);
         ("get_label", 1, j_get_label) ])

let jay_families =
  lazy
    (List.map
       (fun (name, k, arity, fimpl) ->
         { fname = name; base = jay_tree (name ^ ".0");
           vary = Array.init k (fun i -> jay_tree (Printf.sprintf "%s.%d" name (i + 1)));
           farity = arity; fimpl })
       [ ("infer", 1, 1, j_infer); ("tp", 1, 2, j_tp); ("row", 3, 1, j_row) ])

(* -- the registry ------------------------------------------------------ *)

(* fast1: stdlib v1.3 (stdlib/v1/manifest).  PINNED: changing any row
   is a new semantics version. *)
let fast1_entries =
  let e name hash size arity impl = { name; hash; size; arity; cost = 1; impl = value impl; partial = None } in
  [ e "tree-eq" "fd549d45e1e2e481488d0c0ff4ec9c4de263fca5a511e623bbbba0bd6291de03" 406 2 tree_eq;
    e "list-append" "500eaee618df21a52bd4355ba2d60cf9639d108dd45a39bcb17841abb26a26e2" 307 2 list_append;
    e "list-length" "b0f08c694e75ab698415377cf023c14be878e92b82bf87b3dfb30c3f7e6095e8" 298 1 list_length;
    e "list-reverse" "b524cf23e4300b731fb909f00786f2f607989fb988ba309f6193857d79097ef6" 620 1 list_reverse;
    e "html-escape" "edb068dda9294c4bd3fe3da2ae4fbd0abf24755a52ab9bce7b65f80f01e17b50" 3321 1 html_escape;
    e "int-add" "7f33ee5c4b9c43818a74e359ebce0cbe88343551b3cffb75122498b42077eab3" 22259 2 int_add;
    e "int-sub" "bb2e50fae5b33069d24d87072baf07ddf2d553f761781cda8f2049f756db12af" 22943 2 int_sub;
    e "int-mul" "fb3f462a1b191e5065ebd21018ee85cd7b8116f9693f282fe9527dd13411f5fa" 6213 2 int_mul;
    e "int-cmp" "de0a52cae88f868eea5a6fb128068dd88cd2a3a69898638c0222176ad3be8a40" 5613 2 int_cmp ]

let fast1 = { semantics = "fast1"; entries = fast1_entries; static_partials = false; families = [];
  wrapper_partials = false }

(* fast2: fast1's rows plus records and the higher-order list family,
   and compile-time partial recognition.  PINNED like fast1. *)
let fast2 =
  let e name hash size arity impl = { name; hash; size; arity; cost = 1; impl; partial = None } in
  { semantics = "fast2";
    static_partials = true;
    families = [];
    wrapper_partials = false;
    entries =
      [ e "rec-val" "1132bbd758d195165b345c89dc31b242e20902243ebda70ba7789c25762c034d" 888 2 (value rec_val);
        e "rec-has" "e20bc94f9ebc66df748e4c91aa3505b1fa6afba238692e686ca0f91021d7a52e" 887 2 (value rec_has);
        e "rec-get" "25ae72255db18f143b1b238656b6249e2f1473b97ccefa32d6479a00ebdf760b" 867 2 (value rec_get);
        e "list-filter" "e51fbd4cdae60fa5d2d276001c921beeaae7e6c11a97db87dba927ebc7f7a615" 406 2 list_filter;
        e "list-map" "b0bd1e1934d18dba698eeb2b9e42aca5e68e8c33146666958143dbb0488371c0" 340 2 list_map;
        e "list-fold" "3a4225df7bd383bc2f1a6b3d0fa0a0bb84632f90e1415825a2be683503fabc4f" 326 3 list_fold ]
      @ fast1_entries }

(* fast3: fast2's rows unchanged, plus Jay's checker (E3): helper
   natives by hash, infer / tp / row as families, continuation plans,
   wrapper partials, and the narrowed law of E3 (d). *)
let fast3 =
  lazy
    { semantics = "fast3";
      static_partials = true;
      families = Lazy.force jay_families;
      wrapper_partials = true;
      entries = fast2.entries @ Lazy.force jay_entries }

let registry_of_semantics = function
  | "fast1" -> Some fast1
  | "fast2" -> Some fast2
  | "fast3" -> Some (Lazy.force fast3)
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
  fskels : (family * skel) list;
  wrappers : bool;
  mutable froots : (t * string * t array * entry) list;
      (* family members found or built: tree, family, parameters *)
  mutable prim_free : (t * bool) list;
      (* memo per family parameter (physical): it has no prim call *)
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

(* the family's template.  A parameter site: the base has Leaf, exactly
   one variant has K there and the others Leaf.  Elsewhere all programs
   agree.  None when they are not of that form, or a parameter has no
   site. *)
let family_skel fam =
  let k = Array.length fam.vary in
  let found = Array.make k false in
  let site b vs =
    if b <> Leaf then None
    else
      let hit = ref None and ok = ref true in
      Array.iteri
        (fun j v ->
          match v with
          | Leaf -> ()
          | Stem Leaf when !hit = None -> hit := Some j
          | _ -> ok := false)
        vs;
      if !ok then !hit else None
  in
  let rec d b vs =
    match site b vs with
    | Some i -> found.(i) <- true; Some (Hole i)
    | None -> (
        match b with
        | Leaf -> if Array.for_all (fun v -> v = Leaf) vs then Some SLeaf else None
        | Stem x -> (
            match Array.map (function Stem y -> Some y | _ -> None) vs with
            | ys when Array.for_all Option.is_some ys ->
                Option.map (fun s -> SStem s) (d x (Array.map Option.get ys))
            | _ -> None)
        | Fork (x, y) -> (
            match Array.map (function Fork (u, v) -> Some (u, v) | _ -> None) vs with
            | ps when Array.for_all Option.is_some ps -> (
                let ps = Array.map Option.get ps in
                match d x (Array.map fst ps) with
                | None -> None
                | Some l -> Option.map (fun r -> SFork (l, r)) (d y (Array.map snd ps)))
            | _ -> None))
  in
  match d fam.base fam.vary with
  | Some sk when Array.for_all Fun.id found -> Some sk
  | _ -> None

(* [reduce f args]: the REFERENCE engine (v0, no accelerators, no prim
   host), bounded; None when it does not finish normally *)
(* trees and skeletons are pure functions of the (pinned) registry:
   computed once per process, not per run *)
let derived : (string, (entry * t) list * (entry * int * skel) list) Hashtbl.t =
  Hashtbl.create 4

let family_skels : (string, (family * skel) list) Hashtbl.t = Hashtbl.create 4

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
  let fskels =
    match Hashtbl.find_opt family_skels r.semantics with
    | Some f -> f
    | None ->
        let f =
          List.map
            (fun fam ->
              match family_skel fam with
              | Some sk -> (fam, sk)
              | None -> invalid_arg ("Accel: family template malformed: " ^ fam.fname))
            r.families
        in
        Hashtbl.replace family_skels r.semantics f;
        f
  in
  { entries = r.entries; skels; trees; roots = []; partials = Phys.create 1024;
    calls = 0; declines = 0; fskels; froots = []; wrappers = r.wrapper_partials;
    prim_free = [] }

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

(* no prim call anywhere in [t]: a plain walk (the start walk is
   already linear in the program), memoised per parameter.  A
   structural-hash memo per node was tried first and lost: similar
   subtrees of a large ontology collide in its buckets. *)
let rec no_prim t =
  Tuna.Cprim.shape t = None
  && match t with Leaf -> true | Stem a -> no_prim a | Fork (a, b) -> no_prim a && no_prim b

let prim_free st t =
  match List.assq_opt t st.prim_free with
  | Some b -> b
  | None ->
      let b = no_prim t in
      st.prim_free <- (t, b) :: st.prim_free;
      b

let rec instantiate sk ps =
  match sk with
  | SLeaf -> Leaf
  | SStem s -> Stem (instantiate s ps)
  | SFork (a, b) -> Fork (instantiate a ps, instantiate b ps)
  | Hole i -> ps.(i)

(* register [node] as the member of [fam] at [ps]; its native declines
   when a parameter carries a prim (E3 law (d): no effect is skipped) *)
let rec add_member st fam ps node =
  let pure = lazy (Array.for_all (prim_free st) ps) in
  let impl args = if Lazy.force pure then fam.fimpl (ctx st) ps args else None in
  st.froots <-
    (node, fam.fname, ps,
     { name = fam.fname; hash = ""; size = 0; arity = fam.farity; cost = 1; impl; partial = None })
    :: st.froots

and ctx st =
  { member =
      (fun name ps ->
        let same_ps a b = Array.length a = Array.length b && Array.for_all2 ( == ) a b in
        match List.find_opt (fun (_, n, p, _) -> n = name && same_ps p ps) st.froots with
        | Some (r, _, _, _) -> r
        | None -> (
            match List.find_opt (fun (f, _) -> f.fname = name) st.fskels with
            | None -> invalid_arg ("Accel.member: no family " ^ name)
            | Some (fam, sk) ->
                let node = instantiate sk ps in
                add_member st fam ps node;
                node)) }

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
        List.iter
          (fun (fam, sk) ->
            if not (List.exists (fun (r, _, _, _) -> r == node) st.froots) then
              match fit sk node (Array.length fam.vary) with
              | Some ps -> add_member st fam (Array.of_list ps) node
              | None -> ())
          st.fskels;
        (node, n)
  in
  fst (go t)

(* is [f] a registered tree or a remembered partial of one? *)
(* structural equality that short-circuits on shared subtrees: a family
   member rebuilt by its own recursion (Z unfolding) shares all but its
   top few nodes with the member the walk found *)
let rec same a b =
  a == b
  || match (a, b) with
     | Leaf, Leaf -> true
     | Stem x, Stem y -> same x y
     | Fork (x, y), Fork (u, v) -> same x u && same y v
     | _ -> false

let lookup st f =
  match List.assq_opt f st.roots with
  | Some e -> Some (e, [])
  | None -> (
      match if Phys.length st.partials = 0 then None else Phys.find_opt st.partials f with
      | Some p -> Some p
      | None -> (
          match st.froots with
          | [] -> None
          | froots -> (
              match List.find_opt (fun (r, _, _, _) -> r == f) froots with
              | Some (_, _, _, e) -> Some (e, [])
              | None -> (
                  match List.find_opt (fun (r, _, _, _) -> same r f) froots with
                  | Some (_, _, _, e) -> Some (e, [])
                  | None -> None))))

let remember st value partial = Phys.replace st.partials value partial
