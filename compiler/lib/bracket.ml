(* Tuna compiler: surface IR -> tree by bracket abstraction with eta.

   Normative source: reference/tree-calculus/implementation/ocaml/
   lib/tree_builder.ml (star_abstraction) — ported with the same three
   combinators as trees built by application of the leaf:

     k u = Node*Node*u          reduces to Fork (Leaf, u)
     s u v = Node*(Node*u)*v    reduces to Fork (Stem u, v)
     i   = s (Node*Node) Node   reduces to Fork (Stem (Stem Leaf), Leaf)

   The intermediate combinator language (cterm) mirrors upstream's
   Ref/Node/App exactly (plus an id tag on every node for provenance
   and a CLam shell before elimination). Compilation IS reduction: the
   final pass evaluates the closed combinator term to its normal-form
   tree (upstream's to_tree), under a compile-time fuel/size budget.
   Exceeding the budget is a compile error, never a hang.

   Provenance: every tree node in the compiled artifact carries the id
   of the IR node responsible for it (see provenance.ml); bracket
   abstraction synthesizes nodes tagged with the id of the enclosing
   lambda (they have no source occurrence of their own). *)

type cterm =
  | CVar of int * string  (* id, name — eliminated by abstraction *)
  | CLeaf of int  (* id *)
  | CApp of int * cterm * cterm  (* id, fn, arg *)
  | CLam of int * string * cterm  (* id, param, body — pre-elimination *)

let rec occurs x (m : cterm) : bool =
  match m with
  | CVar (_, y) -> y = x
  | CLeaf _ -> false
  | CApp (_, m1, m2) -> occurs x m1 || occurs x m2
  | CLam (_, _, b) -> occurs x b

(* Combinators as pending applications, verbatim shapes. [sid] tags
   synthesized nodes with the id of the lambda being eliminated. *)
let k sid u = CApp (sid, CApp (sid, CLeaf sid, CLeaf sid), u)

let s sid u v = CApp (sid, CApp (sid, CLeaf sid, CApp (sid, CLeaf sid, u)), v)

let i_comb sid = s sid (CApp (sid, CLeaf sid, CLeaf sid)) (CLeaf sid)

(* η-reduction, EXCEPT when the function side is a prim gate tree:
   eliminating a prim call's application into data would strand the
   prim where it never fires (the gate must stay in function position
   of a run-time application). *)
let rec is_stem_chain (n : int) (m : cterm) : bool =
  match n with
  | 0 -> (match m with CLeaf _ -> true | _ -> false)
  | _ -> (
      match m with CApp (_, CLeaf _, r) -> is_stem_chain (n - 1) r | _ -> false)

let is_gate_lit (m : cterm) : bool =
  (* lit-expansion of Fork (gate, _): (Leaf·gate-chain)·anything *)
  match m with
  | CApp (_, CApp (_, CLeaf _, g), _) when is_stem_chain 4 g -> true
  | _ -> false

(* Verbatim port of upstream star_abstraction (with eta). *)
let rec abstract sid x (m : cterm) : cterm =
  match occurs x m with
  | false -> k sid m
  | true -> (
      match m with
      | CVar (_, y) when y = x -> i_comb sid
      | CApp (_, m1, CVar (_, y)) when
          y = x && not (occurs x m1) && not (is_gate_lit m1) ->
          (* η-reduction *)
          m1
      | CApp (_, m1, m2) -> s sid (abstract sid x m1) (abstract sid x m2)
      | _ -> assert false
      (* occurs true implies CVar x or CApp, both covered above *))

(* Expand a literal tree structurally: Stem a = Leaf applied to a,
   Fork (a,b) = (Leaf applied to a) applied to b. Every node tagged
   with the literal's IR id. *)
let rec lit id = function
  | Tuna.Tree.Leaf -> CLeaf id
  | Tuna.Tree.Stem a -> CApp (id, CLeaf id, lit id a)
  | Tuna.Tree.Fork (a, b) -> CApp (id, CApp (id, CLeaf id, lit id a), lit id b)

(* IR -> cterm, then eliminate every lambda innermost-first
   (λx.λy.b = [x]([y]b)). *)
let rec elim (t : cterm) : cterm =
  match t with
  | CLam (sid, x, body) -> abstract sid x (elim body)
  | CApp (id, f, a) -> CApp (id, elim f, elim a)
  | CLeaf _ | CVar _ -> t

let rec of_ir (ir : Ir.t) : cterm =
  match ir with
  | Var { id; name; _ } -> CVar (id, name)
  | Lam { id; param; body; _ } -> CLam (id, param, of_ir body)
  | App { id; fn; arg; _ } -> CApp (id, of_ir fn, of_ir arg)
  | Leaf_lit { id; _ } -> CLeaf id
  | Tree_lit { id; tree; _ } -> lit id tree
  | Prim { id; name; args; _ } ->
      (* The prim call = the application of the gate tree (carrying
         name + callsite id) to the arg LIST: nil = Leaf, cons =
         (Leaf·a)·rest.  The gate must remain in function position, so
         this is emitted as a plain application — never η'd away. *)
      let rec list_ct = function
        | [] -> CLeaf id
        | a :: rest -> CApp (id, CApp (id, CLeaf id, of_ir a), list_ct rest)
      in
      CApp (id, lit id (Tuna.Cprim.call_tree ~name ~site:id), list_ct args)

(* ---------- compile-time evaluation (tagged upstream to_tree) ---------- *)

type tagged =
  | TLeaf of int
  | TStem of int * tagged
  | TFork of int * tagged * tagged

type budget = {
  mutable fuel : int
      (* coarse ceiling (triage firings); the clock below is the fine one *)
; mutable steps : int
; size_cap : int
; deadline : float  (* absolute Unix time; infinity = untimed (pure/tests) *)
}

exception Compile_failed of string

let rec size_of = function
  | TLeaf _ -> 1
  | TStem (_, a) -> 1 + size_of a
  | TFork (_, a, b) -> 1 + size_of a + size_of b

let check_size b t =
  if size_of t > b.size_cap then
    raise (Compile_failed "compile-time evaluation exceeded the size cap")

let fire b =
  if b.fuel = 0 then
    raise (Compile_failed "compile-time reduction exceeded the fuel budget");
  b.fuel <- b.fuel - 1;
  b.steps <- b.steps + 1;
  (* wall-clock poll every 4096 firings (run-boundary policy, mirrors
     the interpreter's deadline poll): request-boundary compiles get
     TUNA_COMPILE_MAX_SECONDS, pure/test compiles pass infinity and
     never touch the clock *)
  if b.fuel land 4095 = 0
     && b.deadline <> Float.infinity
     && Unix.gettimeofday () > b.deadline
  then
    raise
      (Compile_failed
         "compile-time evaluation exceeded the wall-clock budget \
          (TUNA_COMPILE_MAX_SECONDS)")

(* Tagged twin of upstream Tree.apply: same match arms, same
   evaluation order. Newly built wrapper nodes are tagged with the id
   of the application performing them.

   A prim gate tree in FUNCTION position of a compile-reducible
   application means the prim call would fire while compiling —
   prims only execute inside a run (with grants + journaling), so
   that is a compile error, not an effect. *)
let rec apply b tag a c =
  (match a with
   | TFork (_, TStem (_, TStem (_, TStem (_, TStem (_, TLeaf _)))), _) ->
       raise
         (Compile_failed
            "a prim call fired during compile-time evaluation: prims only run \
             inside a run — wrap the call in a lambda, e.g. (lambda (x) (prim ...))"
         )
   | _ -> ());
  match a with
  | TLeaf _ ->
      let r = TStem (tag, c) in
      check_size b r;
      r
  | TStem (_, a1) ->
      let r = TFork (tag, a1, c) in
      check_size b r;
      r
  | TFork (_, TLeaf _, a1) ->
      fire b;
      a1
  | TFork (_, TStem (_, a1), a2) ->
      fire b;
      let l = apply b tag a1 c in
      let r = apply b tag a2 c in
      apply b tag l r
  | TFork (_, TFork (_, a1, a2), a3) -> (
      fire b;
      match c with
      | TLeaf _ -> a1
      | TStem (_, u) -> apply b tag a2 u
      | TFork (_, u, v) ->
          let l = apply b tag a3 u in
          apply b tag l v)

(* Tagged twin of upstream to_tree: compile IS reduction. *)
let rec to_tagged b (t : cterm) : tagged =
  match t with
  | CLeaf id -> TLeaf id
  | CVar (_, y) -> raise (Compile_failed (Printf.sprintf "unbound variable %S escaped compilation" y))
  | CApp (id, m1, m2) ->
      let f = to_tagged b m1 in
      let a = to_tagged b m2 in
      apply b id f a
  | CLam (_, x, _) -> raise (Compile_failed (Printf.sprintf "lambda parameter %S was not eliminated" x))

(* ---------- artifact + provenance ---------- *)

(* Canonical tree path: 0 = stem child, 1 = fork left, 2 = fork right
   (constructor-descent choices from the root; matches the ternary
   encoding's digits). *)
type tree_path = int list

type artifact = {
  ir : Ir.t;  (* the named lambda IR, spans intact *)
  tree : Tuna.Tree.t;  (* compiled normal form *)
  ternary : string;  (* canonical serialization *)
  hash_hex : string;  (* sha256 over ternary, lowercase *)
  tags : (tree_path * int) list;  (* every node's responsible IR id, pre-order *)
  steps : int;  (* triage firings during compile-time reduction *)
}

(* Reflective self-application (bf2 bf2 not, crown-style defs) needs
   compile-time reduction well past 1e6 firings; the wall-clock cap at
   the request boundary (TUNA_COMPILE_MAX_SECONDS) is what keeps the
   bigger ceiling from becoming a pinner, so fuel stays a coarse
   ceiling rather than the defense. *)
let default_compile_fuel = 100_000_000
let default_compile_size_cap = 1_000_000

let compile ?(fuel = default_compile_fuel) ?(size_cap = default_compile_size_cap)
    ?(deadline = Float.infinity) (ir : Ir.t) : artifact =
  let b = { fuel; steps = 0; size_cap; deadline } in
  let t = to_tagged b (elim (of_ir ir)) in
  let tree =
    let rec go = function
      | TLeaf _ -> Tuna.Tree.Leaf
      | TStem (_, a) -> Tuna.Tree.Stem (go a)
      | TFork (_, a, c) -> Tuna.Tree.Fork (go a, go c)
    in
    go t
  in
  let rec tags acc path = function
    | TLeaf id -> (path, id) :: acc
    | TStem (id, a) -> tags ((path, id) :: acc) (path @ [ 0 ]) a
    | TFork (id, a, c) ->
        let acc = (path, id) :: acc in
        tags (tags acc (path @ [ 1 ]) a) (path @ [ 2 ]) c
  in
  let tags = List.rev (tags [] [] t) in
  {
    ir;
    tree;
    ternary = Tuna.Canon.encode tree;
    hash_hex = Tuna.Hash.hex_of_tree tree;
    tags;
    steps = b.steps;
  }

(* Convenience: compile surface source directly.  ?dictionary threads
   the REPL's name->tree bindings into the reader (M9). *)
let compile_source ?fuel ?size_cap ?deadline
    ?(dictionary : (string * Tuna.Tree.t) list = []) src =
  compile ?fuel ?size_cap ?deadline (Sexp.parse ~dictionary src)
