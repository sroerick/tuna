(* Tuna M3 corpus tooling.

   Subcommands:
     gen refeval <corpus>   — instrumented VERBATIM copy of upstream
                              apply (reference/.../ocaml/lib/tree.ml) +
                              eval.ml's fuel/size_cap accounting; prints
                              "<status> <result-or--> <steps>". This is
                              the checked-in source of the expected
                              steps in scripts/diff-corpus/*.corpus.
     gen dag <file>         — expand an upstream DAG (let-binding)
                              program (dag.mts format) with eager
                              application; prints ternary.
     gen term <slug>        — compile a checked-in lambda term via
                              bracket abstraction (SKI, port of
                              upstream strategies.mts elim_bracket_ski)
                              and marshal; prints ternary.
     gen nat <n>            — upstream marshaller nat encoding
                              (LSB-first list of booleans); prints ternary.
     gen chain <n>          — intensional chain nat Stem^n Leaf; prints
                              ternary.

   Deliberately does NOT depend on tuna_interp: refeval must be an
   independent instrumented copy of the upstream code, not our
   interpreter. *)

(* ------------------------------------------------------------------ *)
(* Plain upstream apply (verbatim, reference .../ocaml/lib/tree.ml)    *)
(* ------------------------------------------------------------------ *)

type tree =
  | Leaf
  | Stem of tree
  | Fork of tree * tree

let rec apply a b =
  match a with
  | Leaf -> Stem b
  | Stem a -> Fork (a, b)
  | Fork (Leaf, a) -> a
  | Fork (Stem a1, a2) -> apply (apply a1 b) (apply a2 b)
  | Fork (Fork (a1, a2), a3) -> (
      match b with
      | Leaf -> a1
      | Stem u -> apply a2 u
      | Fork (u, v) -> apply (apply a3 u) v)

let rec size = function
  | Leaf -> 1
  | Stem a -> 1 + size a
  | Fork (a, b) -> 1 + size a + size b

let rec encode = function
  | Leaf -> "0"
  | Stem a -> "1" ^ encode a
  | Fork (a, b) -> "2" ^ encode a ^ encode b

let decode s =
  let i = ref 0 in
  let rec go () =
    if !i >= String.length s then failwith "truncated ternary";
    let c = s.[!i] in
    incr i;
    match c with
    | '0' -> Leaf
    | '1' -> Stem (go ())
    | '2' ->
        let a = go () in
        let b = go () in
        Fork (a, b)
    | _ -> failwith "bad char"
  in
  let r = go () in
  if !i <> String.length s then failwith "trailing chars";
  r

(* ------------------------------------------------------------------ *)
(* Instrumented refeval: same arms, same order, counts triage firings; *)
(* fuel/size accounting copied from interpreter/lib/eval.ml.           *)
(* ------------------------------------------------------------------ *)

exception Fuel_out
exception Size_out

type budget = { mutable fuel : int; mutable steps : int; size_cap : int }

let check_size b t = if size t > b.size_cap then raise Size_out

let fire b =
  if b.fuel = 0 then raise Fuel_out;
  b.fuel <- b.fuel - 1;
  b.steps <- b.steps + 1

let rec applyi b a c =
  match a with
  | Leaf ->
      let r = Stem c in
      check_size b r;
      r
  | Stem a1 ->
      let r = Fork (a1, c) in
      check_size b r;
      r
  | Fork (Leaf, a1) ->
      fire b;
      a1
  | Fork (Stem a1, a2) ->
      fire b;
      let l = applyi b a1 c in
      let r = applyi b a2 c in
      applyi b l r
  | Fork (Fork (a1, a2), a3) ->
      fire b;
      (match c with
       | Leaf -> a1
       | Stem u -> applyi b a2 u
       | Fork (u, v) ->
           let l = applyi b a3 u in
           applyi b l v)

let refeval fuel size_cap program args =
  let b = { fuel; steps = 0; size_cap } in
  try
    check_size b program;
    List.iter (check_size b) args;
    let rec go acc = function
      | [] -> acc
      | arg :: rest ->
          let r = applyi b acc arg in
          check_size b r;
          go r rest
    in
    let t = go program args in
    Printf.printf "normal %s %d\n" (encode t) b.steps
  with
  | Fuel_out -> Printf.printf "fuel_exhausted - %d\n" b.steps
  | Size_out -> Printf.printf "size_exhausted - %d\n" b.steps

(* Corpus file reader: only needs program/arg/fuel/size_cap. *)
let read_corpus path =
  let program = ref "" and args = ref [] and fuel = ref 1000 and cap = ref 1000 in
  (try
     let ic = open_in path in
     (try
        while true do
          let line = input_line ic in
          let line = String.trim line in
          if line <> "" && line.[0] <> '#' then (
            match String.index_opt line ' ' with
            | None -> ()
            | Some sp ->
                let key = String.sub line 0 sp in
                let val_ = String.trim (String.sub line (sp + 1) (String.length line - sp - 1)) in
                match key with
                | "program" -> program := val_
                | "arg" -> args := val_ :: !args
                | "fuel" -> fuel := int_of_string val_
                | "size_cap" -> cap := int_of_string val_
                | _ -> ())
        done
      with End_of_file -> close_in ic)
   with Sys_error msg -> prerr_endline msg; exit 2);
  (decode !program, List.rev_map decode !args, !fuel, !cap)

let cmd_refeval path =
  let program, args, fuel, cap = read_corpus path in
  refeval fuel cap program args

(* ------------------------------------------------------------------ *)
(* DAG expansion (upstream format/dag.mts `of` semantics)              *)
(* ------------------------------------------------------------------ *)

let expand_dag path =
  let env : (string, tree) Hashtbl.t = Hashtbl.create 32 in
  Hashtbl.add env "\u{25b3}" Leaf;
  let get n =
    match Hashtbl.find_opt env n with
    | Some t -> t
    | None -> failwith ("unbound variable: " ^ n)
  in
  let ic = open_in path in
  (try
     while true do
       let line = input_line ic in
       let line = String.trim line in
       if line <> "" && line.[0] <> '#' then (
         let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
         match parts with
         | [ a ] -> Hashtbl.replace env "RESULT" (get a)
         | [ a; b ] -> Hashtbl.replace env a (get b)
         | [ a; b; c ] ->
             let t = apply (get b) (get c) in
             Hashtbl.replace env a t
         | _ -> failwith "bad dag line")
     done
   with End_of_file -> ());
  close_in ic;
  match Hashtbl.find_opt env "RESULT" with
  | Some t -> print_string (encode t); print_newline ()
  | None -> failwith "no result line"

(* ------------------------------------------------------------------ *)
(* Bracket abstraction (port of strategies.mts elim_bracket_ski)       *)
(* ------------------------------------------------------------------ *)

type term =
  | N
  | App of term * term
  | V of string
  | Abs of string * term
  | L of tree (* literal tree injection: k1 (L t) marshals to Fork (Leaf, t) *)
  | TCase of term * term * term (* tree-structure T{f0,f1,f2}, NOT application:
      marshals to Fork (Fork (f0', f1'), f2'); dispatch: apply to Leaf
      gives f0, to Stem u gives f1 u, to Fork(u,v) gives f2 u v *)

let app t xs = List.fold_left (fun acc x -> App (acc, x)) t xs
let k_op = App (N, N)
let k1 u = App (k_op, u)
let s1 u = App (N, App (N, u))
let s2 u v = App (s1 u, v)
let i_op = s2 k_op N

let rec elim name = function
  | N -> k1 N
  | TCase (a, b, c) -> TCase (elim name a, elim name b, elim name c)
  | App (a, b) -> s2 (elim name a) (elim name b)
  | V v -> if v = name then i_op else k1 (V v)
  | L t -> k1 (L t)
  | Abs _ -> failwith "unexpected abs"

let rec elim_abs = function
  | N -> N
  | TCase (a, b, c) -> TCase (elim_abs a, elim_abs b, elim_abs c)
  | App (a, b) -> App (elim_abs a, elim_abs b)
  | V v -> V v
  | L t -> L t
  | Abs (n, body) -> elim n (elim_abs body)

let rec marshal_term = function
  | N -> Leaf
  | App (a, b) -> apply (marshal_term a) (marshal_term b)
  | L t -> t
  | TCase (f0, f1, f2) ->
      Fork (Fork (marshal_term f0, marshal_term f1), marshal_term f2)
  | _ -> failwith "unexpected term in marshal"

let compile t = marshal_term (elim_abs t)

(* church numerals: n = λf.λx. f^n x *)
let rec church_body n f x =
  if n = 0 then V x else App (V f, church_body (n - 1) f x)

let church n = Abs ("f", Abs ("x", church_body n "f" "x"))

let succ_ch =
  Abs ("n", Abs ("f", Abs ("x", App (V "f", app (V "n") [ V "f"; V "x" ]))))

let add_ch =
  Abs ("m", Abs ("n", Abs ("f", Abs ("x",
    App (App (V "m", V "f"), App (App (V "n", V "f"), V "x"))))))

let mul_ch =
  Abs ("m", Abs ("n", Abs ("f", App (V "m", App (V "n", V "f")))))

(* fix construction ported verbatim from upstream test.mts *)
let sa_k = Abs ("x", app (V "x") [ k1 (V "x") ])
let self_apply_k = Abs ("x", App (V "x", k1 (V "x")))
let wait a =
  Abs ("b", Abs ("c", app (s1 a) [ k1 (V "c"); V "b" ]))
let wait1 a = s1 (App (s1 (k1 (s1 a)), k_op))
let fix functional =
  app (wait self_apply_k)
    [ Abs ("x", App (functional, App (wait1 self_apply_k, V "x"))) ]

let s_op =
  Abs ("a", Abs ("b", Abs ("c", app (V "a") [ V "c"; app (V "b") [ V "c" ] ])))

let c_op =
  Abs ("a", Abs ("b", Abs ("c", app (V "a") [ V "c"; V "b" ])))

let b_op =
  Abs ("a", Abs ("b", Abs ("c", App (V "a", app (V "b") [ V "c" ]))))

let r_op =
  Abs ("a", Abs ("b", Abs ("c", app (V "b") [ V "c"; V "a" ])))

let t_op = Abs ("a", Abs ("b", App (V "b", V "a")))

(* K as a surface term: λa.λb. a *)
let k_term = Abs ("a", Abs ("b", V "a"))

(* T-program builder: T{f0,f1,f2} = Δ(Δ f0 f1) f2 — triage case analysis.
   Tree structure, NOT application: App(App(N, App(f0,f1)), f2) would
   eagerly apply f0 to f1 during marshaling and wreck the dispatch. *)
let tcase f0 f1 f2 = TCase (f0, f1, f2)

(* boolean ops via triage dispatch. T{f0,f1,f2} applied to x:
   x=Leaf -> f0; x=Stem u -> f1 u; x=Fork(u,v) -> f2 u v.
   In tree calculus only Leaf is false; Stem/Fork are true.
   and a b = T{λu.false, λu.λv.b, λx.λy.λz.b} a b
   or   a b = T{λu.b,      λu.λv.true, λx.λy.λz.true} a b *)
let and_op =
  Abs ("a", Abs ("b",
    app (tcase (Abs ("u", N))
           (Abs ("u", Abs ("v", V "b")))
           (Abs ("x", Abs ("y", Abs ("z", V "b")))))
      [ V "a"; V "b" ]))

(* or a b = not (and (not a) (not b)) — composed from the verified and
   and not; raw-variable triage branches (λu.b / b) do not survive
   bracket abstraction as pointwise functions, so don't use them. *)
let not_tree = decode "22102000"
let and_tree = marshal_term (elim_abs and_op)

let or_op =
  Abs ("a", Abs ("b",
    App (L not_tree,
         app (L and_tree)
           [ App (L not_tree, V "a"); App (L not_tree, V "b") ])))

(* S K K acts as the identity (S K K a = K a (K a) = a). *)
let s_k_k = App (App (s_op, k_term), k_term)

(* not ∘ not = λx. not (not x); compiled, so it reduces not-not-x
   in one go. *)
let not_not =
  Abs ("x", App (L (decode "22102000"), App (L (decode "22102000"), V "x")))

(* ------------------------------------------------------------------ *)
(* Upstream marshaller nats: nat = list of bools, LSB first            *)
(* ------------------------------------------------------------------ *)

let rec of_nat n =
  if n = 0 then Leaf
  else
    let bit = if n land 1 = 1 then Stem Leaf else Leaf in
    Fork (bit, of_nat (n asr 1))

let () =
  let slug = Sys.argv.(1) in
  match slug with
  | "refeval" -> cmd_refeval Sys.argv.(2)
  | "dag" -> expand_dag Sys.argv.(2)
  | "term" ->
      let t =
        match Sys.argv.(2) with
        | "i" -> i_op
        | "s" -> s_op
        | "c" -> c_op
        | "b" -> b_op
        | "r" -> r_op
        | "t" -> t_op
        | "sa_k" -> sa_k
        | "k" -> k_term
        | "and" -> and_op
        | "or" -> or_op
        | "s_k_k" -> s_k_k
        | "not_not" -> not_not
        | "fix" -> fix i_op
        | "add" -> add_ch
        | "mul" -> mul_ch
        | "succ_ch" -> succ_ch
        | "church" -> church (int_of_string Sys.argv.(3))
        | s -> failwith ("unknown term " ^ s)
      in
      print_string (encode (compile t));
      print_newline ()
  | "nat" -> print_string (encode (of_nat (int_of_string Sys.argv.(2)))); print_newline ()
  | "chain" ->
      let n = int_of_string Sys.argv.(2) in
      let rec go k t = if k = 0 then t else go (k - 1) (Stem t) in
      print_string (encode (go n Leaf));
      print_newline ()
  | _ -> failwith "unknown subcommand"
