(* scratch probe for the stdlib defs. *)
let compile ~dictionary src =
  Tuna_compiler.Bracket.compile_source ~fuel:100000000 ~size_cap:100000000
    ~deadline:Float.infinity ~dictionary src
let compiled : (string * Tuna.Tree.t) list ref = ref []
let fail_count = ref 0
let d name src =
  match (try Ok (compile ~dictionary:!compiled src)
         with Tuna_compiler.Bracket.Compile_failed m -> Error m) with
  | Error m -> incr fail_count; Printf.printf "DEF %-14s FAILED: %s\n%!" name m
  | Ok art ->
      Printf.printf "DEF %-14s size=%-6d\n%!" name (Tuna.Tree.size art.Tuna_compiler.Bracket.tree);
      compiled := (name, art.Tuna_compiler.Bracket.tree) :: !compiled
let eval name argsrcs =
  let src = "(" ^ name ^ " " ^ String.concat " " argsrcs ^ ")" in
  match (try Ok (compile ~dictionary:!compiled src)
         with Tuna_compiler.Bracket.Compile_failed m -> Error m) with
  | Error m -> incr fail_count; Printf.printf "  EVAL %s COMPILE-FAIL: %s\n%!" src m
  | Ok art ->
      let open Tuna_interp.Eval in
      (match eval ~fuel:1000000 ~size_cap:10000000
              ~program:art.Tuna_compiler.Bracket.tree [] with
       | Normal (t, steps) ->
           let r = Tuna.Canon.encode t in
           Printf.printf "  %-52s -> %s  (%d steps)\n%!" src
             (if String.length r > 200 then "<big:" ^ string_of_int (String.length r) ^ ">" else r) steps
       | Loop s -> incr fail_count; Printf.printf "  %-52s -> LOOP (%d)\n%!" src s
       | Fuel_exhausted s -> incr fail_count; Printf.printf "  %-52s -> FUEL (%d)\n%!" src s
       | Size_exhausted s -> incr fail_count; Printf.printf "  %-52s -> SIZE (%d)\n%!" src s
       | Deadline_exceeded s -> incr fail_count; Printf.printf "  %-52s -> DEADLINE (%d)\n%!" src s)
let () =
  d "true" "%10";
  d "false" "%0";
  d "not" "%22102000";
  d "tree-case" "(lambda (la) (lambda (lb) (lambda (lf) (pair (pair la lb) lf))))";
  d "is-leaf" "(lambda (t) ((tree-case %10 (lambda (c) %0) (lambda (l) (lambda (r) %0))) t))";
  d "is-stem" "(lambda (t) ((tree-case %0 (lambda (c) %10) (lambda (l) (lambda (r) %0))) t))";
  d "is-fork" "(lambda (t) ((tree-case %0 (lambda (c) %0) (lambda (l) (lambda (r) %10))) t))";
  d "if" "(lambda (t) (lambda (f) (pair (pair f (pair %0 t)) %0)))";
  d "bool-and" "(lambda (a) (lambda (b) ((tree-case %0 (lambda (c) b) (lambda (l) (lambda (r) %0))) a)))";
  d "bool-or" "(lambda (a) (lambda (b) ((tree-case b (lambda (c) %10) (lambda (l) (lambda (r) %0))) a)))";
  d "bool-xor" "(lambda (x) (lambda (y) ((if (not y) y) x)))";
  d "first" "(lambda (p) ((tree-case %0 (lambda (c) %0) (lambda (l) (lambda (r) l))) p))";
  d "second" "(lambda (p) ((tree-case %0 (lambda (c) %0) (lambda (l) (lambda (r) r))) p))";
  d "is-zero" "(lambda (n) ((tree-case %10 (lambda (c) %0) (lambda (l) (lambda (r) %0))) n))";
  d "nat-pred" "(lambda (n) ((tree-case %0 (lambda (c) c) (lambda (l) (lambda (r) l))) n))";
  d "sa-k" "(lambda (x) (x (true x)))";
  d "wait" "(lambda (a) (lambda (b) (lambda (c) (((%0 (%0 a)) (true c)) b))))";
  d "wait1" "(lambda (a) (%0 (%0 ((%0 (%0 (true (%0 (%0 a))))) true))))";
  d "rec-fix" "(lambda (functional) ((wait sa-k) (lambda (x) (functional (wait1 sa-k x)))))";
  d "fold-fn" "(lambda (self) (lambda (xs) (lambda (f) (lambda (z) ((tree-case z (lambda (c) z) (lambda (hd) (lambda (tl) (f hd (self tl f z))))) xs)))))";
  d "list-fold" "(lambda (f) (lambda (z) (lambda (xs) ((((rec-fix fold-fn) xs) f) z))))";
  d "list-length" "(lambda (xs) (list-fold (lambda (h) (lambda (acc) (%0 acc))) %0 xs))";
  d "list-append" "(lambda (xs) (lambda (ys) (list-fold (lambda (h) (lambda (acc) (pair h acc))) ys xs)))";
  d "list-reverse" "(lambda (xs) (list-fold (lambda (h) (lambda (acc) (list-append acc (pair h %0)))) %0 xs))";
  d "list-map" "(lambda (g) (lambda (xs) (list-fold (lambda (h) (lambda (acc) (pair (g h) acc))) %0 xs)))";
  d "ref-fn" "(lambda (self) (lambda (i) (lambda (xs) ((tree-case %0 (lambda (c) %0) (lambda (hd) (lambda (tl) ((if (%0 hd) (self (nat-pred i) tl)) (is-zero i))))) xs))))";
  d "list-ref" "(lambda (i) (lambda (xs) ((rec-fix ref-fn) i xs)))";
  d "sum-fn" "(lambda (self) (lambda (n) ((tree-case %0 (lambda (c) (%0 (self c))) (lambda (l) (lambda (r) %0))) n)))";
  d "nat-sum" "(rec-fix sum-fn)";
  d "eq-fn" "(lambda (self) (lambda (a) (lambda (b) ((tree-case (is-leaf b) (lambda (c) ((tree-case %0 (lambda (c2) (self c c2)) (lambda (u) (lambda (v) %0))) b)) (lambda (l) (lambda (r) ((tree-case %0 (lambda (c) %0) (lambda (l2) (lambda (r2) (bool-and (self l l2) (self r r2))))) b)))) a))))";
  d "tree-eq" "(rec-fix eq-fn)";
  d "list-eq" "(lambda (xs) (lambda (ys) (tree-eq xs ys)))";

  eval "not" ["%10"]; eval "not" ["%0"];
  eval "is-leaf" ["%0"]; eval "is-leaf" ["%10"]; eval "is-leaf" ["%200"];
  eval "is-stem" ["%0"]; eval "is-stem" ["%10"]; eval "is-stem" ["%200"];
  eval "is-fork" ["%0"]; eval "is-fork" ["%10"]; eval "is-fork" ["%200"];
  eval "if" ["%1110"; "%200"; "%10"];
  eval "if" ["%1110"; "%200"; "%0"];
  eval "bool-and" ["%10"; "%10"]; eval "bool-and" ["%10"; "%0"];
  eval "bool-and" ["%0"; "%10"]; eval "bool-and" ["%0"; "%0"];
  eval "bool-or" ["%10"; "%0"]; eval "bool-or" ["%0"; "%0"];
  eval "bool-or" ["%0"; "%10"]; eval "bool-or" ["%10"; "%10"];
  eval "bool-xor" ["%10"; "%0"]; eval "bool-xor" ["%10"; "%10"];
  eval "bool-xor" ["%0"; "%10"]; eval "bool-xor" ["%0"; "%0"];
  eval "list-length" ["(pair %10 (pair %0 (pair %10 %0)))"];
  eval "list-length" ["%0"];
  eval "list-reverse" ["(pair %10 (pair %0 %0))"];
  eval "list-append" ["(pair %10 %0)"; "(pair %200 %0)"];
  eval "list-map" ["not"; "(pair %10 (pair %0 %0))"];
  eval "list-fold" ["(lambda (h) (lambda (acc) (pair h acc)))"; "%0"; "(pair %10 (pair %0 %0))"];
  eval "list-ref" ["%0"; "(pair %10 (pair %200 %0))"];
  eval "list-ref" ["%10"; "(pair %10 (pair %200 %0))"];
  eval "list-ref" ["%110"; "(pair %10 (pair %200 %0))"];
  eval "tree-eq" ["%10"; "%10"];
  eval "tree-eq" ["%10"; "%0"];
  eval "tree-eq" ["(pair %10 %0)"; "(pair %10 %0)"];
  eval "tree-eq" ["(pair %10 %0)"; "(pair %0 %10)"];
  eval "tree-eq" ["%22102000"; "%22102000"];
  eval "list-eq" ["(pair %10 %0)"; "(pair %10 %0)"];
  eval "list-eq" ["(pair %10 %0)"; "(pair %200 %0)"];
  eval "nat-sum" ["%0"]; eval "nat-sum" ["%10"]; eval "nat-sum" ["%1110"];

  eval "is-zero" ["%0"];
  eval "is-zero" ["%10"];
  eval "ref-fn" ["ref-fn"; "%0"; "%0"];
  eval "ref-fn" ["ref-fn"; "%0"; "(pair %10 %0)"];
  eval "ref-fn" ["ref-fn"; "%10"; "(pair %10 (pair %200 %0))"];

  Printf.printf "\nfail_count=%d\n" !fail_count
