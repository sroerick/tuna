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
  d "ref-fn" "(lambda (self) (lambda (i) (lambda (xs) ((tree-case %0 (lambda (c) %0) (lambda (hd) (lambda (tl) ((tree-case (%0 hd) (lambda (c) (self c tl)) (lambda (u) (lambda (v) %0))) i)))) xs))))";
  d "list-ref" "(lambda (i) (lambda (xs) ((rec-fix ref-fn) i xs)))";
  d "sum-fn" "(lambda (self) (lambda (n) ((tree-case %0 (lambda (c) (%0 (self c))) (lambda (l) (lambda (r) %0))) n)))";
  d "nat-sum" "(rec-fix sum-fn)";
  d "eq-fn" "(lambda (self) (lambda (a) (lambda (b) ((tree-case (is-leaf b) (lambda (c) ((tree-case %0 (lambda (c2) (self c c2)) (lambda (u) (lambda (v) %0))) b)) (lambda (l) (lambda (r) ((tree-case %0 (lambda (c) %0) (lambda (l2) (lambda (r2) (bool-and (self l l2) (self r r2))))) b)))) a))))";
  d "tree-eq" "(rec-fix eq-fn)";
  d "list-eq" "(lambda (xs) (lambda (ys) (tree-eq xs ys)))";
  d "mag-canonical" "(lambda (bits) (list-fold (lambda (hd) (lambda (acc) ((tree-case ((tree-case %0 (lambda (c) (pair %10 %0)) (lambda (u) (lambda (v) %0))) hd) (lambda (c2) (pair hd acc)) (lambda (ua) (lambda (va) (pair hd acc)))) acc))) %0 bits))";
  d "int-canonical" "(lambda (n) ((tree-case (pair %0 %0) (lambda (c) (pair %0 %0)) (lambda (sign) (lambda (mag) ((lambda (m) ((tree-case (pair %0 %0) (lambda (c2) (pair (is-stem sign) m)) (lambda (u2) (lambda (v2) (pair (is-stem sign) m)))) m)) (mag-canonical mag))))) n))";
  d "int-zero" "%200";
  d "int-one" "%202100";
  d "int-neg" "(lambda (n) ((lambda (c) ((tree-case (pair %0 %0) (lambda (j) (pair %0 %0)) (lambda (s) (lambda (m) ((tree-case (pair %0 %0) (lambda (j2) (pair (not s) m)) (lambda (u) (lambda (v) (pair (not s) m)))) m)))) c)) (int-canonical n)))";
  d "mag-ripple-fn" "(lambda (self) (lambda (xs) (lambda (c) ((tree-case ((tree-case %0 (lambda (jc) (pair %10 %0)) (lambda (u1) (lambda (v1) %0))) c) (lambda (jx) ((tree-case %0 (lambda (jc) (pair %10 %0)) (lambda (u1) (lambda (v1) %0))) c)) (lambda (hd) (lambda (tl) ((tree-case (pair hd tl) (lambda (jc) ((tree-case (pair %10 tl) (lambda (jh) (pair %0 (self tl %10))) (lambda (u2) (lambda (v2) (pair %10 tl)))) hd)) (lambda (u3) (lambda (v3) (pair hd tl)))) c)))) xs))))";
  d "mag-ripple" "(lambda (xs) (lambda (c) ((rec-fix mag-ripple-fn) xs c)))";
  d "mag-add-fn" "(lambda (self) (lambda (a) (lambda (b) (lambda (c) ((tree-case (mag-ripple b c) (lambda (ja) (mag-ripple b c)) (lambda (ahd) (lambda (atl) ((tree-case (mag-ripple (pair ahd atl) c) (lambda (jb) (mag-ripple (pair ahd atl) c)) (lambda (bhd) (lambda (btl) (pair (bool-xor (bool-xor ahd bhd) c) (self atl btl (bool-or (bool-and ahd bhd) (bool-and c (bool-xor ahd bhd)))))))) b)))) a)))))";
  d "mag-add" "(lambda (a) (lambda (b) (mag-canonical (((rec-fix mag-add-fn) a) b %0))))";
  d "mag-cmp-fn" "(lambda (self) (lambda (a) (lambda (b) ((tree-case ((if %0 %10) (list-fold (lambda (h) (lambda (acc) (bool-or h acc))) %0 b)) (lambda (j12) ((if %0 %10) (list-fold (lambda (h) (lambda (acc) (bool-or h acc))) %0 b))) (lambda (ahd) (lambda (atl) ((tree-case ((if %110 %10) (list-fold (lambda (h) (lambda (acc) (bool-or h acc))) %0 a)) (lambda (jb3) ((if %110 %10) (list-fold (lambda (h) (lambda (acc) (bool-or h acc))) %0 a))) (lambda (bhd) (lambda (btl) ((tree-case %0 (lambda (c) ((tree-case ((tree-case ((tree-case %10 (lambda (jb1) %0) (lambda (j1) (lambda (j2) %10))) bhd) (lambda (jb0) ((tree-case %110 (lambda (jb2) %10) (lambda (j3) (lambda (j4) %110))) bhd)) (lambda (j5) (lambda (j6) %10))) ahd) (lambda (j7) %110) (lambda (j8) (lambda (j9) ((tree-case ((tree-case %10 (lambda (jb1) %0) (lambda (j1) (lambda (j2) %10))) bhd) (lambda (jb0) ((tree-case %110 (lambda (jb2) %10) (lambda (j3) (lambda (j4) %110))) bhd)) (lambda (j5) (lambda (j6) %10))) ahd)))) c)) (lambda (j10) (lambda (j11) ((tree-case ((tree-case %10 (lambda (jb1) %0) (lambda (j1) (lambda (j2) %10))) bhd) (lambda (jb0) ((tree-case %110 (lambda (jb2) %10) (lambda (j3) (lambda (j4) %110))) bhd)) (lambda (j5) (lambda (j6) %10))) ahd)))) (self atl btl))))) b)))) a))))";
  d "mag-cmp" "(lambda (a) (lambda (b) ((rec-fix mag-cmp-fn) a b)))";
  d "mag-ripsub-fn" "(lambda (self) (lambda (xs) ((tree-case %0 (lambda (rj3) %0) (lambda (hd) (lambda (tl) ((tree-case (pair %10 (self tl)) (lambda (jh) (pair %0 tl)) (lambda (rj1) (lambda (rj2) (pair %0 tl)))) hd)))) xs)))";
  d "mag-ripsub" "(lambda (xs) (mag-canonical ((rec-fix mag-ripsub-fn) xs)))";
  d "mag-sub-fn" "(lambda (self) (lambda (a) (lambda (b) (lambda (w) ((tree-case %0 (lambda (rj8) %0) (lambda (ahd) (lambda (atl) ((tree-case ((tree-case a (lambda (rj4) (mag-ripsub a)) (lambda (rj5) (lambda (rj6) (mag-ripsub a)))) w) (lambda (rj7) ((tree-case a (lambda (rj4) (mag-ripsub a)) (lambda (rj5) (lambda (rj6) (mag-ripsub a)))) w)) (lambda (bhd) (lambda (btl) (pair (bool-xor ahd (bool-xor bhd w)) (self atl btl (bool-or (bool-and bhd w) (bool-and (not ahd) (bool-or bhd w)))))))) b)))) a)))))";
  d "mag-sub" "(lambda (a) (lambda (b) (mag-canonical (((rec-fix mag-sub-fn) a) b %0))))";

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

  eval "mag-canonical" ["%0"];            (* nil -> nil *)
  eval "mag-canonical" ["%2100"];         (* [t] -> [t] *)
  eval "mag-canonical" ["%200"];          (* [f] -> nil *)
  eval "mag-canonical" ["%20200"];        (* [f,f] -> nil *)
  eval "mag-canonical" ["%202100"];       (* [f,t] (=2) -> [f,t] unchanged *)
  eval "mag-canonical" ["%210200"];       (* [t,f] (=1) -> [t] *)
  eval "mag-canonical" ["%20210200"];     (* [f,t,f] (=2) -> [f,t] *)
  eval "mag-canonical" ["%202102100"];    (* [f,t,t] (=6) -> unchanged *)
  eval "mag-canonical" ["%10"];           (* junk stem spine -> nil *)
  eval "int-canonical" ["%200"];           (* +0 -> %200 *)
  eval "int-canonical" ["%202100"];        (* +1 unchanged *)
  eval "int-canonical" ["%2102100"];       (* -1 unchanged *)
  eval "int-canonical" ["%2100"];          (* -0 -> +0 (both zeros one form) *)
  eval "int-canonical" ["%20210200"];      (* +(mag [t,f]) -> +1 *)
  eval "int-canonical" ["%2020200"];       (* +(mag [f,f]) -> +0 *)
  eval "int-canonical" ["%22002100"];      (* junk fork sign, [t] -> +1 *)
  eval "int-canonical" ["%0"];             (* junk leaf int -> +0 *)
  eval "int-canonical" ["%10"];            (* junk stem int -> +0 *)
  eval "int-canonical" ["int-zero"];       (* alias pins *)
  eval "int-canonical" ["int-one"];
  eval "int-neg" ["int-one"];            (* +1 -> -1 *)
  eval "int-neg" ["%2102100"];           (* -1 -> +1 *)
  eval "int-neg" ["int-zero"];           (* 0 -> 0, never -0 *)
  eval "int-neg" ["%2100"];              (* -0 in -> +0 out *)
  eval "int-neg" ["%0"];                 (* junk -> +0 *)
  eval "int-neg" ["%20202100"];          (* +2 -> -2 *)
  eval "mag-ripple" ["%0"; "%0"];        (* nil + no carry -> nil *)
  eval "mag-ripple" ["%0"; "%10"];       (* nil + carry -> [t] *)
  eval "mag-ripple" ["%2100"; "%0"];     (* [t] unchanged *)
  eval "mag-ripple" ["%2100"; "%10"];    (* 1+1 = [f,t] *)
  eval "mag-ripple" ["%2102100"; "%10"]; (* 3+1 = [f,f,t] = 4 *)
  eval "mag-add" ["%0"; "%0"];           (* 0+0 *)
  eval "mag-add" ["%2100"; "%2100"];     (* 1+1 = 2 = [f,t] *)
  eval "mag-add" ["%2102100"; "%2100"];  (* 3+1 = 4 = [f,f,t] *)
  eval "mag-add" ["%210202100"; "%202100"]; (* 5+2 = 7 = [t,t,t] *)
  eval "mag-add" ["%0"; "%210202100"];   (* 0+5 = 5 *)
  eval "mag-add" ["%200"; "%2100"];      (* junk bit [f] + 1 -> 1 *)
  eval "mag-cmp" ["%0"; "%0"];           (* 0 = 0 *)
  eval "mag-cmp" ["%2100"; "%0"];        (* 1 > 0 *)
  eval "mag-cmp" ["%0"; "%2100"];        (* 0 < 1 *)
  eval "mag-cmp" ["%2100"; "%2100"];     (* 1 = 1 *)
  eval "mag-cmp" ["%2100"; "%202100"];   (* 1 < 2 *)
  eval "mag-cmp" ["%202100"; "%2100"];   (* 2 > 1 *)
  eval "mag-cmp" ["%2102100"; "%202100"]; (* 3 > 2 *)
  eval "mag-cmp" ["%210202100"; "%202102100"]; (* 5 < 6: low-pos difference never overrides *)
  eval "mag-cmp" ["%210200"; "%2100"];   (* [t,f] = [t]: trailing false inert *)
  eval "mag-cmp" ["%10"; "%0"];          (* junk stem reads 0 -> eq *)
  eval "mag-ripsub" ["%2100"];           (* 1-1 = 0 *)
  eval "mag-ripsub" ["%20202100"];       (* 4-1 = 3, borrow ripple *)
  eval "mag-ripsub" ["%2102100"];        (* 3-1 = 2 *)
  eval "mag-sub" ["%0"; "%0"];           (* 0-0 *)
  eval "mag-sub" ["%2100"; "%2100"];     (* 1-1 *)
  eval "mag-sub" ["%202100"; "%2100"];   (* 2-1 = 1 *)
  eval "mag-sub" ["%2102100"; "%202100"]; (* 3-2 = 1 *)
  eval "mag-sub" ["%20202100"; "%2100"]; (* 4-1 = 3, borrow ripple *)
  eval "mag-sub" ["%20202100"; "%20202100"]; (* 4-4 = 0 *)
  eval "mag-sub" ["%210202100"; "%202100"]; (* 5-2 = 3 *)
  eval "mag-sub" ["%210200"; "%0"];      (* [t,f]-0 -> canonical [t] *)

  eval "is-zero" ["%0"];
  eval "is-zero" ["%10"];
  eval "ref-fn" ["ref-fn"; "%0"; "%0"];
  eval "ref-fn" ["ref-fn"; "%0"; "(pair %10 %0)"];
  eval "ref-fn" ["ref-fn"; "%10"; "(pair %10 (pair %200 %0))"];

  Printf.printf "\nfail_count=%d\n" !fail_count
