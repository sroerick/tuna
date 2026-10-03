(* corpusgen: one scripts/diff-corpus entry per stdlib def
   (borg/stdlib.borg acceptance 2).  Prints a def-file
   (name/program/arg/fuel/size_cap) on stdout for a slug; --list prints
   the slugs.  Programs are the compiled (lambda (w) <case>): compile-IS
   reduction leaves the body open on w so the refeval twin measures
   RUNTIME steps, exactly the differential-harness law.  Ends are small
   on purpose (F6: int paths stay 1-3 bit, lists <= 3 elements). *)

let compile ~dictionary src =
  Tuna_compiler.Bracket.compile_source ~fuel:100000000 ~size_cap:100000000
    ~deadline:Float.infinity ~dictionary src

(* slug -> (case body in w, arg ternary).  Engine fn-defs are exercised
   through rec-fix, their real use. *)
let cases =
  [ ("stdlib_true", ("(pair true w)", "%0"))
  ; ("stdlib_false", ("(pair false w)", "%0"))
  ; ("stdlib_not", ("(not w)", "%10"))
  ; ("stdlib_tree-case", ("((tree-case w (lambda (c) c) (lambda (l) (lambda (r) r))) %10)", "%1110"))
  ; ("stdlib_is-leaf", ("(is-leaf w)", "%0"))
  ; ("stdlib_is-stem", ("(is-stem w)", "%10"))
  ; ("stdlib_is-fork", ("(is-fork w)", "%200"))
  ; ("stdlib_if", ("(if %1110 w %0)", "%200"))
  ; ("stdlib_bool-and", ("(bool-and w %10)", "%10"))
  ; ("stdlib_bool-or", ("(bool-or w %10)", "%0"))
  ; ("stdlib_bool-xor", ("(bool-xor w %10)", "%0"))
  ; ("stdlib_first", ("(first (pair w %200))", "%10"))
  ; ("stdlib_second", ("(second (pair %10 w))", "%200"))
  ; ("stdlib_is-zero", ("(is-zero w)", "%0"))
  ; ("stdlib_nat-pred", ("(nat-pred w)", "%1110"))
  ; ("stdlib_sa-k", ("(sa-k w)", "%21100"))
  ; ("stdlib_wait", ("(wait %0 %1110 w)", "%10"))
  ; ("stdlib_wait1", ("(wait1 w)", "%1110"))
  ; ("stdlib_rec-fix", ("((rec-fix sum-fn) w)", "%1110"))
  ; ("stdlib_fold-fn", ("(((rec-fix fold-fn) w) (lambda (h) (lambda (acc) (pair h acc))) %0)", "%2102100"))
  ; ("stdlib_list-fold", ("(list-fold (lambda (h) (lambda (acc) (pair h acc))) %0 w)", "%2102100"))
  ; ("stdlib_list-length", ("(list-length w)", "%2102100"))
  ; ("stdlib_list-append", ("(list-append w (pair %10 %0))", "%2100"))
  ; ("stdlib_list-reverse", ("(list-reverse w)", "%210200"))
  ; ("stdlib_list-map", ("(list-map not w)", "%210200"))
  ; ("stdlib_ref-fn", ("(ref-fn ref-fn %0 w)", "%2100"))
  ; ("stdlib_list-ref", ("(list-ref w (pair %10 (pair %200 %0)))", "%10"))
  ; ("stdlib_sum-fn", ("((rec-fix sum-fn) w)", "%1110"))
  ; ("stdlib_nat-sum", ("(nat-sum w)", "%1110"))
  ; ("stdlib_eq-fn", ("((rec-fix eq-fn) %10 w)", "%10"))
  ; ("stdlib_tree-eq", ("(tree-eq %22102000 w)", "%22102000"))
  ; ("stdlib_list-eq", ("(list-eq w (pair %10 %0))", "%2100"))
  ; ("stdlib_mag-canonical", ("(mag-canonical w)", "%210200"))
  ; ("stdlib_int-canonical", ("(int-canonical w)", "%2100"))
  ; ("stdlib_int-zero", ("(pair int-zero w)", "%0"))
  ; ("stdlib_int-one", ("(pair int-one w)", "%0"))
  ; ("stdlib_int-neg", ("(int-neg w)", "%202100"))
  ; ("stdlib_mag-ripple-fn", ("((rec-fix mag-ripple-fn) w %10)", "%2102100"))
  ; ("stdlib_mag-ripple", ("(mag-ripple w %10)", "%2102100"))
  ; ("stdlib_mag-add-fn", ("(((rec-fix mag-add-fn) w (pair %10 %0)) %0)", "%2100"))
  ; ("stdlib_mag-add", ("(mag-add w (pair %10 %0))", "%2102100"))
  ; ("stdlib_mag-cmp-fn", ("((rec-fix mag-cmp-fn) w (pair %10 %0))", "%2102100"))
  ; ("stdlib_mag-cmp", ("(mag-cmp w (pair %10 %0))", "%2102100"))
  ; ("stdlib_mag-ripsub-fn", ("((rec-fix mag-ripsub-fn) w)", "%2102100"))
  ; ("stdlib_mag-ripsub", ("(mag-ripsub w)", "%2102100"))
  ; ("stdlib_mag-sub-fn", ("(((rec-fix mag-sub-fn) w (pair %10 %0)) %0)", "%2102100"))
  ; ("stdlib_mag-sub", ("(mag-sub w (pair %10 %0))", "%2102100"))
  ; ("stdlib_int-add", ("(int-add w int-one)", "%202100"))
  ; ("stdlib_int-sub", ("(int-sub w int-one)", "%20202100"))
  ; ("stdlib_int-cmp", ("(int-cmp w int-zero)", "%202100"))
  ; ("stdlib_mag-mul-fn", ("(((rec-fix mag-mul-fn) (pair %10 %0) w) %0)", "%2102100"))
  ; ("stdlib_mag-mul", ("(mag-mul w %202100)", "%2102100"))
  ; ("stdlib_int-mul", ("(int-mul w %20202100)", "%2102100"))
  ; ( "dialect_rec-get-present",
      ("(rec-get %202100 w)", "%222021002021020210022202021002000") )
  ; ( "dialect_rec-get-absent",
      ("(rec-get %20202100 w)", "%222021002021020210022202021002000") )
  ; ( "dialect_rec-has",
      ("(rec-has %202100 w)", "%222021002021020210022202021002000") )
  ; ( "dialect_rec-val",
      ("(rec-val %202100 w)", "%222021002021020210022202021002000") )
  ; ( "dialect_rec-upd",
      ("(rec-upd %202100 %20202100 w)", "%222021002021020210022202021002000") )
  ; ( "dialect_list-filter",
      ("(list-filter is-leaf w)", "%2100") )
  ; ( "dialect_bracket_let",
      ("(let ((a 1) (b 2)) [a b 42 -7])", "%0") )
  ; ( "dialect_bracket_arg",
      ("[w 1 2]", "%110") )
  ; ( "dialect_record_shape",
      ("[[1 w] [2 0]]", "%10") )
  ; ( "dialect_keyed_literal",
      ("{:state todo-open :title w}", "%10") )
  ; ( "dialect_keyed_accessor",
      ("(todo-state {:state todo-open :title w})", "%10") )
  ; ( "dialect_todo_flip",
      ("(todo-state (todo-flip {:state todo-open :title w}))", "%10") )
  ]

let () =
  let dictionary =
    List.fold_left
      (fun acc (name, src) ->
        let art = compile ~dictionary:acc src in
        (name, art.Tuna_compiler.Bracket.tree) :: acc)
      [] Stddefs.all
  in
  match Array.to_list Sys.argv with
  | [ _; "--list" ] -> List.iter (fun (s, _) -> print_endline s) cases
  | [ _; slug ] -> (
      match List.assoc_opt slug cases with
      | None ->
          Printf.ksprintf failwith "unknown corpus slug %S (try --list)" slug
      | Some (body, arg_ternary) ->
          let src = "(lambda (w) " ^ body ^ ")" in
          (match compile ~dictionary src with
           | exception Tuna_compiler.Bracket.Compile_failed m ->
               Printf.ksprintf failwith "compile %s: %s" slug m
           | art ->
               (* corpus arg lines are RAW ternary (no % prefix) *)
               let raw = String.sub arg_ternary 1 (String.length arg_ternary - 1) in
               Printf.printf "name %s\nprogram %s\narg %s\nfuel 100000\nsize_cap 100000\n"
                 slug art.Tuna_compiler.Bracket.ternary raw))
  | _ -> failwith "usage: corpusgen --list | <slug>"
