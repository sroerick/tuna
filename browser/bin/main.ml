(* tuna-js smoke driver: compile a surface program and evaluate it.
   Build: dune build browser/bin/main.bc.js ; run: node main.bc.js *)
let show = function
  | Tuna_interp.Eval.Normal (t, steps) ->
      Printf.sprintf "normal %s %d" (Tuna.Canon.encode t) steps
  | Tuna_interp.Eval.Loop steps -> Printf.sprintf "loop - %d" steps
  | Tuna_interp.Eval.Fuel_exhausted steps ->
      Printf.sprintf "fuel_exhausted - %d" steps
  | Tuna_interp.Eval.Size_exhausted steps ->
      Printf.sprintf "size_exhausted - %d" steps
  | Tuna_interp.Eval.Deadline_exceeded steps ->
      Printf.sprintf "deadline_exceeded - %d" steps

let () =
  let src = "(lambda (x) x)" in
  match Tuna_compiler.Bracket.compile_source src with
  | art ->
      Printf.printf "compiled hash=%s ternary=%s\n" art.hash_hex art.ternary;
      let arg = match Tuna.Canon.of_string "0" with Ok t -> [ t ] | Error _ -> [] in
      Printf.printf "eval: %s\n"
        (show (Tuna_interp.Eval.eval ~fuel:1000 ~size_cap:1000 ~program:art.tree arg))
  | exception Tuna_compiler.Ir.Error (p, msg) ->
      print_endline ("compile error: " ^ Tuna_compiler.Ir.show_error (p, msg))
  | exception Tuna_compiler.Bracket.Compile_failed msg ->
      print_endline ("compile failed: " ^ msg)