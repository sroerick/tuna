(* Tuna CLI: thin terminal clients.

   Subcommands:
     tuna eval <corpus-file>     evaluate a corpus entry (M3 harness)
     tuna compile <source-file>  compile a surface s-expr program
                                 (M4); prints hash/ternary/size/steps
     tuna eval-compiled <ternary> [args...] [--fuel N] [--cap N]
                                 apply a ternary program to ternary args *)

let read_file path =
  try
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s
  with Sys_error msg -> prerr_endline msg; exit 2

let print_result = function
  | Tuna_interp.Eval.Normal (t, steps) ->
      Printf.printf "normal %s %d\n" (Tuna.Canon.encode t) steps
  | Tuna_interp.Eval.Fuel_exhausted steps ->
      Printf.printf "fuel_exhausted - %d\n" steps
  | Tuna_interp.Eval.Size_exhausted steps ->
      Printf.printf "size_exhausted - %d\n" steps
  | Tuna_interp.Eval.Deadline_exceeded steps ->
      Printf.printf "deadline_exceeded - %d\n" steps

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
  (match Tuna.Canon.of_string !program with
  | Ok program -> (program, List.rev_map Tuna.Canon.parse !args, !fuel, !cap)
  | Error (off, msg) ->
      prerr_endline (Printf.sprintf "program parse error at %d: %s" off msg);
      exit 2)

let compile_cmd path =
  let src = read_file path in
  try
    let art = Tuna_compiler.Bracket.compile_source src in
    Printf.printf
      "hash %s\nternary %s\nsize %d\nsteps %d\n"
      art.hash_hex art.ternary
      (Tuna.Tree.size art.tree) art.steps
  with
  | Tuna_compiler.Ir.Error (p, msg) ->
      prerr_endline (Printf.sprintf "compile error: %s" (Tuna_compiler.Ir.show_error (p, msg)));
      exit 1
  | Tuna_compiler.Bracket.Compile_failed msg ->
      prerr_endline (Printf.sprintf "compile failed: %s" msg);
      exit 1

let eval_compiled_cmd argv off =
  (* argv.(off) = program ternary, then args, with optional --fuel/--cap *)
  let fuel = ref 10000 and cap = ref 10000 and rest = ref [] in
  let rec go i =
    if i >= Array.length argv then ()
    else
      match argv.(i) with
      | "--fuel" -> fuel := int_of_string argv.(i + 1); go (i + 2)
      | "--cap" -> cap := int_of_string argv.(i + 1); go (i + 2)
      | arg -> rest := arg :: !rest; go (i + 1)
  in
  go (off + 1); (* argv.(off) is the program; args start after it *)
  let program =
    match Tuna.Canon.of_string argv.(off) with
    | Ok p -> p
    | Error (off', msg) ->
        prerr_endline (Printf.sprintf "program parse error at %d: %s" off' msg);
        exit 2
  in
  let args = List.rev_map Tuna.Canon.parse !rest in
  print_result (Tuna_interp.Eval.eval ~fuel:!fuel ~size_cap:!cap ~program args)

let () =
  match Sys.argv.(1) with
  | "eval" ->
      let program, args, fuel, cap = read_corpus Sys.argv.(2) in
      print_result (Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~program args)
  | "compile" -> compile_cmd Sys.argv.(2)
  | "eval-compiled" -> eval_compiled_cmd Sys.argv 2
  | _ ->
      prerr_endline
        "usage: tuna eval <corpus-file> | tuna compile <source-file> | tuna eval-compiled <ternary> [args...]";
      exit 2
