(* Tuna CLI: thin terminal clients.

   Subcommands:
     tuna eval <corpus-file>     evaluate a corpus entry (M3 harness)
     tuna compile <source-file>  compile a surface s-expr program
                                 (M4); prints hash/ternary/size/steps
     tuna eval-compiled <ternary> [args...] [--fuel N] [--cap N]
                                 apply a ternary program to ternary args
     tuna eval-source <source-file> [arg-terms...] [--fuel N] [--cap N]
                                 compile a source program and each arg
                                 term, then apply (borg/local.borg L1)

   Source compiles resolve names against the frozen stdlib
   (Tuna_stdlib.Std_dict, borg/local.borg L1): lambda param > std >
   reader builtins, as on a server for an identity with no defs. *)

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
  | Tuna_interp.Eval.Loop steps ->
      Printf.printf "loop - %d\n" steps
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

(* compile a source string against the frozen stdlib; answers the
   artifact and the lock (each std name the source resolved, with its
   sha256, sorted and deduplicated - tool-language T2).  Compile errors
   exit 1 with the same messages as before. *)
let compile_std src =
  let resolved = ref [] in
  try
    let art =
      Tuna_compiler.Bracket.compile_source
        ~dictionary:(Tuna_stdlib.Std_dict.dictionary ())
        ~on_resolve:(fun name _off -> resolved := name :: !resolved)
        src
    in
    let lock =
      List.sort_uniq compare !resolved
      |> List.filter_map (fun name ->
             List.find_map
               (fun (n, h, _) -> if n = name then Some (n, h) else None)
               Tuna_stdlib.Std_dict.rows)
    in
    (art, lock)
  with
  | Tuna_compiler.Ir.Error (p, msg) ->
      prerr_endline (Printf.sprintf "compile error: %s" (Tuna_compiler.Ir.show_error (p, msg)));
      exit 1
  | Tuna_compiler.Bracket.Compile_failed msg ->
      prerr_endline (Printf.sprintf "compile failed: %s" msg);
      exit 1

let compile_cmd path =
  let art, lock = compile_std (read_file path) in
  Printf.printf
    "hash %s\nternary %s\nsize %d\nsteps %d\n"
    art.hash_hex art.ternary
    (Tuna.Tree.size art.tree) art.steps;
  List.iter (fun (n, h) -> Printf.printf "lock %s %s\n" n h) lock

(* run flags shared by eval-compiled and eval-source: argv from [first]
   on is positional args plus optional --fuel/--cap/--semantics (v0
   canonical | v1 distinct-work | fast1-fast3 accelerated). *)
let parse_run_flags argv first =
  let fuel = ref 10000 and cap = ref 10000 and rest = ref [] in
  let mode = ref Tuna_interp.Eval.Canonical in
  let accel = ref None in
  let rec go i =
    if i >= Array.length argv then ()
    else
      match argv.(i) with
      | "--fuel" -> fuel := int_of_string argv.(i + 1); go (i + 2)
      | "--cap" -> cap := int_of_string argv.(i + 1); go (i + 2)
      | "--semantics" ->
          mode :=
            (match argv.(i + 1) with
             | "v1" -> Tuna_interp.Eval.Sharing
             | _ -> Tuna_interp.Eval.Canonical);
          (* fast1 (borg/eval-modes.borg E2): v0 counting + accelerators *)
          accel := Tuna_interp.Accel.registry_of_semantics argv.(i + 1);
          go (i + 2)
      | arg -> rest := arg :: !rest; go (i + 1)
  in
  go first;
  (!fuel, !cap, !mode, !accel, List.rev !rest)

let eval_compiled_cmd argv off =
  (* argv.(off) = program ternary, then args and run flags *)
  let fuel, cap, mode, accel, rest = parse_run_flags argv (off + 1) in
  let program =
    match Tuna.Canon.of_string argv.(off) with
    | Ok p -> p
    | Error (off', msg) ->
        prerr_endline (Printf.sprintf "program parse error at %d: %s" off' msg);
        exit 2
  in
  let args = List.map Tuna.Canon.parse rest in
  print_result
    (Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~mode ?accel ~program args)

let eval_source_cmd argv off =
  (* argv.(off) = source file; each positional arg is a source TERM
     compiled against the same dictionary (a raw tree is %<ternary>) *)
  let fuel, cap, mode, accel, rest = parse_run_flags argv (off + 1) in
  let art, lock = compile_std (read_file argv.(off)) in
  let args = List.map (fun t -> (fst (compile_std t)).tree) rest in
  Printf.printf "hash %s\n" art.hash_hex;
  List.iter (fun (n, h) -> Printf.printf "lock %s %s\n" n h) lock;
  print_result
    (Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~mode ?accel ~program:art.tree args)

let () =
  match Sys.argv.(1) with
  | "eval" ->
      let program, args, fuel, cap = read_corpus Sys.argv.(2) in
      print_result (Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~program args)
  | "compile" -> compile_cmd Sys.argv.(2)
  | "eval-compiled" -> eval_compiled_cmd Sys.argv 2
  | "eval-source" when Array.length Sys.argv > 2 -> eval_source_cmd Sys.argv 2
  | _ ->
      prerr_endline
        "usage: tuna eval <corpus-file> | tuna compile <source-file> | tuna eval-compiled <ternary> [args...] | tuna eval-source <source-file> [arg-terms...]";
      exit 2
