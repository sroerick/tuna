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
     tuna repl [--dir D] [--identity N] [--input TERM]... [--fuel N]
               [--cap N] [--semantics S]
                                 the REPL grammar over a local directory
                                 (borg/local.borg L2): one command per
                                 stdin line, rounds as on /api/repl

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
  let compiled = List.map compile_std rest in
  let args = List.map (fun (a, _) -> a.Tuna_compiler.Bracket.tree) compiled in
  (* one lock for the whole run: program and arg terms *)
  let lock = List.sort_uniq compare (lock @ List.concat_map snd compiled) in
  Printf.printf "hash %s\n" art.hash_hex;
  List.iter (fun (n, h) -> Printf.printf "lock %s %s\n" n h) lock;
  print_result
    (Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~mode ?accel ~program:art.tree args)

(* tuna repl: one round per stdin line against the local store.  A
   failed round prints "error <msg>" and the session goes on; the exit
   status is 1 if any round failed, so a piped transcript is a check. *)
let repl_cmd argv off =
  let dir = ref None and identity = ref None and inputs = ref [] in
  let rest = ref [] in
  let value i =
    if i + 1 < Array.length argv then argv.(i + 1)
    else (prerr_endline ("repl: " ^ argv.(i) ^ " needs a value"); exit 2)
  in
  let rec go i =
    if i < Array.length argv then
      match argv.(i) with
      | "--dir" -> dir := Some (value i); go (i + 2)
      | "--identity" -> identity := Some (value i); go (i + 2)
      | "--input" -> inputs := value i :: !inputs; go (i + 2)
      | ("--fuel" | "--cap" | "--semantics") as f ->
          ignore (value i); rest := argv.(i + 1) :: f :: !rest; go (i + 2)
      | a -> rest := a :: !rest; go (i + 1)
  in
  go off;
  let flags = Array.of_list ("tuna" :: List.rev !rest) in
  let fuel, cap, mode, accel, positional = parse_run_flags flags 1 in
  if positional <> [] then (
    prerr_endline ("repl: unexpected argument " ^ List.hd positional);
    exit 2);
  (* the server REPL's run defaults when no flag is given *)
  let has f = Array.exists (( = ) f) flags in
  let fuel = if has "--fuel" then fuel else 1_000_000 in
  let cap = if has "--cap" then cap else 1_000_000 in
  let st =
    try
      let root =
        match !dir with
        | Some d -> d
        | None -> Tuna_local.Local_store.default_root ()
      in
      Tuna_local.Local_store.open_ ?identity:!identity root
    with Failure msg | Sys_error msg -> prerr_endline ("repl: " ^ msg); exit 2
  in
  let inputs = List.rev_map (fun t -> (fst (compile_std t)).tree) !inputs in
  let opts = { Tuna_local.Local_repl.fuel; size_cap = cap; mode; accel; inputs } in
  let tty = Unix.isatty Unix.stdin in
  let failed = ref false in
  let rec loop () =
    if tty then (print_string "tuna> "; flush stdout);
    match input_line stdin with
    | exception End_of_file -> if tty then print_newline ()
    | line ->
        let l = String.trim line in
        if l <> "" && l.[0] <> ';' then (
          (match Tuna_local.Local_repl.execute st opts l with
           | Ok o -> print_string (Tuna_local.Local_repl.render o)
           | Error msg -> failed := true; print_endline ("error " ^ msg));
          print_newline ();
          flush stdout);
        loop ()
  in
  loop ();
  exit (if !failed then 1 else 0)

let () =
  match Sys.argv.(1) with
  | "eval" ->
      let program, args, fuel, cap = read_corpus Sys.argv.(2) in
      print_result (Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~program args)
  | "compile" -> compile_cmd Sys.argv.(2)
  | "eval-compiled" -> eval_compiled_cmd Sys.argv 2
  | "eval-source" when Array.length Sys.argv > 2 -> eval_source_cmd Sys.argv 2
  | "repl" -> repl_cmd Sys.argv 2
  | _ ->
      prerr_endline
        "usage: tuna eval <corpus-file> | tuna compile <source-file> | tuna eval-compiled <ternary> [args...] | tuna eval-source <source-file> [arg-terms...] | tuna repl [--dir D]";
      exit 2
