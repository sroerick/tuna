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
     tuna run <source-file> [arg-terms...] [--grant PRIM]... [--allow-host H]...
              [--dir D] [--fuel N] [--cap N] [--semantics S]
                                 a journaled run with prims answered by the
                                 local host; seals a derivation record that
                                 tools/deriv-check verifies (borg/local.borg L3)
     tuna repl [--dir D] [--identity N] [--input TERM]... [--fuel N]
               [--cap N] [--semantics S] [--grant PRIM]... [--allow-host H]...
                                 the REPL grammar over a local directory
                                 (borg/local.borg L2/L3): one command per
                                 stdin line, rounds as on /api/repl; eval
                                 and def rounds answer prims under grants

   TYPED I/O (borg/local.borg L4): --in TYPE (repeatable, in order) and
   --out TYPE take the SAME type grammar as POST /api/tool/run, written
   as one JSON value on the command line and read through the same codec
   library (Tuna_codec.Json_codec).  With --in the positional arguments
   are JSON values, not source terms.  Without a type the CLI prints
   ternary, since trees are untyped.

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

(* typed I/O (borg/local.borg L4): --in TYPE / --out TYPE take the
   tool-language T4 type grammar, written as one JSON value on the
   command line, through the SAME codec library POST /api/tool/run
   links (Tuna_codec.Json_codec) - so the CLI adds no second grammar.
   Without a type the CLI keeps printing ternary. *)
module C = Tuna_codec.Json_codec
module J = Yojson.Basic

let codec_type flag s =
  match C.ty_of_json ~path:flag (J.from_string s) with
  | Ok t -> t
  | Error m -> prerr_endline (flag ^ ": " ^ m); exit 2

let codec_value flag ty s =
  (* the flag is the JSON path root, so an error names it as the server
     names inputs[0].value: --in.exit *)
  match C.encode ~path:flag ty (J.from_string s) with
  | Ok t -> t
  | Error m -> prerr_endline m; exit 2

(* the answer through --out, printed as the JSON value /api/tool/run
   answers in its "result" field; a tree that does not have the declared
   shape is a result_error, as on the server (the run still happened) *)
let print_typed_tree out_ty ternary =
  let ty = codec_type "--out" out_ty in
  match Tuna.Canon.of_string ternary with
  | Error (_, m) -> Printf.printf "result_error %s\n" m
  | Ok t -> (
      match C.decode ~path:"result" ty t with
      | Ok v -> Printf.printf "result %s\n" (J.to_string v)
      | Error m -> Printf.printf "result_error %s\n" m)

let print_result_typed out_ty = function
  | Tuna_interp.Eval.Normal (t, _) as r ->
      print_typed_tree out_ty (Tuna.Canon.encode t);
      print_result r
  | r -> print_result r

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

(* the run's inputs: either source TERMS (no --in) or JSON values typed
   by the --in types, one value per type, in order *)
let run_inputs positional ins =
  match ins with
  | [] ->
      let compiled = List.map compile_std positional in
      (List.map (fun (a, _) -> a.Tuna_compiler.Bracket.tree) compiled,
       List.concat_map snd compiled)
  | types ->
      if List.length types <> List.length positional then (
        prerr_endline "input: one JSON value per --in TYPE"; exit 2);
      let types = List.map (codec_type "--in") types in
      (List.map2 (codec_value "--in") types positional, [])

(* run flags shared by eval-compiled, eval-source and run: argv from
   [first] on is positional args plus optional --fuel/--cap/--semantics
   (v0 canonical | v1 distinct-work | fast1-fast3 accelerated) and the
   typed-I/O flags --in TYPE / --out TYPE (L4). *)
let parse_run_flags argv first =
  let fuel = ref 10000 and cap = ref 10000 and rest = ref [] in
  let mode = ref Tuna_interp.Eval.Canonical in
  let accel = ref None in
  let ins = ref [] and out = ref None in
  let rec go i =
    if i >= Array.length argv then ()
    else
      match argv.(i) with
      | "--fuel" -> fuel := int_of_string argv.(i + 1); go (i + 2)
      | "--cap" -> cap := int_of_string argv.(i + 1); go (i + 2)
      | "--in" -> ins := argv.(i + 1) :: !ins; go (i + 2)
      | "--out" -> out := Some argv.(i + 1); go (i + 2)
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
  (!fuel, !cap, !mode, !accel, List.rev !ins, !out, List.rev !rest)

let eval_compiled_cmd argv off =
  (* argv.(off) = program ternary, then args and run flags *)
  let fuel, cap, mode, accel, ins, out, rest = parse_run_flags argv (off + 1) in
  let program =
    match Tuna.Canon.of_string argv.(off) with
    | Ok p -> p
    | Error (off', msg) ->
        prerr_endline (Printf.sprintf "program parse error at %d: %s" off' msg);
        exit 2
  in
  let args =
    match ins with
    | [] -> List.map Tuna.Canon.parse rest
    | _ -> fst (run_inputs rest ins)
  in
  let result =
    Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~mode ?accel ~program args
  in
  match out with
  | None -> print_result result
  | Some t -> print_result_typed t result

let eval_source_cmd argv off =
  (* argv.(off) = source file; each positional arg is either a source
     TERM compiled against the same dictionary (a raw tree is
     %<ternary>) or, with --in, a JSON value typed by those types *)
  let fuel, cap, mode, accel, ins, out, rest = parse_run_flags argv (off + 1) in
  let art, lock = compile_std (read_file argv.(off)) in
  let args, arg_lock = run_inputs rest ins in
  (* one lock for the whole run: program and arg terms *)
  let lock = List.sort_uniq compare (lock @ arg_lock) in
  Printf.printf "hash %s\n" art.hash_hex;
  List.iter (fun (n, h) -> Printf.printf "lock %s %s\n" n h) lock;
  let result =
    Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~mode ?accel ~program:art.tree args
  in
  match out with
  | None -> print_result result
  | Some t -> print_result_typed t result

(* tuna run: compile against std, run with the local prim host, print
   the outcome and the journal, seal the record into the local store and
   print where it is.  Exit 0 on any finished run (exhaustion is a
   normal result); 1 on a compile error; 2 on bad usage. *)
let run_cmd argv off =
  let dir = ref None and grants = ref [] and hosts = ref [] and rest = ref [] in
  let value i =
    if i + 1 < Array.length argv then argv.(i + 1)
    else (prerr_endline ("run: " ^ argv.(i) ^ " needs a value"); exit 2)
  in
  let rec go i =
    if i < Array.length argv then
      match argv.(i) with
      | "--dir" -> dir := Some (value i); go (i + 2)
      | "--grant" -> grants := value i :: !grants; go (i + 2)
      | "--allow-host" -> hosts := value i :: !hosts; go (i + 2)
      | ("--fuel" | "--cap" | "--semantics" | "--in" | "--out") as f ->
          ignore (value i); rest := argv.(i + 1) :: f :: !rest; go (i + 2)
      | a -> rest := a :: !rest; go (i + 1)
  in
  go (off + 1);
  let flags = Array.of_list ("tuna" :: List.rev !rest) in
  let fuel, cap, _mode, _accel, ins, out, positional = parse_run_flags flags 1 in
  let semantics =
    let rec find = function
      | "--semantics" :: s :: _ -> s
      | _ :: r -> find r
      | [] -> "v0"
    in
    find (Array.to_list flags)
  in
  if not (Tuna_local.Local_run.semantics_ok semantics) then (
    prerr_endline ("run: unknown semantics " ^ semantics); exit 2);
  let st =
    try
      Tuna_local.Local_store.open_
        (match !dir with Some d -> d | None -> Tuna_local.Local_store.default_root ())
    with Failure msg | Sys_error msg -> prerr_endline ("run: " ^ msg); exit 2
  in
  let art, lock = compile_std (read_file argv.(off)) in
  let inputs, arg_lock = run_inputs positional ins in
  let lock = List.sort_uniq compare (lock @ arg_lock) in
  let cfg =
    { Tuna_local.Local_run.grants = List.rev !grants; allow_hosts = List.rev !hosts;
      semantics; fuel; size_cap = cap }
  in
  let r =
    Tuna_local.Local_run.execute st cfg ~tags:art.Tuna_compiler.Bracket.tags
      ~program:art.Tuna_compiler.Bracket.tree inputs
  in
  let d = r.Tuna_local.Local_run.record in
  let module D = Tuna_deriv.Deriv in
  Printf.printf "run %s\nprogram %s\n" d.D.d_run_id d.D.d_program_hash;
  List.iter (fun (n, h) -> Printf.printf "lock %s %s\n" n h) lock;
  List.iter
    (fun (j : D.Journal.t) ->
      Printf.printf "journal %d %s %s %s\n" j.D.Journal.j_seq j.D.Journal.j_prim
        (if j.D.Journal.j_callsite_path = "" then "-" else j.D.Journal.j_callsite_path)
        (match (j.D.Journal.j_result_hash, j.D.Journal.j_error) with
         | Some h, _ -> "ok " ^ h
         | None, Some e -> "error " ^ e
         | None, None -> "-"))
    d.D.d_journal;
  (match (out, d.D.d_result_ternary) with
   | Some t, Some ternary -> print_typed_tree t ternary
   | _ -> ());
  Printf.printf "%s %s %d\n" (D.status_to_string d.D.d_status)
    (Option.value d.D.d_result_ternary ~default:"-") d.D.d_step_count;
  Printf.printf "record %s\n"
    (Filename.concat (Filename.concat st.Tuna_local.Local_store.root "bytes")
       r.Tuna_local.Local_run.record_hash);
  exit 0

(* tuna repl: one round per stdin line against the local store.  A
   failed round prints "error <msg>" and the session goes on; the exit
   status is 1 if any round failed, so a piped transcript is a check.
   Eval and def rounds answer prims under --grant/--allow-host, as
   `tuna run` does, and seal no record, so L2's op log is unchanged. *)
let repl_cmd argv off =
  let dir = ref None and identity = ref None and inputs = ref [] in
  let grants = ref [] and hosts = ref [] in
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
      | "--grant" -> grants := value i :: !grants; go (i + 2)
      | "--allow-host" -> hosts := value i :: !hosts; go (i + 2)
      | ("--fuel" | "--cap" | "--semantics") as f ->
          ignore (value i); rest := argv.(i + 1) :: f :: !rest; go (i + 2)
      | a -> rest := a :: !rest; go (i + 1)
  in
  go off;
  let flags = Array.of_list ("tuna" :: List.rev !rest) in
  let fuel, cap, _mode, _accel, _ins, _out, positional = parse_run_flags flags 1 in
  let semantics =
    let rec find = function
      | "--semantics" :: s :: _ -> s
      | _ :: r -> find r
      | [] -> "v0"
    in
    find (Array.to_list flags)
  in
  if not (Tuna_local.Local_run.semantics_ok semantics) then (
    prerr_endline ("repl: unknown semantics " ^ semantics); exit 2);
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
  let opts =
    { Tuna_local.Local_repl.fuel; size_cap = cap; semantics;
      grants = List.rev !grants; allow_hosts = List.rev !hosts; inputs }
  in
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
  | "run" when Array.length Sys.argv > 2 -> run_cmd Sys.argv 2
  | _ ->
      prerr_endline
        "usage: tuna eval <corpus-file> | tuna compile <source-file> | tuna eval-compiled <ternary> [args...] | tuna eval-source <source-file> [arg-terms...] [--in TYPE JSON]... [--out TYPE] | tuna run <source-file> [--grant P]... | tuna repl [--dir D] [--grant P]... [--allow-host H]...";
      exit 2
