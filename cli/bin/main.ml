(* Tuna CLI: thin terminal clients.
   Usage: tuna eval <corpus-file> — evaluate a corpus entry (program +
   args under fuel/size_cap) and print "<status> <result-or--> <steps>". *)

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

let () =
  match Sys.argv.(1) with
  | "eval" ->
      let program, args, fuel, cap = read_corpus Sys.argv.(2) in
      (match Tuna_interp.Eval.eval ~fuel ~size_cap:cap ~program args with
       | Normal (t, steps) -> Printf.printf "normal %s %d\n" (Tuna.Canon.encode t) steps
       | Fuel_exhausted steps -> Printf.printf "fuel_exhausted - %d\n" steps
       | Size_exhausted steps -> Printf.printf "size_exhausted - %d\n" steps)
  | _ ->
      prerr_endline "usage: tuna eval <corpus-file>";
      exit 2
