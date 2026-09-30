(* deriv-check: the standalone offline verifier for tuna derivation
   records (borg/deriv.borg).

   Usage: deriv-check [record.json|-]
   A missing argument reads stdin (pipes).

   Exit codes: 0 = VERIFIED, 1 = FAILED, 2 = unparseable input.
   The verifier needs only tuna_deriv: no server, no Postgres, no
   network. It answers one question: does this record re-derive the
   value it claims? *)

let read_all = function
  | Some path ->
      let ic = open_in_bin path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic; s
  | None ->
      let buf = Buffer.create 4096 in
      (try
         while true do
           Buffer.add_channel buf stdin 4096
         done
       with End_of_file -> ());
      Buffer.contents buf

module D = Tuna_deriv.Deriv

let () =
  let arg =
    match Sys.argv with
    | [| _ |] -> None
    | [| _; a |] -> if a = "-" || a = "" then None else Some a
    | _ -> Printf.eprintf "usage: deriv-check [record.json|-]\n"; exit 2
  in
  let src = read_all arg in
  match D.of_string src with
  | Error e ->
      Printf.printf "unparseable: %s\n" e;
      exit 2
  | Ok d ->
      (match D.verify d with
       | D.Verified ->
           Printf.printf "VERIFIED %s\n" d.D.d_deriv_id;
           Printf.printf "  run      %s (%s, %d steps)\n" d.D.d_run_id
             d.D.d_semantics d.D.d_step_count;
           Printf.printf "  program  %s\n" d.D.d_program_hash;
           Printf.printf "  inputs   %d\n" (List.length d.D.d_input_ternaries);
           Printf.printf "  journal  %d rows\n" (List.length d.D.d_journal);
           (match d.D.d_result_hash with
            | Some h -> Printf.printf "  result   %s\n" h
            | None ->
                Printf.printf "  result   none (%s)\n"
                  (D.status_to_string d.D.d_status));
           exit 0
       | D.Failed msg ->
           Printf.printf "FAILED %s\n%s\n" d.D.d_deriv_id msg;
           exit 1)
