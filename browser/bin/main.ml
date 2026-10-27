(* tuna-js: the engine in the browser.  window.tunaEval(source, args,
   fuel, size_cap) -> result text.  The same Tuna_compiler +
   Tuna_interp pipeline the server runs, so a browser answer and a
   server answer agree by construction.  Build:
   dune build browser/bin/main.bc.js ; test under node by appending
   console.log(tunaEval("(lambda (x) x)", "", 10000, 10000)). *)
open Js_of_ocaml

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

let parse_args s =
  String.split_on_char ' ' (String.trim s)
  |> List.filter (fun t -> t <> "")
  |> List.map (fun t ->
          match Tuna.Canon.of_string t with
          | Ok tree -> tree
          | Error (_, msg) -> failwith ("bad arg " ^ t ^ ": " ^ msg))

let run ~fuel ~size_cap src args_str =
  try
    let art = Tuna_compiler.Bracket.compile_source src in
    let args = parse_args args_str in
    Printf.sprintf "hash %s\nternary %s\n%s" art.hash_hex art.ternary
      (show (Tuna_interp.Eval.eval ~fuel ~size_cap ~program:art.tree args))
  with
  | Tuna_compiler.Ir.Error (p, msg) ->
      "compile error: " ^ Tuna_compiler.Ir.show_error (p, msg)
  | Tuna_compiler.Bracket.Compile_failed msg -> "compile failed: " ^ msg
  | Failure msg -> "error: " ^ msg

let tuna_eval src args fuel cap =
  Js.string (run ~fuel ~size_cap:cap (Js.to_string src) (Js.to_string args))

(* Js.export never materialized the global under node (only the name
   string survived into the bundle) - assign it explicitly. *)
let () =
  Js.Unsafe.set Js.Unsafe.global (Js.string "tunaEval")
    (Js.wrap_callback tuna_eval)