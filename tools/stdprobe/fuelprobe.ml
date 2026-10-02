(* fuel table probe: RUNTIME step counts for the stdlib vocabulary.
   Programs are (lambda (w) ...) so compile-IS-reduction leaves the body
   open; the operand trees arrive at runtime via the args list.  v0 =
   Canonical, v1 = Sharing.  Scratch like stdprobe; the table lands in
   borg/stdlib.borg acceptance 3. *)



let compile ~dictionary src =
  Tuna_compiler.Bracket.compile_source ~fuel:100000000 ~size_cap:100000000
    ~deadline:Float.infinity ~dictionary src

(* --- tree port of the defs, verbatim from stdprobe (source of truth:
   tools/stdprobe/gen_defs.py, mirrored in stdprobe.ml) --- *)
let compiled : (string * Tuna.Tree.t) list ref = ref []
let d name src =
  match compile ~dictionary:!compiled src with
  | art -> compiled := (name, art.Tuna_compiler.Bracket.tree) :: !compiled
  | exception Tuna_compiler.Bracket.Compile_failed m ->
      Printf.ksprintf failwith "DEF %s: %s" name m

(* --- tree builders --- *)
open Tuna.Tree
let bit b = if b then Stem Leaf else Leaf
let rec list_of ts = match ts with [] -> Leaf | t :: ts -> Fork (t, list_of ts)
let mag_ones n = list_of (List.init n (fun _ -> bit true))
let int_pos mag = Fork (Leaf, mag)
let pair a b = Fork (a, b)

let keep label =
  let pats = match Array.to_list Sys.argv with [] | [_] -> [] | _ :: ps -> ps in
  match pats with
  | [] -> true
  | pats -> List.exists (fun p ->
      let ls, lp = String.lowercase_ascii label, String.lowercase_ascii p in
      let rec contains i =
        i + String.length lp <= String.length ls
        && (String.sub ls i (String.length lp) = lp || contains (i + 1))
      in
      contains 0) pats

let measure ?(v0secs = 0.) ?(secs = 0.) label src arg =
  if not (keep label) then () else
  let src = "(lambda (w) " ^ src ^ ")" in
  match compile ~dictionary:!compiled src with
  | exception Tuna_compiler.Bracket.Compile_failed m ->
      Printf.printf "%-40s COMPILE-FAIL %s\n%!" label m
  | art ->
      let program = art.Tuna_compiler.Bracket.tree in
      List.iter
        (fun (mode, tag, persecs) ->
          let deadline =
            if persecs > 0. then Unix.gettimeofday () +. persecs
            else Float.infinity
          in
          match Tuna_interp.Eval.eval ~mode ~deadline ~fuel:100000000
                  ~size_cap:1000000000 ~program [ arg ] with
          | Tuna_interp.Eval.Normal (_, steps) ->
              Printf.printf "%-40s %-5s steps=%d\n%!" label tag steps
          | Tuna_interp.Eval.Fuel_exhausted s ->
              Printf.printf "%-40s %-5s FUEL>=%d\n%!" label tag s
          | Tuna_interp.Eval.Size_exhausted s ->
              Printf.printf "%-40s %-5s SIZE@%d\n%!" label tag s
          | Tuna_interp.Eval.Deadline_exceeded s ->
              Printf.printf "%-40s %-5s DEADLINE@%d\n%!" label tag s
          | Tuna_interp.Eval.Loop s ->
              Printf.printf "%-40s %-5s LOOP@%d\n%!" label tag s)
        [ (Tuna_interp.Eval.Canonical, "v0", if v0secs > 0. then v0secs else secs); (Tuna_interp.Eval.Sharing, "v1", secs) ]

let () =
  List.iter (fun (n, s) -> d n s) Stddefs.all;
  (* tree-case dispatch: one triage rule per shape *)
  measure "tree-case leaf" "((tree-case %10 (lambda (c) %0) (lambda (l) (lambda (r) %0))) w)" Leaf;
  measure "tree-case stem" "((tree-case %10 (lambda (c) %0) (lambda (l) (lambda (r) %0))) w)" (Stem Leaf);
  measure "tree-case fork" "((tree-case %10 (lambda (c) %0) (lambda (l) (lambda (r) %0))) w)" (Fork (Leaf, Leaf));
  (* list-fold n: fold cons (identity reconstruction) *)
  List.iter
    (fun n ->
      let arg = list_of (List.init n (fun _ -> bit true)) in
      measure (Printf.sprintf "list-fold %d" n)
        "(list-fold (lambda (h) (lambda (acc) (pair h acc))) %0 w)" arg)
    [ 10; 100 ];
  (* int-add: all-ones magnitudes, worst-case carry *)
  List.iter
    (fun n ->
      let arg = pair (int_pos (mag_ones n)) (int_pos (mag_ones n)) in
      measure (Printf.sprintf "int-add %d-bit" n)
        "(int-add (first w) (second w))" arg)
    [ 8; 32; 64 ];
  (* int-mul: same; v0 mul blows past 1e8 fuel / the clock (F6), so
     bound v0 rows at 90s and take the v1 numbers straight. *)
  List.iter
    (fun n ->
      let arg = pair (int_pos (mag_ones n)) (int_pos (mag_ones n)) in
      measure ~v0secs:60. ~secs:600. (Printf.sprintf "int-mul %d-bit" n)
        "(int-mul (first w) (second w))" arg)
    [ 8; 16 ]
