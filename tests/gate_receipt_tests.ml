(* Gate receipts, step G1 (borg/gate-receipts.borg): the verdict
   fixtures and the host-side log adapters.

   The verdict program is scripts/gate-receipts/verdict.sabra: prim-free,
   self-contained, compiled once.  These tests build observation records
   in OCaml with the same encoding the wrapper writes (small-nat field
   tags, cstr strings, law-5 ints, counts [run passed failed skipped] or
   the atom %10 for unparsed) and evaluate the compiled tree directly -
   no Postgres, no HTTP.

   The adapter tests run scripts/gate-receipts/adapt.sh on fixture logs.
   They pin the two fail-open shapes the chapter was written for:
   alcotest [OK] cases that print SKIP, and the old differential rule's
   misleading "sbcl missing" sentence after a real 93/93 failure. *)

module T = Tuna.Tree
module Canon = Tuna.Canon
module Cstr = Tuna.Cstr
module Int_enc = Tuna.Int_enc
module Bracket = Tuna_compiler.Bracket

let read_all path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_all path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* -- the observation record encoding (chapter observation-record) ----- *)

type counts = Unparsed | Counts of int * int * int * int

type obs = {
  gate : string;
  project : string;
  command : string;
  rev : string;
  tree_clean : int;
  diff_hash : string;
  exit : int;
  log_hash : string;
  counts : counts;
  adapter : string;
  runner : string;
  started_at : string;
}

let base =
  {
    gate = "tuna/runtest";
    project = "tuna";
    command = "make -C tuna test";
    rev = String.make 40 'a';
    tree_clean = 1;
    diff_hash = "";
    exit = 0;
    log_hash = String.make 64 'b';
    counts = Counts (93, 93, 0, 0);
    adapter = "alcotest-v1";
    runner = "runner";
    started_at = "2026-11-24T00:00:00Z";
  }

(* small-nat tag: tag n = Stem^n Leaf *)
let tag n =
  let rec go = function 0 -> T.Leaf | k -> T.Stem (go (k - 1)) in
  go n

let int_lit n = Int_enc.positive_of_decimal (string_of_int n)
let str s = Cstr.encode s
let pair a b = T.Fork (a, b)
let rec lst = function [] -> T.Leaf | x :: xs -> pair x (lst xs)

(* one record entry: [key value] = pair key (pair value 0) *)
let entry k v = pair (tag k) (pair v T.Leaf)

let counts_tree = function
  | Unparsed -> tag 1
  | Counts (run, passed, failed, skipped) ->
      lst [ int_lit run; int_lit passed; int_lit failed; int_lit skipped ]

let observation o =
  lst
    [ entry 1 (str o.gate);
      entry 2 (str o.project);
      entry 3 (str o.command);
      entry 4 (str o.rev);
      entry 5 (int_lit o.tree_clean);
      entry 6 (str o.diff_hash);
      entry 7 (int_lit o.exit);
      entry 8 (str o.log_hash);
      entry 9 (counts_tree o.counts);
      entry 10 (str o.adapter);
      entry 11 (str o.runner);
      entry 12 (str o.started_at) ]

let skip_allowed slugs = lst (List.map str slugs)

(* -- the verdict program ---------------------------------------------- *)

let verdict_source = read_all "../scripts/gate-receipts/verdict.sabra"
let verdict = lazy (Bracket.compile_source verdict_source)

let pass = "10"
let fail = "110"
let unverified = "1110"

let prim_calls = ref 0

let verdict_of ?(skip = []) o =
  prim_calls := 0;
  let host ~site:_ ~name ~args:_ =
    incr prim_calls;
    `Error ("verdict called prim " ^ name)
  in
  let art = Lazy.force verdict in
  match
    Tuna_interp.Eval.eval ~host ~fuel:50_000_000 ~size_cap:1_000_000
      ~program:art.Bracket.tree [ observation o; skip_allowed skip ]
  with
  | Tuna_interp.Eval.Normal (t, _) ->
      Alcotest.(check int) "prim-free (no host call)" 0 !prim_calls;
      Canon.encode t
  | Tuna_interp.Eval.Fuel_exhausted n ->
      Alcotest.failf "verdict exhausted fuel at %d steps" n
  | Tuna_interp.Eval.Size_exhausted n ->
      Alcotest.failf "verdict exhausted size at %d steps" n
  | Tuna_interp.Eval.Loop n -> Alcotest.failf "verdict looped at %d steps" n
  | Tuna_interp.Eval.Deadline_exceeded n ->
      Alcotest.failf "verdict hit a deadline at %d steps" n

let expect name want o =
  Alcotest.(check string) name want (verdict_of o)

(* -- fixtures: the chapter's verdict verify line ---------------------- *)

let fixture_zero_run () =
  expect "exit 0, counts [0 0 0 0]" unverified
    { base with counts = Counts (0, 0, 0, 0) }

let fixture_pp_skip_shape () =
  expect "pp SKIP shape is unverified" unverified
    { base with gate = "pp/data-spaces"; counts = Counts (15, 15, 0, 15) }

let fixture_pp_skip_allowed () =
  Alcotest.(check string) "pp SKIP is pass when skip-allowed" pass
    (verdict_of ~skip:[ "pp/data-spaces" ]
       { base with gate = "pp/data-spaces"; counts = Counts (15, 15, 0, 15) })

let fixture_pass () =
  expect "exit 0, counts [93 93 0 0]" pass
    { base with counts = Counts (93, 93, 0, 0) }

let fixture_exit_fail () = expect "exit 1" fail { base with exit = 1 }

let fixture_unparsed () =
  expect "unparsed counts" unverified { base with counts = Unparsed }

let fixture_order_exit_first () =
  expect "exit nonzero outranks unparsed" fail
    { base with exit = 1; counts = Unparsed }

let fixture_failed () =
  expect "failed nonzero is fail" fail
    { base with counts = Counts (5, 4, 1, 0) }

let fixture_skip_cannot_excuse_failure () =
  Alcotest.(check string) "skip-allowed does not excuse a failure" fail
    (verdict_of ~skip:[ base.gate ]
       { base with counts = Counts (5, 4, 1, 1) })

let fixture_dirty_no_diff () =
  expect "dirty tree with no diff hash is unverified" unverified
    { base with tree_clean = 0; diff_hash = "" }

let fixture_dirty_with_diff () =
  expect "dirty tree with a diff hash is pass" pass
    { base with tree_clean = 0; diff_hash = "deadbeef" }

(* -- adapters --------------------------------------------------------- *)

let run_adapter adapter log =
  let logf = Filename.temp_file "gate-receipt-log" ".log" in
  let outf = logf ^ ".out" in
  let errf = logf ^ ".err" in
  write_all logf log;
  let cmd =
    Printf.sprintf "bash ../scripts/gate-receipts/adapt.sh %s %s >%s 2>%s"
      (Filename.quote adapter) (Filename.quote logf) (Filename.quote outf)
      (Filename.quote errf)
  in
  let rc = Sys.command cmd in
  let out = String.trim (read_all outf) in
  let err = read_all errf in
  Sys.remove logf;
  Sys.remove outf;
  Sys.remove errf;
  Alcotest.(check int) "adapter exits 0" 0 rc;
  if out = "" then Alcotest.failf "adapter wrote nothing (stderr: %s)" err;
  out

let counts_of_string s =
  match String.split_on_char ' ' s with
  | [ a; b; c; d ] ->
      Counts (int_of_string a, int_of_string b, int_of_string c, int_of_string d)
  | _ -> Unparsed

let pp_log =
  let b = Buffer.create 1024 in
  Buffer.add_string b "Testing `data_spaces'.\nThis run has ID `X'.\n\n";
  for i = 1 to 15 do
    Buffer.add_string b
      (Printf.sprintf "  [OK]          spaces  %d   a case.\n" i)
  done;
  for _ = 1 to 15 do
    Buffer.add_string b "SKIP data_spaces: postgres unreachable\n"
  done;
  Buffer.add_string b "Test Successful in 0.001s. 15 tests run.\n";
  Buffer.contents b

let alcotest_fail_log =
  "Testing `x'.\n\n  [FAIL] x 0 a case.\n  [OK] x 1 b case.\n\
   \  [OK] x 2 c case.\n  [OK] x 3 d case.\n\n\
   1 failure! in 0.002s. 3 tests run.\n"

let diff_ok_log =
  "differential: all 93 corpus entries agree (refeval == CL twin == tuna CLI)\n"

let diff_fail_log = "differential: 93/93 corpus entries FAILED\n"

let diff_old_bug_log =
  "FAIL succ_zero (scripts/diff-corpus/succ_zero.corpus)\n\
   differential: 93/93 corpus entries FAILED\n\
   sbcl missing: differential harness skipped\n"

let diff_skip_log = "sbcl missing: differential harness skipped\n"

let alcotest_counts () =
  Alcotest.(check string) "pp shape (SKIP lines count as skips)"
    "15 15 0 15" (run_adapter "alcotest" pp_log)

let alcotest_failure_counts () =
  Alcotest.(check string) "failure summary: run = passes + failures"
    "4 3 1 0" (run_adapter "alcotest" alcotest_fail_log)

let differential_counts () =
  Alcotest.(check string) "all agree" "93 93 0 0"
    (run_adapter "differential" diff_ok_log);
  Alcotest.(check string) "F/N FAILED" "93 0 93 0"
    (run_adapter "differential" diff_fail_log);
  Alcotest.(check string) "FAILED outranks the misleading skip sentence"
    "93 0 93 0" (run_adapter "differential" diff_old_bug_log);
  Alcotest.(check string) "sbcl missing alone is run 0" "0 0 0 0"
    (run_adapter "differential" diff_skip_log)

let make_test_aggregate () =
  Alcotest.(check string) "sums every suite"
    "108 108 0 15"
    (run_adapter "make-test" (pp_log ^ diff_ok_log));
  Alcotest.(check string) "a skipped suite makes the gate unparsed" "unparsed"
    (run_adapter "make-test"
       ("store tests skipped (postgres down; scripts/dev.sh start-pg)\n"
       ^ diff_ok_log));
  Alcotest.(check string) "the old bug's real failure is still fail"
    "93 0 93 0" (run_adapter "make-test" diff_old_bug_log)

(* alcotest colourises its summary even when the log is a file; a green
   suite must still parse.  The wrapper's log carries the escape bytes. *)
let colored_log =
  "\027[32mTest Successful\027[0m in 1.200s. 3 tests run.\n"

let colored_counts () =
  Alcotest.(check string) "ANSI colour does not hide a green suite"
    "3 3 0 0" (run_adapter "make-test" colored_log)

(* python -m unittest: a first-class harness in the factory's gates. *)
let unittest_ok_log =
  "............\n"
  ^ "----------------------------------------------------------------------\n"
  ^ "Ran 12 tests in 0.004s\n\nOK\n"

let unittest_failed_log =
  "Ran 4 tests in 0.002s\n\nFAILED (failures=1, errors=1)\n"

let unittest_skipped_log = "Ran 5 tests in 0.001s\n\nOK (skipped=2)\n"

(* stock 3.14 emits this for a suite that did not do what it said; it is
   a FAILING suite, and counting it as pass is the fail-open the review
   measured. *)
let unittest_unexpected_log =
  "Ran 1 test in 0.000s\n\nFAILED (unexpected successes=1)\n"

let unittest_no_summary_log = "Ran 4 tests in 0.1s\n"

let unittest_counts () =
  Alcotest.(check string) "OK: every test passed" "12 12 0 0"
    (run_adapter "unittest" unittest_ok_log);
  Alcotest.(check string) "FAILED: failures and errors count" "4 2 2 0"
    (run_adapter "unittest" unittest_failed_log);
  Alcotest.(check string) "OK (skipped=N)" "5 3 0 2"
    (run_adapter "unittest" unittest_skipped_log);
  Alcotest.(check string) "unexpected successes is a failure" "1 0 1 0"
    (run_adapter "unittest" unittest_unexpected_log);
  Alcotest.(check string) "Ran without OK/FAILED is unparsed" "unparsed"
    (run_adapter "unittest" unittest_no_summary_log);
  Alcotest.(check string) "the aggregate reads the same shapes" "16 14 2 0"
    (run_adapter "make-test" (unittest_ok_log ^ unittest_failed_log));
  Alcotest.(check string) "a coloured alcotest summary still parses" "3 3 0 0"
    (run_adapter "alcotest" colored_log)

(* a harness without a summary shape declares its counts explicitly; the
   aggregate adapter sums the line like any other suite summary. *)
let counts_line_log = "COUNTS run=12 passed=11 failed=0 skipped=1\n"

let counts_line () =
  Alcotest.(check string) "an explicit COUNTS line is a suite summary"
    "12 11 0 1" (run_adapter "make-test" counts_line_log);
  Alcotest.(check string) "an inconsistent COUNTS line is not a summary"
    "unparsed"
    (run_adapter "make-test" "COUNTS run=5 passed=1 failed=0 skipped=0\n")

let pp_shape_end_to_end () =
  let c = counts_of_string (run_adapter "alcotest" pp_log) in
  Alcotest.(check string) "pp shape verdict" unverified
    (verdict_of { base with gate = "pp/data-spaces"; counts = c })

let differential_end_to_end () =
  let c = counts_of_string (run_adapter "differential" diff_ok_log) in
  Alcotest.(check string) "differential green verdict" pass
    (verdict_of { base with gate = "tuna/differential"; counts = c })

(* -- the shell encoder must write the same record these tests build --- *)

let run_encoder mode envs =
  let outf = Filename.temp_file "gate-receipt-enc" ".out" in
  let assigns =
    String.concat " "
      (List.map (fun (k, v) -> k ^ "=" ^ Filename.quote v) envs)
  in
  let cmd =
    Printf.sprintf "env %s bash ../scripts/gate-receipts/encode.sh %s >%s"
      assigns (Filename.quote mode) (Filename.quote outf)
  in
  let rc = Sys.command cmd in
  let out = String.trim (read_all outf) in
  Sys.remove outf;
  Alcotest.(check int) "encoder exits 0" 0 rc;
  out

let obs_env o =
  [ ("G_GATE", o.gate);
    ("G_PROJECT", o.project);
    ("G_COMMAND", o.command);
    ("G_REV", o.rev);
    ("G_TREE_CLEAN", string_of_int o.tree_clean);
    ("G_DIFF_HASH", o.diff_hash);
    ("G_EXIT", string_of_int o.exit);
    ("G_LOG_HASH", o.log_hash);
    ( "G_COUNTS",
      match o.counts with
      | Unparsed -> "unparsed"
      | Counts (a, b, c, d) ->
          Printf.sprintf "%d %d %d %d" a b c d );
    ("G_ADAPTER", o.adapter);
    ("G_RUNNER", o.runner);
    ("G_STARTED_AT", o.started_at) ]

let encoder_matches_ocaml () =
  let o =
    { base with
      gate = "tuna/runtest";
      command = "make -C /tmp/tuna test";
      counts = Counts (108, 93, 0, 15) }
  in
  Alcotest.(check string) "observation record ternary" (Canon.encode (observation o))
    (run_encoder "observation" (obs_env o));
  Alcotest.(check string) "skip-allowed list ternary"
    (Canon.encode (skip_allowed [ "pp/data-spaces"; "tuna/differential" ]))
    (run_encoder "skip"
       [ ("G_SKIP", "pp/data-spaces\ntuna/differential") ]);
  Alcotest.(check string) "empty skip list is the leaf" "0"
    (run_encoder "skip" [ ("G_SKIP", "") ]);
  let u = { base with counts = Unparsed } in
  Alcotest.(check string) "unparsed counts ternary" (Canon.encode (observation u))
    (run_encoder "observation" (obs_env u))

(* every field is inside the cited input: any change moves the tree and
   the hash, and a policy field moves the verdict too. *)
let field_binding () =
  let changes =
    [ ("gate", { base with gate = "pp/data-spaces" });
      ("project", { base with project = "pp" });
      ("command", { base with command = "make -C pp test" });
      ("rev", { base with rev = String.make 40 'c' });
      ("tree_clean", { base with tree_clean = 0 });
      ("diff_hash", { base with diff_hash = "deadbeef" });
      ("exit", { base with exit = 1 });
      ("log_hash", { base with log_hash = String.make 64 'd' });
      ("counts", { base with counts = Counts (93, 92, 1, 0) });
      ("counts unparsed", { base with counts = Unparsed });
      ("adapter", { base with adapter = "none" });
      ("runner", { base with runner = "verifier" });
      ("started_at", { base with started_at = "2026-11-25T00:00:00Z" }) ]
  in
  let base_tree = Canon.encode (observation base) in
  List.iter
    (fun (name, o) ->
      Alcotest.(check bool)
        (name ^ " changes the observation tree") true
        (Canon.encode (observation o) <> base_tree))
    changes;
  Alcotest.(check string) "the unchanged base is pass" pass (verdict_of base);
  Alcotest.(check string) "exit 0 -> 1 moves the verdict to fail" fail
    (verdict_of { base with exit = 1 })



let () =
  Alcotest.run "gate-receipts"
    [ ( "verdict-fixtures"
      , [ Alcotest.test_case "exit 0, counts [0 0 0 0] -> unverified" `Quick
            fixture_zero_run
        ; Alcotest.test_case "pp SKIP shape -> unverified" `Quick
            fixture_pp_skip_shape
        ; Alcotest.test_case "pp SKIP shape + skip-allowed -> pass" `Quick
            fixture_pp_skip_allowed
        ; Alcotest.test_case "exit 0, counts [93 93 0 0] -> pass" `Quick
            fixture_pass
        ; Alcotest.test_case "exit 1 -> fail" `Quick fixture_exit_fail
        ; Alcotest.test_case "unparsed counts -> unverified" `Quick
            fixture_unparsed
        ; Alcotest.test_case "policy order: exit outranks unparsed" `Quick
            fixture_order_exit_first
        ; Alcotest.test_case "failed nonzero -> fail" `Quick fixture_failed
        ; Alcotest.test_case "skip-allowed cannot excuse failure" `Quick
            fixture_skip_cannot_excuse_failure
        ; Alcotest.test_case "dirty tree, no diff -> unverified" `Quick
            fixture_dirty_no_diff
        ; Alcotest.test_case "dirty tree, diff present -> pass" `Quick
            fixture_dirty_with_diff ] )
    ; ( "adapters"
      , [ Alcotest.test_case "alcotest: pp SKIP shape" `Quick alcotest_counts
        ; Alcotest.test_case "alcotest: failure summary" `Quick
            alcotest_failure_counts
        ; Alcotest.test_case "differential: agree / FAILED / old bug / skip"
            `Quick differential_counts
        ; Alcotest.test_case "make-test: sums suites, skips are unparsed"
            `Quick make_test_aggregate
        ; Alcotest.test_case "alcotest: ANSI colour is stripped" `Quick
            colored_counts
        ; Alcotest.test_case "unittest: OK / FAILED / skipped" `Quick
            unittest_counts
        ; Alcotest.test_case "make-test: an explicit COUNTS line" `Quick
            counts_line
        ; Alcotest.test_case "pp shape end to end -> unverified" `Quick
            pp_shape_end_to_end
        ; Alcotest.test_case "differential green end to end -> pass" `Quick
            differential_end_to_end ] )
    ; ("encoders", [ Alcotest.test_case "shell == OCaml observation encode" `Quick
                       encoder_matches_ocaml
                   ; Alcotest.test_case "every field is inside the cited input"
                       `Quick field_binding ]) ]
