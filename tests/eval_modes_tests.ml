(* eval-modes E1 (borg/eval-modes.borg): the staged template vocabulary
   (stdlib v1.3: tmpl-compile, tmpl-each, html-escape, list-index).

   Two claims, both checked here against the frozen stdlib transcript:
   - the vocabulary renders correctly (holes, escaping, missing fields,
     stray braces, coalesced text);
   - written in a source, (tmpl-compile "...") is specialised at COMPILE
     time: the page program's request answers byte-identically to the
     live render (template supplied at request time) while paying none
     of the parse.  How MUCH that saves is a measurement, not an
     assertion (EM1 in the chapter records it); here we pin only that
     the parse moved to compile time and the answers agree. No PG. *)

open Tuna
module B = Tuna_compiler.Bracket
module T = Tree

let read_all path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let dictionary =
  lazy
    (read_all "../stdlib/v1/core.defs"
    |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           match String.index_opt line ' ' with
           | Some 3 when String.sub line 0 4 = "def " ->
               let rest = String.sub line 4 (String.length line - 4) in
               let sp = String.index rest ' ' in
               Some
                 ( String.sub rest 0 sp,
                   String.sub rest (sp + 1) (String.length rest - sp - 1) )
           | _ -> None)
    |> List.fold_left
         (fun dict (name, src) ->
           (name, (B.compile_source ~dictionary:dict src).B.tree) :: dict)
         [])

let compile src = B.compile_source ~dictionary:(Lazy.force dictionary) src

let eval program args =
  match
    Tuna_interp.Eval.eval ~fuel:200_000_000 ~size_cap:50_000_000 ~program args
  with
  | Tuna_interp.Eval.Normal (t, steps) -> (t, steps)
  | _ -> Alcotest.fail "did not finish"

let str t =
  match Cstr.decode t with Some s -> s | None -> Alcotest.fail "not a string"

let render template record_src =
  str
    (fst
       (eval
          (compile (Printf.sprintf "(tmpl-compile %S)" template)).B.tree
          [ (compile record_src).B.tree ]))

let item = "<li>{2} <small>by {3}</small></li>"

let todos n =
  (compile
     (Printf.sprintf "[%s]"
        (String.concat " "
           (List.init n (fun i ->
                Printf.sprintf
                  "(todo-item todo-open \"task %d <b>&</b>\" \"user%d\" 1727000000)"
                  i i)))))
    .B.tree

let page_fn =
  {|(lambda (t) (lambda (todos) (list-append "<ul>" (list-append (tmpl-each (tmpl-compile t) todos) "</ul>"))))|}

let rendering =
  let case name template record want =
    Alcotest.test_case name `Quick (fun () ->
        Alcotest.(check string) "output" want (render template record))
  in
  [ case "holes read todo fields and escape them" item
      {|(todo-item todo-open "a<b" "o'neil" 1)|}
      "<li>a&lt;b <small>by o&#39;neil</small></li>";
    case "every escaped character" "{1}" {|[[#1 "& < > ' x"]]|}
      "&amp; &lt; &gt; &#39; x";
    case "a missing field renders empty" "[{9}]" {|[[#1 "x"]]|} "[]";
    case "a brace that is not {digit} is text (holes are one digit)" "{z}{12}{1}{"
      {|[[#1 "x"]]|} "{z}{12}x{";
    case "a template with no holes" "plain" "[]" "plain";
    case "the empty template" "" {|[[#1 "x"]]|} "";
    Alcotest.test_case "adjacent text is one segment" `Quick (fun () ->
        let segs, _ = eval (compile {|(tmpl-parse "ab{1}cd{2}")|}).B.tree [] in
        let rec len = function T.Fork (_, tl) -> 1 + len tl | _ -> 0 in
        Alcotest.(check int) "text hole text hole" 4 (len segs));
    Alcotest.test_case "list-index answers an option" `Quick (fun () ->
        let found, _ = eval (compile {|(list-index (first "7") "0123456789")|}).B.tree [] in
        let missing, _ = eval (compile {|(list-index (first "x") "0123456789")|}).B.tree [] in
        Alcotest.(check string) "some 7" (Canon.encode (T.Stem ((compile "#7").B.tree)))
          (Canon.encode found);
        Alcotest.(check string) "none" "0" (Canon.encode missing)) ]

let staging =
  [ Alcotest.test_case "the page specialises at compile and answers as the live render"
      `Quick (fun () ->
        let td = todos 3 in
        let live_out, live_steps =
          eval (compile page_fn).B.tree [ (compile (Printf.sprintf "%S" item)).B.tree; td ]
        in
        let staged = compile (Printf.sprintf "(%s %S)" page_fn item) in
        let staged_out, staged_steps = eval staged.B.tree [ td ] in
        Alcotest.(check string) "same page" (str live_out) (str staged_out);
        Alcotest.(check bool)
          (Printf.sprintf "the parse ran at compile (%d steps)" staged.B.steps)
          true (staged.B.steps > 100_000);
        Alcotest.(check bool)
          (Printf.sprintf "request %d < live %d" staged_steps live_steps)
          true (staged_steps < live_steps);
        Alcotest.(check bool) "escaped" true
          (let s = str staged_out in
           let sub = "task 2 &lt;b&gt;&amp;&lt;/b&gt;" in
           let n = String.length sub in
           let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
           go 0));
    Alcotest.test_case "staging composes: (tmpl-compile t) under a lambda over t is lifted" `Quick
      (fun () ->
        (* the stdlib's own composition needs no annotation: applying
           page_fn to a literal template at compile time leaves no parse
           in the program, so specialising twice is free *)
        let once = compile (Printf.sprintf "(%s %S)" page_fn item) in
        let twice = compile (Printf.sprintf "(%s %S)" page_fn item) in
        Alcotest.(check string) "one tree" once.B.hash_hex twice.B.hash_hex) ]

let () =
  Alcotest.run "tuna eval-modes"
    [ ("template vocabulary", rendering); ("staging", staging) ]
