(* Tuna_server.Pages.Repl: the REPL page — an htmx round-trip of
   compile+eval (M8 slice).  The name->tree dictionary, defines, and
   journaled transcripts land in M9; this page already degrades to a
   plain form for no-JS clients. *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout
module B = Tuna_compiler.Bracket

type outcome = {
  o_ternary : string  (* result ternary ("" unless normal) *)
; o_steps : int
; o_status : string
}

let form_html ?(fuel = "1000000") ?(size_cap = "1000000") () =
  Printf.sprintf
    {|<form hx-post="/repl/eval" hx-target="#repl-result" hx-swap="innerHTML" method="post" action="/repl/eval">
<p><label>term (s-expr)<br/><textarea name="source" rows="6" cols="70" placeholder="(lambda (x) x)"></textarea></label></p>
<p><label>inputs (one ternary per line)<br/><textarea name="inputs" rows="2" cols="70"></textarea></label></p>
<p><label>fuel <input name="fuel" value="%s" style="width:12ch"/></label>
<label>size cap <input name="size_cap" value="%s" style="width:12ch"/></label></p>
<button type="submit">eval</button>
</form>|}
    fuel size_cap

let result_html = function
  | Ok (Some o) ->
      Printf.sprintf
        {|<h3>result</h3>
<p>%s</p>
<pre class="code">%s</pre>
<p>steps %d</p>|}
        (L.status_badge o.o_status) (L.esc o.o_ternary) o.o_steps
  | Ok None -> {|<p class="muted">no evaluation yet — submit a term.</p>|}
  | Error msg -> Printf.sprintf {|<p class="err">%s</p>|} (L.esc msg)

(* M8 round-trip: compile the surface term, then evaluate the artifact
   with the pure stepper (the same engine as the differential harness).
   Compilation IS reduction, so the artifact is already a normal form;
   evaluation folds it against the given inputs. *)
let compile_and_eval ~source ~input_trees ~fuel ~size_cap :
    (outcome option, string) result =
  try
    (match B.compile_source source with
    | art -> (
        match Tuna.Canon.of_string art.B.ternary with
        | Error (off, msg) ->
            Error (Printf.sprintf "artifact unparseable at offset %d: %s" off msg)
        | Ok program -> (
            match Tuna_interp.Eval.eval ~fuel ~size_cap ~program input_trees with
            | Tuna_interp.Eval.Normal (t, steps) ->
                Ok
                  (Some
                     { o_ternary = Tuna.Canon.encode t
                     ; o_steps = steps
                     ; o_status = "normal" })
            | Tuna_interp.Eval.Fuel_exhausted steps ->
                Ok
                  (Some
                     { o_ternary = ""; o_steps = steps; o_status = "fuel_exhausted" })
            | Tuna_interp.Eval.Size_exhausted steps ->
                Ok
                  (Some
                     { o_ternary = ""; o_steps = steps; o_status = "size_exhausted" }))))
    with
    | Tuna_compiler.Ir.Error (p, msg) ->
        Error (Printf.sprintf "compile error: %s" (Tuna_compiler.Ir.show_error (p, msg)))
    | B.Compile_failed msg -> Error ("compile failed: " ^ msg)
    | exn -> Error ("compile crashed: " ^ Printexc.to_string exn)

let eval _pool user req =
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      let get k = List.assoc_opt k fields in
      let source = match get "source" with Some s -> s | None -> "" in
      let int_of name dflt =
        match get name with
        | Some s -> (try int_of_string (String.trim s) with _ -> dflt)
        | None -> dflt
      in
      let fuel = int_of "fuel" 1_000_000 in
      let cap = int_of "size_cap" 1_000_000 in
      if String.trim source = "" then Dream.html (result_html (Ok None))
      else
        let inputs =
          match get "inputs" with
          | None -> Ok []
          | Some s ->
              let rec go acc = function
                | [] -> Ok (List.rev acc)
                | line :: rest -> (
                    let line = String.trim line in
                    if line = "" then go acc rest
                    else
                      match Tuna.Canon.of_string line with
                      | Ok t -> go (t :: acc) rest
                      | Error (off, msg) ->
                          Error
                            (Printf.sprintf "input parse error at offset %d: %s" off
                               msg))
              in
              go [] (String.split_on_char '\n' s)
        in
        match inputs with
        | Error msg -> Dream.html (result_html (Error msg))
        | Ok input_trees -> (
            match compile_and_eval ~source ~input_trees ~fuel ~size_cap:cap with
            | Ok o ->
                if L.is_htmx req then Dream.html (result_html (Ok o))
                else
                  L.page ~user ~title:"tuna — repl"
                    (form_html ()
                    ^ Printf.sprintf {|<div id="repl-result">%s</div>|}
                        (result_html (Ok o)))
            | Error msg -> Dream.html (result_html (Error msg))))
  | _ -> L.err_page ~user "bad form submission"

let view _pool user _req =
  L.page ~user ~title:"tuna — repl"
    (Printf.sprintf
       {|<h2>repl</h2>
<section>%s
<div id="repl-result">%s</div>
</section>
<p class="muted">repl rounds are pure compile+eval here; dictionary +
journaled transcripts land with the M9 repl milestone.</p>|}
       (form_html ())
       (result_html (Ok None)))
