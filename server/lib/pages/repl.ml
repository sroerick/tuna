(* Tuna_server.Pages.Repl: the REPL page (M9) — the round-based REPL
   over the identity's name->tree dictionary, journaled run-parent
   chain, and structural commands (get / patch / first-diff / dict).
   The engine lives in Repl_cmd (shared with POST /api/repl); this page
   renders its outcomes and degrades to plain forms for no-JS
   clients. *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout

let form_html ?(source = "") ?(inputs = "") ?(fuel = "1000000")
    ?(size_cap = "1000000") () =
  Printf.sprintf
    {|<form hx-post="/repl/eval" hx-target="#repl-result" hx-swap="innerHTML" method="post" action="/repl/eval">
<p><label>command (one round: eval &lt;term&gt;, def &lt;name&gt; &lt;term&gt;, get &lt;path&gt;, patch &lt;path&gt; &lt;ternary&gt;, first-diff &lt;hashA&gt; &lt;hashB&gt;, dict)<br/>
<textarea name="source" rows="6" cols="70">%s</textarea></label></p>
<p><label>inputs (one ternary per line, eval rounds)<br/><textarea name="inputs" rows="2" cols="70">%s</textarea></label></p>
<p><label>fuel <input name="fuel" value="%s" style="width:12ch"/></label>
<label>size cap <input name="size_cap" value="%s" style="width:12ch"/></label></p>
<button type="submit">run round</button>
</form>
<p class="muted">cheat sheet: <code>eval (lambda (x) x)</code> · <code>def not %%22102000</code> ·
<code>eval (not %%22102000)</code> · <code>get 0</code> · <code>patch 1 %%0</code> ·
<code>first-diff &lt;hashA&gt; &lt;hashB&gt;</code> · <code>dict</code> · <code>undef name</code>.
Journaled rounds (eval, def) are run rows chained per session — every result links its run.</p>|}
    (L.esc source) (L.esc inputs) fuel size_cap

(* -- outcome rendering ------------------------------------------------ *)

let outcome_html (o : Repl_cmd.outcome) =
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf "<h3>round: %s</h3>\n" (L.esc o.Repl_cmd.o_kind));
  (match o.Repl_cmd.o_status with
   | Some st ->
       Buffer.add_string buf (Printf.sprintf "<p>%s</p>\n" (L.status_badge st))
   | None -> ());
  if o.Repl_cmd.o_ternary <> "" then
    Buffer.add_string buf
      (Printf.sprintf
         "<p>ternary</p>\n<pre class=\"code\">%s</pre>\n<p>hash <code>%s</code></p>\n"
         (L.esc o.Repl_cmd.o_ternary)
         (L.esc o.Repl_cmd.o_hash));
  if o.Repl_cmd.o_steps > 0 then
    Buffer.add_string buf (Printf.sprintf "<p>steps %d</p>\n" o.Repl_cmd.o_steps);
  if o.Repl_cmd.o_note <> "" then
    Buffer.add_string buf
      (Printf.sprintf "<p>%s</p>\n" (L.esc o.Repl_cmd.o_note));
  (match o.Repl_cmd.o_run_id with
   | Some id ->
       Buffer.add_string buf
         (Printf.sprintf "<p>run %s (journaled; transcript chain)</p>\n"
            (L.link_run id))
   | None -> ());
  (match o.Repl_cmd.o_program_hash with
   | Some h ->
       Buffer.add_string buf
         (Printf.sprintf "<p>program %s</p>\n" (L.link_program h))
   | None -> ());
  if o.Repl_cmd.o_dict_rows <> [] then begin
    Buffer.add_string buf "<table><tr><th>name</th><th>ternary</th></tr>\n";
    List.iter
      (fun (n, t) ->
        Buffer.add_string buf
          (Printf.sprintf "<tr><td>%s</td><td><code>%s</code></td></tr>\n"
             (L.esc n) (L.esc t)))
      o.Repl_cmd.o_dict_rows;
    Buffer.add_string buf "</table>\n"
  end;
  Buffer.contents buf

let error_html msg = Printf.sprintf {|<p class="err">%s</p>|} (L.esc msg)
let empty_html = {|<p class="muted">no evaluation yet — submit a command.</p>|}

(* -- dictionary table -------------------------------------------------- *)

let dict_table (rows : S.dict_entry list) =
  if rows = [] then
    {|<p class="muted">dictionary is empty — <code>def name term</code> adds entries.</p>|}
  else begin
    let buf = Buffer.create 512 in
    Buffer.add_string buf
      "<h3>dictionary</h3><table><tr><th>name</th><th>ternary</th><th>updated</th></tr>\n";
    List.iter
      (fun d ->
        Buffer.add_string buf
          (Printf.sprintf
             "<tr><td>%s</td><td><code>%s</code></td><td>%s</td></tr>\n"
             (L.esc d.S.d_name)
             (L.esc d.S.d_ternary)
             (L.esc (Option.value d.S.d_updated_at ~default:""))))
      rows;
    Buffer.add_string buf "</table>\n";
    Buffer.contents buf
  end

(* -- handlers ---------------------------------------------------------- *)

let int_of _name dflt = function
  | None -> dflt
  | Some s -> ( match int_of_string (String.trim s) with i -> i | exception _ -> dflt)

(* The round: same engine as POST /api/repl.  htmx gets the outcome
   fragment; no-JS gets the full page with the form refilled. *)
let eval pool user req =
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      let get k = List.assoc_opt k fields in
      let source = match get "source" with Some s -> s | None -> "" in
      let inputs = match get "inputs" with Some s -> s | None -> "" in
      let fuel = int_of "fuel" 1_000_000 (get "fuel") in
      let size_cap = int_of "size_cap" 1_000_000 (get "size_cap") in
      let input_list =
        List.filter (fun s -> String.trim s <> "")
          (String.split_on_char '\n' inputs)
      in
      if String.trim source = "" then
        if L.is_htmx req then Dream.html empty_html
        else
          L.page ~user ~title:"tuna — repl"
            (form_html () ^ Printf.sprintf {|<div id="repl-result">%s</div>|}
               empty_html)
      else
        Repl_cmd.execute pool
          ~caller:user.S.i_id ~command:source ~inputs:input_list ~grant_ids:[]
          ~fuel ~size_cap ()
        >>= (function
              | Error (_, msg) -> Dream.html (error_html msg)
              | Ok o ->
                  if L.is_htmx req then Dream.html (outcome_html o)
                  else
                    L.page ~user ~title:"tuna — repl"
                      (form_html ~source ~inputs
                         ~fuel:(string_of_int fuel)
                         ~size_cap:(string_of_int size_cap) ()
                      ^ Printf.sprintf {|<div id="repl-result">%s</div>|}
                          (outcome_html o))))
  | _ -> L.err_page ~user "bad form submission"

let view pool user _req =
  S.dict_list pool ~identity_id:user.S.i_id
  >>= fun rows ->
  L.page ~user ~title:"tuna — repl"
    (Printf.sprintf
       {|<h2>repl</h2>
<section>%s
<div id="repl-result">%s</div>
</section>
<section>%s</section>|}
       (form_html ())
       empty_html
       (dict_table rows))
