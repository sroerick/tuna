(* Tuna_server.Pages.Dashboard: the runs table.  The table itself is a
   fragment (/frag/runs) the page auto-refreshes via htmx; without JS
   the same URL renders inside a plain page, plus a manual refresh
   link. *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout

let runs_table pool =
  S.list_runs pool ~caller:None ~program:None ~limit:100 ()
  >>= fun rs ->
  let row r =
    Printf.sprintf
      {|<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>|}
      (L.link_run r.S.r_id)
      (L.link_program r.S.r_program_hash)
      (L.status_badge (S.Run_status.to_string r.S.r_status))
      (match r.S.r_step_count with Some s -> string_of_int s | None -> "—")
      (L.verify_badge r.S.r_verify_status)
      (L.esc (Option.value r.S.r_created_at ~default:""))
  in
  let rows = String.concat "" (List.map row rs) in
  Lwt.return
    (Printf.sprintf
       {|<div id="runs-box" hx-get="/frag/runs" hx-trigger="every 5s" hx-swap="outerHTML">
<table>
<tr><th>run</th><th>program</th><th>status</th><th>steps</th><th>verify</th><th>created</th></tr>
%s
</table>
</div>|}
       rows)

let view pool user _req =
  runs_table pool
  >>= fun table ->
  L.page ~user ~title:"tuna — runs"
       (Printf.sprintf
          {|<h2>runs</h2>
<section>%s
<noscript><p><a href="/frag/runs">refresh</a></p></noscript></section>
<section>
<h3>program lookup</h3>
<form method="get" action="/programs/lookup">
<label>hash <input name="hash" style="width:64ch"/></label>
<button type="submit">open</button>
</form>
</section>|}
          table)

(* The htmx refresh fragment (also the no-JS fallback view). *)
let frag_runs pool user req =
  runs_table pool
  >>= fun table ->
  if L.is_htmx req then Dream.html table
  else
    L.page ~user ~title:"tuna — runs (fresh)"
      (Printf.sprintf
         {|<h2>runs (fresh)</h2><section>%s</section><p><a href="/">back to runs</a></p>|}
         table)
