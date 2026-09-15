(* Tuna_server.Pages.Run_page: the run row, its journal, the verify
   button (replay), and the divergence view (journal.divergence-surface
   rendered as addressed structure).  The journal is a htmx tick
   fragment while the run is running; the tick stops by dropping its
   own poll attribute once the run is finished.  Without JS: refresh
   links + the verify form posts to the same URL and redirects back. *)

open Lwt.Infix

module S = Tuna_store.Store
module L = Layout

let finished r = r.S.r_status <> S.Run_status.Running

let journal_div run_id (js : S.journal list) running =
  let row (j : S.journal) =
    Printf.sprintf
      {|<tr><td>%d</td><td>/%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>|}
      j.S.j_seq
      (L.esc j.S.j_callsite_path)
      (L.esc j.S.j_prim)
      (L.esc (Option.value j.S.j_args_ternary ~default:"—"))
      (L.esc (Option.value j.S.j_result_ternary ~default:"—"))
      (L.esc (Option.value j.S.j_error ~default:""))
      (match j.S.j_wall_ms with Some w -> Printf.sprintf "%dms" w | None -> "—")
      (L.esc (L.short_hash j.S.j_row_hash))
  in
  let poll =
    if running then
      Printf.sprintf {| hx-get="/runs/%s/journal" hx-trigger="every 2s" hx-swap="outerHTML"|} run_id
    else ""
  in
  Printf.sprintf
    {|<div id="journal-box"%s>
<table>
<tr><th>seq</th><th>callsite</th><th>prim</th><th>args</th><th>result</th><th>error</th><th>wall</th><th>row_hash</th></tr>
%s
</table>
</div>|}
    poll (String.concat "" (List.map row js))

let verify_fragment (v : Replay.verdict) : string =
  match v with
  | Replay.Verified _ ->
      Printf.sprintf
        {|<span id="verify-slot">%s <span class="okmsg">verified by faithful replay</span></span>|}
        (L.badge "ok" "verified")
  | Replay.Bad_chain msg ->
      Printf.sprintf
        {|<span id="verify-slot">%s <span class="err">journal hash chain broken: %s</span></span>|}
        (L.badge "bad" "verify failed") (L.esc msg)
  | Replay.Diverged d ->
      Printf.sprintf
        {|<span id="verify-slot">%s</span>
<div class="divergence">
<h3>divergence</h3>
<table>
<tr><th>seq</th><td>%s</td></tr>
<tr><th>callsite path</th><td>/%s</td></tr>
<tr><th>prim</th><td>%s</td></tr>
<tr><th>reason</th><td>%s</td></tr>
<tr><th>first diff path</th><td>/%s</td></tr>
<tr><th>recorded hash</th><td>%s</td></tr>
<tr><th>replayed hash</th><td>%s</td></tr>
</table>
</div>|}
        (L.badge "bad" "verify failed")
        (match d.Replay.div_seq with Some s -> string_of_int s | None -> "—")
        (L.esc d.Replay.callsite_path)
        (L.esc d.Replay.prim)
        (L.esc d.Replay.reason)
        (L.esc d.Replay.first_diff_path)
        (L.esc (Option.value d.Replay.recorded_hash ~default:"—"))
        (L.esc (Option.value d.Replay.replayed_hash ~default:"—"))
  | Replay.Unverifiable msg ->
      Printf.sprintf
        {|<span id="verify-slot">%s <span class="err">%s</span></span>|}
        (L.badge "muted" "unverifiable") (L.esc msg)

let verify_button run_id =
  Printf.sprintf
    {|<span id="verify-slot">
<form hx-post="/runs/%s/verify" hx-target="#verify-slot" hx-swap="outerHTML" method="post" action="/runs/%s/verify" style="display:inline">
<button type="submit">verify (replay)</button>
</form>
</span>|}
    run_id run_id

let view pool user req =
  let id = Dream.param req "id" in
  S.fetch_run pool id
  >>= function
  | None -> L.not_found ~user "unknown run id"
  | Some r0 -> (
      (* auto-verify an unverified finished run — same rule as the API *)
      (if r0.S.r_verify_status = None && finished r0 then
         Replay.verify_and_record pool ~run_id:id >>= fun _ -> S.fetch_run pool id
       else Lwt.return (Some r0))
      >>= function
      | None -> L.not_found ~user "run row vanished"
      | Some r -> (
          S.fetch_journals pool id
          >>= fun js ->
          let running = not (finished r) in
          L.page ~user ~title:"tuna — run"
            (Printf.sprintf
               {|<h2>run %s…</h2>
<p>%s %s</p>
<table>
<tr><th>program</th><td>%s</td></tr>
<tr><th>status</th><td>%s</td></tr>
<tr><th>steps</th><td>%s</td></tr>
<tr><th>fuel / cap</th><td>%d / %d</td></tr>
<tr><th>result</th><td>%s</td></tr>
<tr><th>verify</th><td>%s %s</td></tr>
<tr><th>parent run</th><td>%s</td></tr>
<tr><th>created</th><td>%s</td></tr>
</table>
<section><h3>journal</h3>%s
<noscript><p><a href="/runs/%s/journal">refresh journal</a></p></noscript></section>|}
               (L.esc (L.short_hash r.S.r_id))
               (L.status_badge (S.Run_status.to_string r.S.r_status))
               (L.verify_badge r.S.r_verify_status)
               (L.link_program r.S.r_program_hash)
               (S.Run_status.to_string r.S.r_status)
               (match r.S.r_step_count with Some s -> string_of_int s | None -> "—")
               r.S.r_fuel r.S.r_size_cap
               (match r.S.r_result_ternary with
                | Some t -> L.code_block t
                | None -> {|<span class="muted">no result yet</span>|})
               (L.verify_badge r.S.r_verify_status)
               (if r.S.r_verify_status = Some "failed" then verify_button r.S.r_id else "")
               (match r.S.r_parent_run_id with Some p -> L.link_run p | None -> "—")
               (L.esc (Option.value r.S.r_created_at ~default:""))
               (journal_div r.S.r_id js running)
               r.S.r_id)))

(* The journal tick fragment (no-JS fallback: a plain page with the table). *)
let journal_frag pool user req =
  let id = Dream.param req "id" in
  S.fetch_run pool id
  >>= function
  | None -> L.not_found ~user "unknown run id"
  | Some r -> (
      S.fetch_journals pool id
      >>= fun js ->
      let html = journal_div r.S.r_id js (not (finished r)) in
      if L.is_htmx req then Dream.html html
      else
        L.page ~user ~title:"tuna — journal"
          (Printf.sprintf
             {|<h2>journal of run %s…</h2><section>%s</section><p><a href="/runs/%s">back to run</a></p>|}
             (L.esc (L.short_hash r.S.r_id)) html r.S.r_id))

(* Verify button POST: run the replay verifier now, surface the verdict
   (fragment for htmx; redirect back to the run page for plain forms). *)
let verify_post pool user req =
  let id = Dream.param req "id" in
  S.fetch_run pool id
  >>= function
  | None -> L.not_found ~user "unknown run id"
  | Some _ -> (
      Replay.verify_and_record pool ~run_id:id
      >>= fun v ->
      if L.is_htmx req then Dream.html (verify_fragment v)
      else Dream.redirect req ("/runs/" ^ id))
