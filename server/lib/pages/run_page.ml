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
  | Replay.Gone msg ->
      Printf.sprintf
        {|<span id="verify-slot">%s <span class="muted">%s</span></span>|}
        (L.badge "muted" "gone (gced)") (L.esc msg)

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
           Replay.verify_and_record pool ~run_id:id ~deadline:(Run.deadline_now ()) ()
           >>= fun _ -> S.fetch_run pool id
       else Lwt.return (Some r0))
      >>= function
      | None -> L.not_found ~user "run row vanished"
        | Some r -> (
            S.fetch_journals pool id
            >>= fun js ->
            S.fetch_run_trace pool id
            >>= fun ts ->
            let running = not (finished r) in
          L.page ~user ~title:"tuna — run"
            (Printf.sprintf
               {|<h2>run %s…</h2>
<p>%s %s</p>
<table>
<tr><th>program</th><td>%s</td></tr>
<tr><th>status</th><td>%s</td></tr>
<tr><th>steps</th><td>%s</td></tr>
<tr><th>trace</th><td>%s</td></tr>
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
                 (match ts with
                  | Some s ->
                      Printf.sprintf
                        {|<a href="/runs/%s/trace">%d events</a> <span class="muted">(raw %d · charged %d · hits %d%s%s)</span>|}
                        r.S.r_id s.S.t_recorded s.S.t_raw_firings s.S.t_charged
                        s.S.t_memo_hits
                        (if s.S.t_loop then " · loop" else "")
                        (if s.S.t_truncated then " · truncated" else "")
                  | None -> {|<span class="muted">untraced</span>|})
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


(* The firing-trace page (borg/trace.borg): summary + capped event
   pages, server-rendered with plain links (no JS needed).  The trace is
   observability only — the run row's numbers and verdict remain the
   authoritative statement; the trace just shows the firings behind
   them, one row at a time. *)
let trace_page pool user req =
  let id = Dream.param req "id" in
  let page_size =
    match Dream.query req "limit" with
    | Some s -> (
        match int_of_string_opt s with
        | Some v when v > 0 && v <= 1000 -> v
        | _ -> 200)
    | None -> 200
  in
  let after =
    match Dream.query req "after" with
    | Some s -> (
        match int_of_string_opt s with Some v when v >= 0 -> v | _ -> -1)
    | None -> -1
  in
  S.fetch_run pool id
  >>= function
  | None -> L.not_found ~user "unknown run id"
  | Some r -> (
      S.fetch_run_trace pool id
      >>= function
      | None -> L.not_found ~user "run has no trace"
      | Some ts ->
          S.fetch_trace_events pool id ~after_seq:after ~limit:page_size
          >>= fun evs ->
          let ev_row (e : S.trace_event) =
            Printf.sprintf
              {|<tr><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>|}
              e.S.v_seq (L.esc e.S.v_kind) (L.esc e.S.v_rule) (L.esc e.S.v_fun)
              (L.esc e.S.v_arg) (L.esc e.S.v_note)
          in
          let nav =
            let first =
              if after >= 0 then
                Printf.sprintf {|<a href="/runs/%s/trace">first page</a> |} id
              else ""
            in
            let next =
              if List.length evs = page_size then
                let last =
                  match List.rev evs with e :: _ -> e.S.v_seq | [] -> after
                in
                Printf.sprintf {|<a href="/runs/%s/trace?after=%d">next page</a>|}
                  id (last + 1)
              else ""
            in
            first ^ next
          in
          L.page ~user ~title:"tuna — trace"
            (Printf.sprintf
               {|<h2>trace of run %s…</h2>
<p>%s <a href="/runs/%s">back to run</a></p>
<table>
<tr><th>semantics</th><td>%s</td></tr>
<tr><th>raw firings / charged</th><td>%d / %d</td></tr>
<tr><th>memo hits / dirty re-executions</th><td>%d / %d</td></tr>
<tr><th>loop</th><td>%s</td></tr>
<tr><th>events recorded</th><td>%d%s</td></tr>
</table>
<section><h3>events</h3>
<table>
<tr><th>seq</th><th>kind</th><th>rule</th><th>fun</th><th>arg</th><th>note</th></tr>
%s
</table>
<p>%s</p></section>|}
               (L.esc (L.short_hash r.S.r_id))
               (L.status_badge (S.Run_status.to_string r.S.r_status))
               r.S.r_id
               (L.esc ts.S.t_semantics)
               ts.S.t_raw_firings ts.S.t_charged
               ts.S.t_memo_hits ts.S.t_dirty_firings
               (if ts.S.t_loop then "detected (in-flight re-entry)" else "no")
               ts.S.t_recorded
               (if ts.S.t_truncated then
                  {| <span class="muted">(truncated at the cap)</span>|}
                else "")
               (String.concat "" (List.map ev_row evs))
               nav))

(* Verify button POST: run the replay verifier now, surface the verdict
   (fragment for htmx; redirect back to the run page for plain forms). *)
let verify_post pool user req =
  let id = Dream.param req "id" in
  S.fetch_run pool id
  >>= function
  | None -> L.not_found ~user "unknown run id"
  | Some _ -> (
      Replay.verify_and_record pool ~run_id:id ~deadline:(Run.deadline_now ()) ()
      >>= fun v ->
      if L.is_htmx req then Dream.html (verify_fragment v)
      else Dream.redirect req ("/runs/" ^ id))
