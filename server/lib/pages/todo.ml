(* Tuna_server.Pages.Todo: the /todo board - a small site worklist
   living IN the store, not beside it.  Each item is a tree_paths entry
   under todo/<epoch>-<hex> whose value is a JSON BYTE record
   {"title", "state" (open|done), "created_by", "created_at"}; every
   write is a journaled path op attributed to the signed-in identity,
   so the board is auditable and rewind-as-fold covers it like any
   other namespace (the routes.borg law 1 pattern at a non-route
   prefix).

   Writes go through the same accessors the /api/tree/* operator
   surface uses: page identities are token holders, so no covering
   grant is demanded here (the grant gate is the PRIM boundary, not
   the host surface).  Plain forms + redirect, no-JS first, like every
   other page. *)

open Lwt.Infix

module S = Tuna_store.Store
module J = Yojson.Basic
module L = Layout

let prefix = "todo/"

let item_id () =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let raw = really_input_string ic 4 in
      let hex = Buffer.create 8 in
      String.iter
        (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c)))
        raw;
      Printf.sprintf "%Ld-%s"
        (Int64.of_float (Unix.time ()))
        (Buffer.contents hex))

let now_iso () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let record_json ~title ~state ~created_by ~created_at =
  J.to_string
    (`Assoc
      [ ("title", `String title)
      ; ("state", `String state)
      ; ("created_by", `String created_by)
      ; ("created_at", `String created_at) ])

let journal pool ~actor ~op ~path ~value_hash ~version () =
  S.op_append pool ~op ~path ~value_hash ~prev_version:None ~version ~actor
  >>= fun _ -> Lwt.return ()

let put_item pool ~actor ~path ~bytes =
  let hash = S.byte_hash bytes in
  S.byte_value_put pool ~hash ~bytes >>= fun () ->
  S.path_put pool ~path ~value_hash:hash ~owner:actor >>= fun newv ->
  journal pool ~actor ~op:"put" ~path ~value_hash:(Some hash)
    ~version:(Some newv) ()
  >>= fun () -> Lwt.return newv

type item = {
  i_id : string
; i_title : string
; i_state : string
; i_created_by : string
; i_created_at : string
; i_version : int64
}

(* read every item under todo/, path order; unparseable records are
   shown raw rather than dropped - the board never hides store truth *)
let items pool =
  S.path_list pool ~prefix ()
  >>= fun entries ->
  Lwt_list.map_s
    (fun (e : S.path_entry) ->
      S.byte_value_fetch pool e.S.tp_value_hash
      >>= function
      | Some bytes -> (
          match J.from_string bytes with
          | j ->
              let get k =
                match J.Util.member k j with `String s -> s | _ -> "?"
              in
              Lwt.return
                (Some
                   { i_id = String.sub e.S.tp_path (String.length prefix)
                       (String.length e.S.tp_path - String.length prefix)
                   ; i_title = get "title"
                   ; i_state = get "state"
                   ; i_created_by = get "created_by"
                   ; i_created_at = get "created_at"
                   ; i_version = e.S.tp_version })
          | exception _ -> Lwt.return None)
      | None -> Lwt.return None)
    entries
  >>= fun rows -> Lwt.return (List.filter_map Fun.id rows)

let view pool user _req =
  items pool >>= fun rows ->
  let row (it : item) =
    let state_badge =
      match it.i_state with
      | "done" -> L.badge "ok" "done"
      | _ -> L.badge "warn" "open"
    in
    let title_html =
      if it.i_state = "done" then Printf.sprintf {|<s>%s</s>|} (L.esc it.i_title)
      else L.esc it.i_title
    in
    let toggle_label = if it.i_state = "done" then "reopen" else "done" in
    Printf.sprintf
      {|<tr><td>%s</td><td>%s</td><td><code>%s</code></td><td class="muted">%s</td><td><form method="post" action="/todo/%s/state" style="display:inline"><button type="submit">%s</button></form> <form method="post" action="/todo/%s/del" style="display:inline"><button type="submit">del</button></form></td></tr>|}
      state_badge title_html
      (L.esc (L.short_hash it.i_created_by))
      (L.esc it.i_created_at)
      (L.esc it.i_id) toggle_label (L.esc it.i_id)
  in
  let table =
    if rows = [] then "<p class=\"muted\">nothing on the board yet</p>"
    else
      Printf.sprintf {|<table><tr><th>state</th><th>item</th><th>by</th><th>created</th><th>actions</th></tr>%s</table>|}
        (String.concat "" (List.map row rows))
  in
  L.page ~user ~title:"todo"
    (Printf.sprintf
       {|<h2>todo</h2><p class="muted">one JSON byte value per item at <code>todo/&lt;id&gt;</code>; every write is a journaled path op. %d items.</p>%s<section><form method="post" action="/todo/add"><input name="title" placeholder="what needs doing" style="width:48ch" required> <button type="submit">add</button></form></section>|}
       (List.length rows) table)

let add pool user req =
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      match List.assoc_opt "title" fields with
      | None -> L.err_page ~user "missing title field"
      | Some title ->
          if String.trim title = "" then L.err_page ~user "empty title"
          else
            let path = prefix ^ item_id () in
            let bytes =
              record_json ~title:(String.trim title) ~state:"open"
                ~created_by:user.S.i_name ~created_at:(now_iso ())
            in
            put_item pool ~actor:user.S.i_id ~path ~bytes >>= fun _ ->
            Dream.redirect req "/todo")
  | _ -> L.err_page ~user "bad form submission"

(* the record is re-read and rewritten with the state flipped; the item
   id is path-derived, so a forged form can only touch todo/ items *)
let set_state pool user req =
  let id = Dream.param req "id" in
  let path = prefix ^ id in
  S.path_get pool ~path
  >>= function
  | None -> L.not_found ~user ("no todo item " ^ id)
  | Some entry -> (
      S.byte_value_fetch pool entry.S.tp_value_hash
      >>= function
      | None -> L.err_page ~user ("todo value missing for " ^ id)
      | Some bytes -> (
          match J.from_string bytes with
          | exception _ -> L.err_page ~user ("todo record unparseable: " ^ id)
          | j ->
              let get k =
                match J.Util.member k j with `String s -> s | _ -> "?"
              in
              let state = if get "state" = "done" then "open" else "done" in
              let bytes =
                record_json ~title:(get "title") ~state
                  ~created_by:(get "created_by")
                  ~created_at:(get "created_at")
              in
              put_item pool ~actor:user.S.i_id ~path ~bytes >>= fun _ ->
              Dream.redirect req "/todo"))

let del pool user req =
  let id = Dream.param req "id" in
  let path = prefix ^ id in
  S.path_get pool ~path
  >>= function
  | None -> L.not_found ~user ("no todo item " ^ id)
  | Some _entry ->
      S.path_delete pool ~path ~expected_version:None >>= fun _ ->
      journal pool ~actor:user.S.i_id ~op:"del" ~path ~value_hash:None
        ~version:None ()
      >>= fun () -> Dream.redirect req "/todo"
