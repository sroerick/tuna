(* Tuna_server.Pages.Todo: the /todo board v2 — rim-only OCaml over
   Midden-backed Sabra programs (board-add, board-view, board-flip,
   board-del).

   Every read and write happens inside a Sabra program calling
   `midden/{get,put,delete,query}` with per-request structurally-scoped
   grants (scope_schema = "todo"), revoked at request end.  Counts come
   from Midden's declarative `todo_counts` projection (true whole-board
   open/done counts at 5,000+ items, solving F14). *)

open Tuna_store.Direct
module Tree = Tuna.Tree
module S = Tuna_store.Store
module MC = Tuna_codec.Midden_codec
module L = Layout

let schema_name = "todo"
let page_size = 24
let run_fuel = 10_000_000
let run_size_cap = 100_000

(* Deterministic or fresh RFC 4122 v4 UUID for each new todo entity. *)
let fresh_entity_id () =
  let ic = open_in_bin "/dev/urandom" in
  let b = Bytes.create 16 in
  really_input ic b 0 16;
  close_in ic;
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let h = Buffer.create 36 in
  String.iter
    (fun c -> Buffer.add_string h (Printf.sprintf "%02x" (Char.code c)))
    (Bytes.unsafe_to_string b);
  let s = Buffer.contents h in
  Printf.sprintf "%s-%s-%s-%s-%s" (String.sub s 0 8) (String.sub s 8 4)
    (String.sub s 12 4) (String.sub s 16 4) (String.sub s 20 12)

let valid_uuid id =
  match Uuidm.of_string id with Some _ -> true | None -> false

let iso_of_epoch (t : int64) =
  let tm = Unix.gmtime (Int64.to_float t) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* -- result-tree decoding (draw only) --------------------------------- *)

let rec nat_of = function
  | Tree.Leaf -> Some 0
  | Tree.Stem t -> Option.map succ (nat_of t)
  | Tree.Fork _ -> None

let tagged_fields (recd : Tree.t) : (int * Tree.t) list =
  let rec go acc = function
    | Tree.Fork (Tree.Fork (k, Tree.Fork (v, Tree.Leaf)), rest) -> (
        match nat_of k with
        | Some tag -> go ((tag, v) :: acc) rest
        | None -> go acc rest)
    | _ -> List.rev acc
  in
  go [] recd

let str_of_tag fields tag ~default =
  match List.assoc_opt tag fields with
  | Some v -> Option.value (Tuna.Cstr.decode v) ~default
  | None -> default

let int64_of_tag fields tag ~default =
  match List.assoc_opt tag fields with
  | Some v -> Option.value (MC.int64_of_tree v) ~default
  | None -> default

type row = {
  id : string;
  title : string;
  state : string;
  who : string;
  created : string;
}

type board = {
  rows : row list;
  opens : int64;
  done_ct : int64;
}

(* todo_by_created columns (ordinal_position 1..5):
   1=entity, 2=created, 3=state, 4=title, 5=who
   todo_counts columns (ordinal_position 1..2):
   1=state, 2=count *)
let board_of_tree t =
  match t with
  | Tree.Fork (items_t, counts_t) ->
      let rows =
        Prims.list_of_tree items_t
        |> List.filter_map (fun rt ->
               let fs = tagged_fields rt in
               let id = str_of_tag fs 1 ~default:"" in
               if id = "" then None
               else
                 Some
                   {
                     id;
                     created = iso_of_epoch (int64_of_tag fs 2 ~default:0L);
                     state = str_of_tag fs 3 ~default:"open";
                     title = str_of_tag fs 4 ~default:"";
                     who = str_of_tag fs 5 ~default:"?";
                   })
      in
      let opens = ref 0L and done_ct = ref 0L in
      List.iter
        (fun ct ->
          let fs = tagged_fields ct in
          let st = str_of_tag fs 1 ~default:"" in
          let n = int64_of_tag fs 2 ~default:0L in
          if st = "open" then opens := n
          else if st = "done" then done_ct := n)
        (Prims.list_of_tree counts_t);
      Some { rows; opens = !opens; done_ct = !done_ct }
  | _ -> None

(* -- rim run (narrow structural Midden grants, revoked after request) -- *)

let tree_str s = Tuna.Cstr.encode s

let mint_run pool user ~name inputs prims : (S.run * S.journal list, string) result =
  let rec mints acc = function
    | [] -> return (List.rev acc)
    | p :: rest ->
        S.mint_grant pool ~prim:p ~args_attenuation:"null"
          ~scope_schema:(Some schema_name) ~scope_namespace:(Some "local")
          ~caller:user.S.i_id ~minted_by:(Some user.S.i_id) ()
        >>= fun g -> mints (g.S.g_id :: acc) rest
  in
  mints [] prims >>= fun gids ->
  Board_seed.program_hash pool ~name >>= function
  | Error m ->
      List.iter (S.revoke_grant pool) gids;
      return (Error m)
  | Ok ph ->
      let r =
        Run.execute_run pool ~caller:user.S.i_id ~program_hash:ph
          ~input_trees:inputs ~grant_ids:gids ~fuel:run_fuel
          ~size_cap:run_size_cap ()
      in
      List.iter (S.revoke_grant pool) gids;
      match r with
      | Ok x -> return (Ok x)
      | Error (_, m) -> return (Error m)

(* -- handlers --------------------------------------------------------- *)

let banner pool req =
  match Web.query req "run" with
  | None | Some "" -> ""
  | Some run_id -> (
      S.fetch_run_resolved pool run_id >>= function
      | None -> {|<p class="err">no such run</p>|}
      | Some (_, row) ->
          let denials = row.S.r_denial_count in
          let verdict =
            match row.S.r_verify_status with Some v -> v | None -> "unverified"
          in
          let cls =
            match verdict with "verified" -> "ok" | "unverified" -> "" | _ -> "warn"
          in
          Printf.sprintf
            {|<p class="muted">last change: run %s %s (%d denial%s)</p>|}
            (L.link_run run_id) (L.badge cls verdict) denials
            (if denials = 1 then "" else "s"))

let view pool user req =
  banner pool req >>= fun banner_html ->
  let state_filter =
    match Web.query req "state" with
    | Some ("open" as s) | Some ("done" as s) -> s
    | _ -> ""
  in
  let filter_arg =
    if state_filter = "" then "" else "state=" ^ state_filter
  in
  let offset =
    match Web.query req "offset" with
    | Some s -> ( match int_of_string_opt s with Some n when n >= 0 -> n | _ -> 0)
    | None -> 0
  in
  let cursor_arg = if offset = 0 then "" else string_of_int offset in
  mint_run pool user ~name:"board-view"
    [ tree_str filter_arg; tree_str cursor_arg; tree_str "10" ]
    [ "midden/query" ]
  >>= function
  | Error m -> L.err_page ~user m
  | Ok (row, _) -> (
      match row.S.r_status with
      | S.Run_status.Normal -> (
          match row.S.r_result_ternary with
          | None -> L.err_page ~user "the board view run produced no result"
          | Some tern -> (
              match Tuna.Canon.of_string tern with
              | Error (off, msg) ->
                  L.err_page ~user
                    (Printf.sprintf "board view result unparseable at %d: %s" off
                       msg)
              | Ok t -> (
                  match board_of_tree t with
                  | None -> L.err_page ~user "board view answered a wrong shape"
                  | Some b ->
                      let render_row (it : row) =
                        let state_badge =
                          if it.state = "done" then L.badge "ok" "done"
                          else L.badge "warn" "open"
                        in
                        let title_html =
                          if it.state = "done" then
                            Printf.sprintf {|<s>%s</s>|} (L.esc it.title)
                          else L.esc it.title
                        in
                        let toggle_label =
                          if it.state = "done" then "reopen" else "done"
                        in
                        Printf.sprintf
                          {|<tr><td>%s</td><td>%s</td><td><code>%s</code></td><td class="muted">%s</td><td><form method="post" action="/todo/%s/state" style="display:inline"><button type="submit">%s</button></form> <form method="post" action="/todo/%s/del" style="display:inline"><button type="submit">del</button></form></td></tr>|}
                          state_badge title_html (L.esc it.who)
                          (L.esc it.created) (L.esc it.id) toggle_label
                          (L.esc it.id)
                      in
                      let table =
                        if b.rows = [] then
                          "<p class=\"muted\">nothing on the board yet</p>"
                        else
                          Printf.sprintf
                            {|<table><tr><th>state</th><th>item</th><th>by</th><th>created</th><th>actions</th></tr>%s</table>|}
                            (String.concat "" (List.map render_row b.rows))
                      in
                      let qs_state =
                        if state_filter = "" then "" else "state=" ^ state_filter ^ "&"
                      in
                      let filter_bar =
                        Printf.sprintf
                          {|<p class="muted">filter: <a href="/todo">all</a> · <a href="/todo?state=open">open (%Ld)</a> · <a href="/todo?state=done">done (%Ld)</a></p>|}
                          b.opens b.done_ct
                      in
                      let pager =
                        if List.length b.rows >= page_size then
                          Printf.sprintf
                            {|<p class="muted"><a href="/todo?%soffset=%d">older cards…</a></p>|}
                            qs_state (offset + page_size)
                        else ""
                      in
                      L.page ~user ~title:"todo"
                        (Printf.sprintf
                           {|<h2>todo</h2><p class="muted">every read and write on this board is a sabra program run over journaled, grant-gated Midden prims (board v2). %Ld open, %Ld done total.</p>%s%s%s%s<section><form method="post" action="/todo/add"><input name="title" placeholder="what needs doing" style="width:48ch" required> <button type="submit">add</button></form></section>|}
                           b.opens b.done_ct filter_bar banner_html table pager))))
      | st ->
          L.err_page ~user
            (Printf.sprintf "the board program ended %s"
               (S.Run_status.to_string st)))

let add pool user req =
  Web.form ~csrf:false req >>= function
  | `Wrong_content_type -> L.err_page ~user "bad form submission"
  | `Ok fields ->
      let title = Option.value ~default:"" (List.assoc_opt "title" fields) in
      if String.trim title = "" then L.err_page ~user "a title is required"
      else
        let entity = fresh_entity_id () in
        let who = user.S.i_name in
        let created = MC.int64_to_tree (Int64.of_float (Unix.time ())) in
        mint_run pool user ~name:"board-add"
          [ tree_str entity; tree_str title; tree_str who; created ]
          [ "midden/put" ]
        >>= function
        | Error m -> L.err_page ~user m
        | Ok (row, _) -> (
            match row.S.r_status with
            | S.Run_status.Normal ->
                Web.redirect req (Printf.sprintf "/todo?run=%s" row.S.r_id)
            | st ->
                L.err_page ~user
                  (Printf.sprintf "add run %s ended %s" row.S.r_id
                     (S.Run_status.to_string st)))

let act pool user req ~name ~prims =
  let id = Web.param req "id" in
  if not (valid_uuid id) then L.not_found ~user ("no todo item " ^ id)
  else
    mint_run pool user ~name [ tree_str id ] prims >>= function
    | Error m -> L.err_page ~user m
    | Ok (row, _) -> (
        match row.S.r_status with
        | S.Run_status.Normal ->
            Web.redirect req (Printf.sprintf "/todo?run=%s" row.S.r_id)
        | st ->
            L.err_page ~user
              (Printf.sprintf "%s run %s ended %s" name row.S.r_id
                 (S.Run_status.to_string st)))

let set_state pool user req =
  act pool user req ~name:"board-flip" ~prims:[ "midden/get"; "midden/put" ]

let del pool user req =
  act pool user req ~name:"board-del" ~prims:[ "midden/get"; "midden/delete" ]
