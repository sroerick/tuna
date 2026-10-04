(* Tuna_server.Pages.Todo: the /todo board - THE FIRST MEMBER PAGE IN-
   CALCULUS (borg/board.borg).  Once host OCaml over JSON byte records,
   the page is now RIM ONLY (law L3): session auth, form parse, form
   id mint, per-request grant mint (law L4), Run.execute, render.  No
   store path or value accessor appears in this module (acceptance
   13.4 greps that accessor family absent from the code); every
   board read and write happens inside a sabra program (Board_embed,
   Board_seed) against the tree store through prims, journaled as runs
   attributed to the signed-in member (law L5).

   The page's own runs surface ON the page itself (law L8): a mutation
   redirects back with ?run=<id> and the view banner carries the run
   link, denial_count, and verify state - a member toggling a card
   reads the effect the way the differentiator pitch says they can.

   Grant note: the mint names EXACTLY the prims each program uses,
   path-scopes the tree prims to todo/, and REVOKES at the end of the
   request - lifetime the request, nothing accumulating on
   /grants (tighter than the serve_program precedent, which mints
   prim "*" and never revokes).  Auth posture unchanged: require_auth
   per routes.borg law 1 - the grant gate is the prim boundary, not
   the host surface. *)

open Tuna_store.Direct
module Tree = Tuna.Tree
module S = Tuna_store.Store
module L = Layout

let prefix = "todo/"

(* law L6: page-triggered runs ride the measured F14 bridge budget *)
let run_fuel = 10_000_000
let run_size_cap = 100_000

(* -- rim helpers --------------------------------------------------------- *)

(* member-visible ids, exactly the host-era mint: <epoch>-<8 hex>, so
   pre-swap links keep resolving.  Rim-minted and opaque to programs
   (law L2). *)
let item_id () =
  let t = Unix.time () in
  let ic = open_in_bin "/dev/urandom" in
  let b = really_input_string ic 4 in
  close_in ic;
  let hex =
    let fmt = Printf.sprintf "%02x" in
    String.concat "" (List.init 4 (fun i -> fmt (Char.code b.[i])))
  in
  Printf.sprintf "%Ld-%s" (Int64.of_float t) hex

let valid_id id =
  let n = String.length id in
  n > 0
  && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub id 0 1)
  && (match String.index_opt id '-' with
      | Some dash ->
          dash > 0
          && dash + 9 = n
          && String.for_all
               (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
               (String.sub id (dash + 1) 8)
          && String.for_all (fun c -> c >= '0' && c <= '9')
               (String.sub id 0 dash)
      | None -> false)

let iso_of_epoch (t : int64) =
  let tm = Unix.gmtime (Int64.to_float t) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* -- result-tree rendering (decode-to-draw; the calculus decided) ------ *)

let canon = Tuna.Canon.encode

(* law-5 int tree -> int64, per the pinned codec (draw-only decode) *)
let tree_to_int64 t =
  let bit = function
    | Tree.Leaf -> Some 0L
    | Tree.Stem _ -> Some 1L
    | Tree.Fork _ -> None
  in
  let rec mag acc v i =
    match v with
    | Tree.Leaf | Tree.Stem _ -> Some acc
    | Tree.Fork (h, tl) -> (
        if i >= 63 then None
        else
          match bit h with
          | None -> None
          | Some b -> mag (Int64.logor acc (Int64.shift_left b i)) tl (i + 1))
  in
  match t with
  | Tree.Fork (sign, m) -> (
      let bits = mag 0L m 0 in
      if bits = Some 0L then Some 0L
      else
        (* products keep positive whens; a negative sign draws as the
           absolute value with a leading '-' (never expected here) *)
        match (bit sign, bits) with Some 1L, Some v -> Some v | Some 0L, Some v -> Some v | _ -> None)
  | Tree.Leaf -> Some 0L (* bare 0 stays Leaf (12.2 compat): the canonical zero reads *)
  | Tree.Stem _ -> None

(* L1 record: a list of (key (tag . %0)) pairs *)
let record_fields (recd : Tree.t) : (string * Tree.t) list =
  let rec go acc t =
    match t with
    | Tree.Leaf -> List.rev acc
    | Tree.Stem _ -> List.rev acc
    | Tree.Fork (Fork (keyt, Fork (valt, Tree.Leaf)), rest) -> (
        match canon keyt with
        | "10" -> go (("state", valt) :: acc) rest
        | "110" -> go (("title", valt) :: acc) rest
        | "1110" -> go (("who", valt) :: acc) rest
        | "11110" -> go (("when", valt) :: acc) rest
        | _ -> go acc rest)
    | _ -> List.rev acc
  in
  go [] recd

let field_str fields name ~default =
  match List.assoc_opt name fields with
  | Some v -> ( match Tuna.Cstr.decode v with Some s -> s | None -> default)
  | None -> default

let field_when fields =
  match List.assoc_opt "when" fields with
  | Some v -> ( match tree_to_int64 v with Some i -> i | None -> 0L)
  | None -> 0L

let field_state fields =
  match List.assoc_opt "state" fields with
  | Some v -> ( if canon v = "10" then "open" else "done")
  | None -> "?"

type row = {
  id : string
; title : string
; state : string
; who : string
; created : string
; version : string
}

type board = { rows : row list; opens : string; done_ct : string; last_path : string }

(* the view answer: (pair <entries> <summary>) - entries are the
   tree/list (path, value, version) shape, the summary the L1 record *)
let board_of_tree t =
  match t with
  | Tree.Fork (items, summary) -> (
      let fields = record_fields summary in
      let opens = match List.assoc_opt "state" fields with Some v -> (match tree_to_int64 v with Some i -> Int64.to_string i | None -> "?") | None -> "?" in
      let done_ct = match List.assoc_opt "title" fields with Some v -> (match tree_to_int64 v with Some i -> Int64.to_string i | None -> "?") | None -> "?" in
      let rec go acc t =
        match t with
        | Tree.Leaf | Tree.Stem _ -> Some (List.rev acc)
        | Tree.Fork (Fork (pathT, Fork (valueT, verT)), rest) -> (
            match (Tuna.Cstr.decode pathT, Tuna.Cstr.decode verT) with
            | Some path, Some ver -> (
                let f = record_fields valueT in
                let id =
                  if String.length path > String.length prefix then
                    String.sub path (String.length prefix)
                      (String.length path - String.length prefix)
                  else path
                in
                go ({ id
                    ; title = field_str f "title" ~default:""
                    ; state = field_state f
                    ; who = field_str f "who" ~default:"?"
                    ; created =
                        iso_of_epoch (field_when f)
                    ; version = ver }
                     :: acc)
                  rest)
            | _ -> None)
        | _ -> None
      in
      match go [] items with
      | None -> None
      | Some rows ->
          let last_path = match List.rev rows with r :: _ -> prefix ^ r.id | [] -> "" in
          Some { rows; opens; done_ct; last_path })
  | _ -> None

(* -- the rim run (L4 narrow mint + L6 budget) ----------------------------- *)

let tree_str s = Tuna.Cstr.encode s

let mint_run pool user ~name inputs prims ~scope : (S.run * S.journal list, string) result =
  let rec mints acc = function
    | [] -> return (List.rev acc)
    | p :: rest ->
        S.mint_grant pool ~prim:p ~args_attenuation:"null"
          ~path_prefix:(if scope then Some prefix else None)
          ~caller:user.S.i_id ~minted_by:(Some user.S.i_id) ()
        >>= fun g -> mints (g.S.g_id :: acc) rest
  in
  mints [] prims >>= fun gids ->
  Board_seed.program_hash pool ~name >>= function
  | Error m ->
      let rec revoke_all = function
        | [] -> return (Error m)
        | g :: rest -> S.revoke_grant pool g >>= fun () -> revoke_all rest
      in
      revoke_all gids
  | Ok ph ->
      Run.execute_run pool ~caller:user.S.i_id ~program_hash:ph
        ~input_trees:inputs ~grant_ids:gids ~fuel:run_fuel
        ~size_cap:run_size_cap ()
      >>= fun r ->
      let rec revoke_all = function
        | [] ->
            (match r with
             | Ok x -> return (Ok x)
             | Error (_, m) -> return (Error m))
        | g :: rest -> S.revoke_grant pool g >>= fun () -> revoke_all rest
      in
      revoke_all gids

(* -- handlers ------------------------------------------------------------- *)

let banner pool req =
  match Web.query req "run" with
  | None | Some "" -> ""
  | Some run_id -> (
      S.fetch_run_resolved pool run_id >>= function
      | None -> {|<p class="err">no such run</p>|}
      | Some (_, row) ->
          let denials = row.S.r_denial_count in
          let verdict = match row.S.r_verify_status with Some v -> v | None -> "unverified" in
          let cls = match verdict with "verified" -> "ok" | "unverified" -> "" | _ -> "warn" in
          Printf.sprintf
            {|<p class="muted">last change: run %s %s (%d denial%s)</p>|}
            (L.link_run run_id) (L.badge cls verdict) denials
            (if denials = 1 then "" else "s"))

let view pool user req =
  banner pool req >>= fun banner_html ->
  let cursor = match Web.query req "after" with Some c -> c | None -> "" in
  mint_run pool user ~name:"board-view" [tree_str prefix; tree_str cursor]
    ["tree/list"; "math/add"] ~scope:true
  >>= (function
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
                          (Printf.sprintf "board view result unparseable at %d: %s" off msg)
                    | Ok t -> (
                    match board_of_tree t with
                    | None -> L.err_page ~user "board view answered a wrong shape"
                    | Some b -> (
                    let r (it : row) =
                      let state_badge =
                        if it.state = "done" then L.badge "ok" "done"
                        else L.badge "warn" "open"
                      in
                      let title_html =
                        if it.state = "done" then Printf.sprintf {|<s>%s</s>|} (L.esc it.title)
                        else L.esc it.title
                      in
                      let toggle_label = if it.state = "done" then "reopen" else "done" in
                      Printf.sprintf
                        {|<tr><td>%s</td><td>%s</td><td><code>%s</code></td><td><code>v%s</code></td><td class="muted">%s</td><td><form method="post" action="/todo/%s/state" style="display:inline"><button type="submit">%s</button></form> <form method="post" action="/todo/%s/del" style="display:inline"><button type="submit">del</button></form></td></tr>|}
                        state_badge title_html (L.esc it.who) it.version
                        (L.esc it.created)
                        (L.esc it.id) toggle_label (L.esc it.id)
                    in
                    let table =
                      if b.rows = [] then "<p class=\"muted\">nothing on the board yet</p>"
                      else
                        Printf.sprintf
                          {|<table><tr><th>state</th><th>item</th><th>by</th><th>ver</th><th>created</th><th>actions</th></tr>%s</table>|}
                          (String.concat "" (List.map r b.rows))
                    in
                    let pager =
                      if b.last_path <> "" then
                        Printf.sprintf
                          {|<p class="muted"><a href="/todo?after=%s">older cards…</a></p>|}
                          (L.esc b.last_path)
                      else ""
                    in
                    L.page ~user ~title:"todo"
                      (Printf.sprintf
                         {|<h2>todo</h2><p class="muted">every read and write on this board is a sabra program run over journaled, grant-gated tree prims (board.borg). %s open, %s done in this window.</p>%s%s%s<section><form method="post" action="/todo/add"><input name="title" placeholder="what needs doing" style="width:48ch" required> <button type="submit">add</button></form></section>|}
                         b.opens b.done_ct banner_html table pager)))))
            | st ->
                L.err_page ~user
                  (Printf.sprintf "the board program ended %s"
                     (S.Run_status.to_string st))))

let add pool user req =
  Web.form ~csrf:false req >>= function
  | `Wrong_content_type -> L.err_page ~user "bad form submission"
  | `Ok fields -> (
      let title = Option.value ~default:"" (List.assoc_opt "title" fields) in
      if String.trim title = "" then L.err_page ~user "a title is required"
      else
        let path = prefix ^ item_id () in
        let who = user.S.i_name in
        (match Tuna.Int_enc.of_decimal_atom (Int64.to_string (Int64.of_float (Unix.time ()))) with
         | None -> L.err_page ~user "when could not encode"
         | Some whenT ->
             mint_run pool user ~name:"board-add"
               [tree_str path; tree_str title; tree_str who; whenT]
               ["tree/put"] ~scope:true
             >>= (function
                   | Error m -> L.err_page ~user m
                   | Ok (row, _) -> (
                       match row.S.r_status with
                       | S.Run_status.Normal ->
                           Web.redirect req (Printf.sprintf "/todo?run=%s" row.S.r_id)
                       | st ->
                           L.err_page ~user
                             (Printf.sprintf "add run %s ended %s"
                                row.S.r_id (S.Run_status.to_string st))))))

let act pool user req ~name ~prims =
  let id = Web.param req "id" in
  if not (valid_id id) then L.not_found ~user ("no todo item " ^ id)
  else
    mint_run pool user ~name [tree_str (prefix ^ id)] prims ~scope:true
    >>= (function
          | Error m -> L.err_page ~user m
          | Ok (row, _) -> (
              match row.S.r_status with
              | S.Run_status.Normal ->
                  Web.redirect req (Printf.sprintf "/todo?run=%s" row.S.r_id)
              | st ->
                  L.err_page ~user
                    (Printf.sprintf "%s run %s ended %s" name row.S.r_id
                       (S.Run_status.to_string st))))

let set_state pool user req = act pool user req ~name:"board-flip" ~prims:["tree/list"; "tree/get"; "tree/cas"]

let del pool user req = act pool user req ~name:"board-del" ~prims:["tree/del"]
