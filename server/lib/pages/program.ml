(* Tuna_server.Pages.Program: the program page — ternary, pretty tree,
   provenance (tree path -> IR node -> span), a structural CAS patch
   form, and a "run it" form.  All controls are plain forms that work
   without JS; htmx enhances them in place (hx-target fragments). *)

open Lwt.Infix

module J = Yojson.Basic
module JU = Yojson.Basic.Util
module P = Patch
module S = Tuna_store.Store
module L = Layout

let view pool user req =
  let hash = Dream.param req "hash" in
  S.fetch_program pool hash
  >>= function
  | None ->
      L.not_found ~user
        (Printf.sprintf "no program with hash %s" hash)
  | Some p -> (
      let size =
        match Tuna.Canon.of_string p.S.p_ternary with
        | Ok t -> Tuna.Tree.size t
        | Error _ -> 0
      in
      (* provenance table from the ir column's tags (call-sites.provenance) *)
      let provenance =
        match p.S.p_ir with
        | None -> Lwt.return ""
        | Some ir_text -> (
            let tags =
              try
                (match J.from_string ir_text with
                | `Assoc fields ->
                    (match JU.member "tags" (`Assoc fields) with
                    | `List tags -> tags
                    | _ -> [])
                | _ -> [])
              with Yojson.Json_error _ -> []
            in
            let row = function
              | `Assoc fields -> (
                  let path = JU.member "path" (`Assoc fields) in
                  let ir = JU.member "ir" (`Assoc fields) in
                  let span = JU.member "span" (`Assoc fields) in
                  let path = match path with `String s -> s | _ -> "?" in
                  let ir = match ir with `Int i -> string_of_int i | _ -> "?" in
                  let span =
                    match span with
                    | `Assoc sp -> (
                        match (JU.member "off" (`Assoc sp), JU.member "len" (`Assoc sp)) with
                        | `Int o, `Int l -> Printf.sprintf "%d..%d" o (o + l)
                        | _ -> "?")
                    | _ -> "?"
                  in
                  Some (path, ir, span))
              | _ -> None
            in
            let rows =
              tags
              |> List.filter_map row
              |> List.map (fun (path, ir, span) ->
                     Printf.sprintf
                       {|<tr><td>/%s</td><td>node %s</td><td>span %s</td></tr>|}
                       (L.esc path) (L.esc ir) (L.esc span))
            in
            if rows = [] then Lwt.return ""
            else
              Lwt.return
                (Printf.sprintf
                   {|<section><h3>provenance</h3><table><tr><th>tree path</th><th>IR node</th><th>source span</th></tr>%s</table></section>|}
                   (String.concat "" rows)))
      in
      match Tuna.Canon.of_string p.S.p_ternary with
      | Error (off, msg) ->
          L.err_page ~code:500 ~user
            (Printf.sprintf "stored ternary unparseable (offset %d: %s)" off msg)
      | Ok tree ->
          provenance
          >>= fun prov ->
          L.page ~user ~title:"tuna — program"
               (Printf.sprintf
                  {|<h2>program %s…</h2>
<p>size %d &nbsp; created by <code>%s</code></p>
<section><h3>canonical ternary</h3>%s</section>
<section><h3>tree</h3>%s</section>
%s
<section><h3>patch (CAS)</h3>
<form hx-post="/programs/%s/patch" hx-target="#patch-result" method="post" action="/programs/%s/patch">
<p><label>path <input name="path" value="" placeholder="/0" style="width:12ch"/></label>
<label>expected old hash <input name="expected_old_hash" value="%s" style="width:64ch"/></label></p>
<p><label>new ternary (subtree)<br/><textarea name="new_ternary" rows="3" cols="70" placeholder="22102000"></textarea></label></p>
<button type="submit">apply patch</button>
</form>
<div id="patch-result"></div>
</section>
<section><h3>run it</h3>
<form hx-post="/programs/%s/run" hx-target="#run-result" method="post" action="/programs/%s/run">
<p><label>inputs (one ternary per line)<br/><textarea name="inputs" rows="3" cols="70" placeholder="10"></textarea></label></p>
<p><label>fuel <input name="fuel" value="10000" style="width:10ch"/></label>
<label>size cap <input name="size_cap" value="10000" style="width:10ch"/></label></p>
<p><label>grant ids (comma-separated, for prim calls)<br/><input name="grants" style="width:64ch"/></label></p>
<button type="submit">run</button>
</form>
<div id="run-result"></div>
</section>|}
                  (L.esc (L.short_hash p.S.p_hash))
                  size
                  (L.esc (Option.value p.S.p_created_by ~default:"—"))
                  (L.code_block p.S.p_ternary)
                  (L.code_block (L.tree_outline tree))
                  prov
                  hash hash
                  (L.esc p.S.p_hash)
                  hash hash))

(* --- patch POST ------------------------------------------------------ *)

let trim_path path =
  let path = String.trim path in
  if String.length path > 0 && path.[0] = '/' then
    String.sub path 1 (String.length path - 1)
  else path

let patch_post pool user req =
  let hash = Dream.param req "hash" in
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      let get k = List.assoc_opt k fields in
      match (get "path", get "expected_old_hash", get "new_ternary") with
      | Some path, Some expected, Some new_t -> (
          let parse s =
            match Tuna.Canon.of_string (String.trim s) with
            | Ok t -> Ok t
            | Error (off, msg) ->
                Error (Printf.sprintf "ternary parse error at offset %d: %s" off msg)
          in
          match parse new_t with
          | Error msg -> L.err_page ~user ("new ternary: " ^ msg)
          | Ok new_sub -> (
              S.fetch_program pool hash
              >>= function
              | None -> L.not_found ~user "unknown program hash"
              | Some prog -> (
                  match Tuna.Canon.of_string prog.S.p_ternary with
                  | Error (off, msg) ->
                      L.err_page ~code:500 ~user
                        (Printf.sprintf "ternary parse error at offset %d: %s" off msg)
                  | Ok old_tree -> (
                      match
                        P.apply_patch ~old_tree ~path:(trim_path path)
                          ~believed:None
                          ~expected_old_hash:(String.trim expected) ~new_sub
                      with
                      | P.Bad_path p ->
                          let html =
                            Printf.sprintf
                              {|<span class="err">path %S does not address a subtree</span>|}
                              (L.esc p)
                          in
                          if L.is_htmx req then Dream.html html
                          else L.err_page ~code:404 ~user html
                      | P.Conflict { expected_old_hash; actual_old_hash; first_diff }
                        ->
                          let html =
                            Printf.sprintf
                              {|<span class="err">patch conflict: subtree hash mismatch (expected %s…, actual %s…, first diff path /%s)</span>|}
                              (L.esc (L.short_hash expected_old_hash))
                              (L.esc (L.short_hash actual_old_hash))
                              (L.esc (Option.value first_diff ~default:""))
                          in
                          if L.is_htmx req then Dream.html html
                          else L.err_page ~code:409 ~user html
                      | P.Applied { ternary; hash = new_hash } ->
                          S.upsert_program pool ~hash:new_hash ~ternary ~ir:None
                            ~created_by:(Some user.S.i_id)
                          >>= fun _ ->
                          if L.is_htmx req then
                            Dream.html
                              (Printf.sprintf
                                 {|<span class="okmsg">applied — new program %s</span>|}
                                 (L.link_program new_hash))
                          else Dream.redirect req ("/programs/" ^ new_hash)))))
      | _ -> L.err_page ~user "patch needs path, expected_old_hash, new_ternary")
  | _ -> L.err_page ~user "bad form submission"

(* --- run POST -------------------------------------------------------- *)

let run_post pool user req =
  let hash = Dream.param req "hash" in
  Dream.form ~csrf:false req
  >>= function
  | `Ok fields -> (
      let get k = List.assoc_opt k fields in
      let int_of name dflt =
        match get name with
        | Some s -> (try int_of_string (String.trim s) with _ -> dflt)
        | None -> dflt
      in
      let fuel = int_of "fuel" 10_000 in
      let cap = int_of "size_cap" 10_000 in
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
      | Error msg -> L.err_page ~user msg
      | Ok input_trees -> (
          let grants =
            match get "grants" with
            | None -> []
            | Some s ->
                String.split_on_char ',' s
                |> List.map String.trim
                |> List.filter (fun s -> s <> "")
          in
          Run.execute_run pool ~caller:user.S.i_id ~program_hash:hash
            ~input_trees ~grant_ids:grants ~fuel ~size_cap:cap ()
          >>= (function
                | Error (_, msg) -> L.err_page ~user msg
                | Ok (row, _js) ->
                    if L.is_htmx req then
                      Dream.html
                        (Printf.sprintf
                           {|<span class="okmsg">ran — open run %s</span>|}
                           (L.link_run row.S.r_id))
                    else Dream.redirect req ("/runs/" ^ row.S.r_id))))
  | _ -> L.err_page ~user "bad form submission"
