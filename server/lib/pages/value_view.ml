(* Tuna_server.Pages.Value_view: the /view/:hash page - a human window
   onto the content-addressed stores (tree_values, byte_values,
   programs) with the same probe order the API and the fed surface use
   (byte-values.borg law 1).  Read-only: every fact shown is derived
   from the hash itself or a store lookup, nothing is attributed.

   The point: a visitor who only has a hash (from a run page, a fed
   exchange, a route record) can see WHAT the hash names without the
   JSON API. *)

open Tuna_store.Direct

module S = Tuna_store.Store
module L = Layout

let preview_len = 384

(* printable-ASCII preview of a byte payload; everything else as \xNN *)
let bytes_preview b =
  let n = min (String.length b) preview_len in
  let buf = Buffer.create (n * 4) in
  String.iteri
    (fun i c ->
      if i >= n then ()
      else if c >= ' ' && c <= '~' && c <> '\\' then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c)))
    b;
  if String.length b > n then Buffer.add_string buf " ...";
  Buffer.contents buf

let api_note hash =
  Printf.sprintf
    {|<p class="muted">API: <code>GET /api/value/%s</code> · fed: <code>GET /api/fed/value/%s</code></p>|}
    (L.esc hash) (L.esc hash)

let view pool user req =
  let hash = String.lowercase_ascii (Web.param req "hash") in
  let title = "value " ^ L.short_hash hash in
  S.value_fetch pool hash
  >>= function
  | Some ternary ->
      let tree =
        match Tuna.Canon.of_string ternary with
        | Ok t -> Some t
        | Error _ -> None
      in
      let decoded =
        match tree with Some t -> Tree_svg.summary t | None -> "unparseable"
      in
      let kind_row =
        Printf.sprintf {|<p>%s <code>%s</code> · %d chars · <b>%s</b></p>|}
          (L.badge "ok" "tree") (L.esc hash) (String.length ternary)
          (L.esc decoded)
      in
      let tree_section =
        match tree with
        | Some t -> "<h3>tree</h3>" ^ Tree_svg.legend ^ Tree_svg.svg t
        | None -> ""
      in
      S.fetch_program pool hash
      >>= fun prog ->
      let plink =
        match prog with
        | Some _ ->
            Printf.sprintf
              {|<p>this hash is also a stored <b>program</b>: <a href="/programs/%s">open it</a></p>|}
              (L.esc hash)
        | None -> ""
      in
      let body =
        kind_row ^ plink ^ "<h3>canonical ternary</h3>"
        ^ L.code_block ternary ^ tree_section ^ api_note hash
      in
      L.page ~user ~title body
  | None ->
      S.byte_value_fetch pool hash
      >>= function
      | Some b ->
          let b64 = Base64.encode_string b in
          let b64_html =
            if String.length b64 > 4096 then String.sub b64 0 4096 ^ " ..."
            else b64
          in
          let kind_row =
            Printf.sprintf {|<p>%s <code>%s</code> · %d bytes</p>|}
              (L.badge "ok" "bytes") (L.esc hash) (String.length b)
          in
          let body =
            kind_row ^ "<h3>preview</h3>" ^ L.code_block (bytes_preview b)
            ^ "<h3>base64</h3>" ^ L.code_block b64_html ^ api_note hash
          in
          L.page ~user ~title body
      | None ->
          S.fetch_program pool hash
          >>= function
          | Some prog ->
              let ir =
                match prog.S.p_ir with
                | Some _ -> " · compiled (ir stored)"
                | None -> " · ternary-only"
              in
              let kind_row =
                Printf.sprintf
                  {|<p>%s <code>%s</code>%s · <a href="/programs/%s">full program page</a></p>|}
                  (L.badge "ok" "program") (L.esc hash) ir (L.esc hash)
              in
              let tree_section =
                match Tuna.Canon.of_string prog.S.p_ternary with
                | Ok t ->
                    Printf.sprintf
                      {|<details class="treebox"><summary>tree (%s)</summary>%s%s</details>|}
                      (L.esc (Tree_svg.summary t))
                      Tree_svg.legend (Tree_svg.svg t)
                | Error _ -> ""
              in
              let body =
                kind_row ^ "<h3>canonical ternary</h3>"
                ^ L.code_block prog.S.p_ternary ^ tree_section
              in
              L.page ~user ~title:("program " ^ L.short_hash hash) body
          | None ->
              L.not_found ~user
                (Printf.sprintf "no tree, byte value, or program with hash %s"
                   hash)
