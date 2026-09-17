(* Tuna_server.Pages.Layout: the page shell + small HTML helpers.

   Everything is server-rendered strings (PP-style); htmx progressively
   enhances — every interactive control also works as a plain form/POST
   ending in a redirect, so the UI degrades to pure HTML with JS
   disabled.  htmx requests (HX-Request: true) get fragments instead.

   Responders here return response promises directly. *)

module S = Tuna_store.Store

let esc = Dream.html_escape

let is_htmx req =
  match Dream.header req "HX-Request" with
  | Some "true" -> true
  | _ -> false

let short_hash h = if String.length h > 10 then String.sub h 0 10 else h

let badge class_ text =
  Printf.sprintf {|<span class="badge %s">%s</span>|} class_ (esc text)

let status_badge status =
  match status with
  | "normal" | "verified" -> badge "ok" status
  | "running" -> badge "warn" status
  | "failed" | "error" -> badge "bad" status
  | "fuel_exhausted" | "size_exhausted" | "deadline_exceeded" ->
      badge "warn" status
  | _ -> badge "muted" status

let verify_badge = function
  | Some "verified" -> badge "ok" "verified"
  | Some "failed" -> badge "bad" "verify failed"
  | Some other -> badge "muted" other
  | None -> badge "muted" "unverified"

let code_block s = Printf.sprintf {|<pre class="code">%s</pre>|} (esc s)

let link_run id = Printf.sprintf {|<a href="/runs/%s">%s…</a>|} id (esc (short_hash id))
let link_program h =
  Printf.sprintf {|<a href="/programs/%s">%s…</a>|} h (esc (short_hash h))

(* Pretty ASCII outline of a tree (no-JS friendly). *)
let tree_outline (t : Tuna.Tree.t) : string =
  let buf = Buffer.create 512 in
  let add = Buffer.add_string buf in
  let rec go ~pre ~cont = function
    | Tuna.Tree.Leaf -> add (pre ^ "leaf\n")
    | Tuna.Tree.Stem a ->
        add (pre ^ "stem\n");
        go ~pre:(cont ^ "\xe2\x94\x94\xe2\x94\x80 ") ~cont:(cont ^ "   ") a
    | Tuna.Tree.Fork (a, b) ->
        add (pre ^ "fork\n");
        go ~pre:(cont ^ "\xe2\x94\x9c\xe2\x94\x80 ") ~cont:(cont ^ "\xe2\x94\x82  ") a;
        go ~pre:(cont ^ "\xe2\x94\x94\xe2\x94\x80 ") ~cont:(cont ^ "   ") b
  in
  go ~pre:"" ~cont:"" t;
  Buffer.contents buf

let nav user =
  let who =
    match user with
    | Some i ->
        Printf.sprintf {|<span class="who">%s</span> <a href="/logout">logout</a>|}
          (esc i.S.i_name)
    | None -> {|<a href="/login">login</a>|}
  in
  Printf.sprintf
    {|<nav><a href="/">runs</a> <a href="/repl">repl</a> <a href="/grants">grants</a> %s</nav>|}
    who

(* Full document; [user] is the signed-in identity (None shows a login link). *)
let page ?(code = 200) ?user ~title body =
  Dream.html ~code
    (Printf.sprintf
       {|<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>%s</title>
<script src="/static/htmx.min.js"></script>
<style>
body { font-family: monospace; margin: 1rem; background: #111; color: #ddd; }
a { color: #7fb4ca; }
nav { border-bottom: 1px solid #333; padding-bottom: .5rem; margin-bottom: 1rem; }
nav a, nav .who { margin-right: 1rem; }
table { border-collapse: collapse; width: 100%%; margin-bottom: 1rem; }
th, td { text-align: left; padding: .15rem .6rem; border-bottom: 1px solid #222; }
pre.code { background: #181818; padding: .6rem; overflow-x: auto; border: 1px solid #333; }
.badge { padding: 0 .4rem; border-radius: 3px; font-size: .85em; }
.ok { background: #1d3321; color: #a6d189; }
.warn { background: #3a2d1b; color: #dbb65f; }
.bad { background: #3a1d1d; color: #e67172; }
.muted { background: #222; color: #888; }
input, textarea, select { background: #181818; color: #ddd; border: 1px solid #333; font-family: inherit; }
button { background: #222; color: #ddd; border: 1px solid #444; padding: .2rem .8rem; cursor: pointer; }
section { margin-bottom: 1.5rem; }
h3 { margin: .4rem 0; }
.who { color: #9cc; }
.err { color: #e67172; }
.okmsg { color: #a6d189; }
code { color: #c9a; }
</style>
</head>
<body>
%s
<main>
%s
</main>
</body>
</html>|}
       (esc title) (nav user) body)

(* 404 rendered inside the shell (no-JS navigable error surface). *)
let not_found ?(user : S.identity option) what =
  page ?user ~code:404 ~title:"not found"
    (Printf.sprintf
       {|<h2>404</h2><p class="err">%s</p><p><a href="/">back to runs</a></p>|}
       (esc what))

let err_page ?(code = 400) ?(user : S.identity option) msg =
  page ?user ~code ~title:"error"
    (Printf.sprintf {|<h2>error</h2><p class="err">%s</p><p><a href="/">back</a></p>|} (esc msg))
