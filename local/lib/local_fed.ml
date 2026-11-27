(* Tuna_local.Local_fed: the local directory as a federation peer
   (borg/local.borg L6).

   Sync reuses the existing /api/fed routes and adds none.  PUSH is one
   apply of the local ops window into ns/<laptop>/... with the values the
   effect rows cite carried inline: dictionary rows land at
   ns/<laptop>/dict/<identity>/<name>, a sealed deriv record at
   ns/<laptop>/runs/<run-id> (the local host writes that put row when it
   seals).  PULL walks the ops window of the owner's server dictionary
   (prefix dict/<owner>/), seq-paged 5000 rows at a time, and rehashes
   every value it takes (byte-values law 2: trust nothing but the hash).

   A record above the store's value size cap is a 413 that refuses the
   WHOLE apply before anything lands, so the tool resends the window
   without the rows citing that value (rows verify one by one, so
   dropping one is safe) and reports it by run id.

   DIVERGENCE: after a push or pull the tool compares
   ns/<laptop>/dict/... with the owner's server dictionary name by name
   and reports every mismatch with both hashes.  Nothing is overwritten:
   adopting one side is an explicit def, an ordinary op.

   ENROLMENT: peers are bearer identities the operator lists in
   TUNA_FED_PEERS; there is no self-enrolment.  The laptop holds its peer
   token (push) and its owner's bearer (pull of a private dictionary) in
   one local file (mode 600), read here as key=value lines. *)

module S = Local_store
module J = Yojson.Basic
module P = Tuna_prims.Prim_core

let starts_with pre s =
  String.length s >= String.length pre && String.sub s 0 (String.length pre) = pre

let trim_slash s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let find_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go 0

(* -- the token file: peer=<push token>, owner=<owner bearer> ---------- *)

(* the two tokens the chapter names: the peer bearer (push) and the
   owner's bearer (pull of a private dictionary).  Nothing else. *)
type tokens = { peer : string; owner : string }

let read_tokens path =
  let ic = open_in path in
  let lines =
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        let rec go acc =
          match input_line ic with
          | l -> go (l :: acc)
          | exception End_of_file -> List.rev acc
        in
        go [])
  in
  List.fold_left
    (fun (t : tokens) l ->
      match String.index_opt l '=' with
      | None -> t
      | Some i ->
          let k = String.trim (String.sub l 0 i) in
          let v =
            String.trim (String.sub l (i + 1) (String.length l - i - 1))
          in
          if k = "peer" then { t with peer = v }
          else if k = "owner" then { t with owner = v }
          else t)
    { peer = ""; owner = "" } lines

(* -- HTTP over a plain socket (HTTP/1.0, http only, as the prim host) -- *)

let http ~meth ~url ~token ?body () =
  match P.parse_url url with
  | None -> Error "only http:// URLs are supported"
  | Some (host, port, path) -> (
      try
        match
          Unix.getaddrinfo host (string_of_int port)
            [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
        with
        | [] -> Error ("cannot resolve host " ^ host)
        | ai :: _ ->
            let sock = Unix.socket ai.Unix.ai_family Unix.SOCK_STREAM 0 in
            Fun.protect ~finally:(fun () -> Unix.close sock) (fun () ->
                Unix.setsockopt_float sock Unix.SO_RCVTIMEO 60.;
                Unix.setsockopt_float sock Unix.SO_SNDTIMEO 60.;
                Unix.connect sock ai.Unix.ai_addr;
                let body = Option.value body ~default:"" in
                let req =
                  Printf.sprintf
                    "%s %s HTTP/1.0\r\nHost: %s\r\nAuthorization: Bearer \
                     %s\r\nContent-Type: application/json\r\n\
                     Content-Length: %d\r\nConnection: close\r\n\
                     User-Agent: tuna-fed/1\r\n\r\n%s"
                    meth path host token (String.length body) body
                in
                ignore (Unix.write_substring sock req 0 (String.length req));
                let buf = Buffer.create 65536 and chunk = Bytes.create 65536 in
                let rec read () =
                  let n = Unix.read sock chunk 0 (Bytes.length chunk) in
                  if n > 0 then (
                    Buffer.add_subbytes buf chunk 0 n;
                    read ())
                in
                read ();
                let resp = Buffer.contents buf in
                match String.index_opt resp ' ' with
                | None -> Error "malformed response"
                | Some i -> (
                    let code =
                      if String.length resp >= i + 4 then
                        int_of_string_opt (String.sub resp (i + 1) 3)
                      else None
                    in
                    match code with
                    | None -> Error "malformed status line"
                    | Some code -> (
                        match find_sub resp "\r\n\r\n" with
                        | None -> Ok (code, "")
                        | Some j ->
                            Ok
                              ( code
                              , String.sub resp (j + 4)
                                  (String.length resp - j - 4) ))))
      with
      | Unix.Unix_error (e, _, _) ->
          Error ("transport: " ^ Unix.error_message e)
      | Failure m -> Error m)

(* -- rows and their values --------------------------------------------- *)

let row_string j k =
  match J.Util.member k j with `String s -> Some s | _ -> None

let row_of_json (j : J.t) : (string * string option) option =
  match row_string j "path" with
  | None -> None
  | Some path -> (
      match J.Util.member "value_hash" j with
      | `String h -> Some (path, Some h)
      | _ -> Some (path, None))

(* the identity a bearer names: GET /api/whoami answers the name the
   dictionary prefix needs.  A token is not a name, so a pull that used
   the bearer as the path identity would match no row at all. *)
let whoami ~base ~token =
  match
    http ~meth:"GET" ~url:(trim_slash base ^ "/api/whoami") ~token ()
  with
  | Error e -> Error e
  | Ok (200, body) -> (
      match J.from_string body with
      | exception _ -> Error "whoami is not JSON"
      | j -> (
          match row_string j "name" with
          | Some name when name <> "" -> Ok name
          | _ -> Error "whoami names no identity"))
  | Ok (code, body) ->
      Error (Printf.sprintf "whoami answered %d: %s" code (String.trim body))

(* the dictionary a window's rows name, folded to name -> value hash (the
   undef marker removes a name, as the server's dict_fold does) *)
let fold_dict ~prefix rows =
  let n = String.length prefix in
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (path, vh) ->
      if String.length path > n && String.sub path 0 n = prefix then
        let name = String.sub path n (String.length path - n) in
        match vh with
        | Some h when h = S.undef_marker -> Hashtbl.remove tbl name
        | Some h -> Hashtbl.replace tbl name h
        | None -> ())
    rows;
  List.sort compare
    (Hashtbl.fold (fun name h acc -> (name, h) :: acc) tbl [])

(* -- divergence (L5's comparison of two name spaces) ------------------- *)

type divergence = {
  dv_name : string;
  dv_local : string option;
  dv_server : string option;
}

let divergences ~local ~server =
  let names =
    List.sort_uniq compare (List.map fst local @ List.map fst server)
  in
  List.filter_map
    (fun n ->
      let l = List.assoc_opt n local and s = List.assoc_opt n server in
      if l = s then None
      else Some { dv_name = n; dv_local = l; dv_server = s })
    names

let render_divergence d =
  Printf.sprintf "divergence %s local %s server %s" d.dv_name
    (Option.value d.dv_local ~default:"-")
    (Option.value d.dv_server ~default:"-")

(* -- the 413 retry: drop the rows citing the oversize value ------------ *)

(* the 413 body is JSON ({"error":"value <hash> exceeds the value cap"}),
   so the hash is found as the one 64-char hex run in the message rather
   than by splitting on spaces and expecting a bare "value <hex>" pair. *)
let oversize_hash msg =
  let is_hex c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  in
  let n = String.length msg in
  let rec window i =
    if i + 64 > n then None
    else
      let ok = ref true in
      for k = i to i + 63 do
        if not (is_hex msg.[k]) then ok := false
      done;
      let bounded =
        (i = 0 || not (is_hex msg.[i - 1]))
        && (i + 64 = n || not (is_hex msg.[i + 64]))
      in
      if !ok && bounded then Some (String.sub msg i 64) else window (i + 1)
  in
  window 0

let run_id_of_path path =
  let pre = "runs/" in
  if starts_with pre path && String.length path > String.length pre then
    String.sub path (String.length pre) (String.length path - String.length pre)
  else path

let dropped_by rows hash =
  List.filter_map
    (fun (o : S.op) -> if o.S.value_hash = Some hash then Some o.S.path else None)
    rows
  |> List.map (fun p -> (run_id_of_path p, hash))

type applied = { ap_rows : int; ap_dropped : (string * string) list }

(* resend the window without the rows citing a value the store refused
   (413).  [apply] answers the HTTP status and body; anything but 200 or
   a 413 naming a value is an error. *)
let push_rows ~apply rows =
  let rec go dropped rows =
    if rows = [] then Ok { ap_rows = 0; ap_dropped = List.rev dropped }
    else
      match apply rows with
      | Error e -> Error e
      | Ok (200, _) ->
          Ok { ap_rows = List.length rows; ap_dropped = List.rev dropped }
      | Ok (413, body) -> (
          match oversize_hash body with
          | None -> Error ("apply refused (413): " ^ String.trim body)
          | Some h ->
              let rows' =
                List.filter (fun (o : S.op) -> o.S.value_hash <> Some h) rows
              in
              if List.length rows' = List.length rows then
                Error
                  (Printf.sprintf
                     "apply refused (413) for value %s but no row of this \
                      window cites it" h)
              else
                go (List.rev_append (dropped_by rows h) dropped) rows')
      | Ok (code, body) ->
          Error
            (Printf.sprintf "apply refused (%d): %s" code (String.trim body))
  in
  go [] rows

(* -- push -------------------------------------------------------------- *)

let effect_hashes rows =
  List.filter_map (fun (o : S.op) -> o.S.value_hash) rows
  |> List.sort_uniq compare

let value_json ~src st h =
  (* the undef marker is the zero-length bytes value: the local store
     removes a name by citing it but keeps no bytes file for it, and fed
     apply needs the value inline to hold it, exactly as dict_op_undef
     seeds it server-side *)
  if h = S.undef_marker then
    Some
      (`Assoc
        [ ("hash", `String h); ("kind", `String "bytes")
        ; ("payload", `String "") ])
  else if src = "runs/" then
    (* sealed derivation records at runs/<id> are always raw bytes *)
    match S.get_bytes st h with
    | Some b ->
        Some
          (`Assoc
            [ ("hash", `String h); ("kind", `String "bytes")
            ; ("payload", `String (Base64.encode_string b)) ])
    | None -> None
  else
    match S.get_value st h with
    | Some t ->
        Some
          (`Assoc
            [ ("hash", `String h); ("kind", `String "tree")
            ; ("payload", `String (Tuna.Canon.encode t)) ])
    | None -> (
        match S.get_bytes st h with
        | Some b ->
            Some
              (`Assoc
                [ ("hash", `String h); ("kind", `String "bytes")
                ; ("payload", `String (Base64.encode_string b)) ])
        | None -> None)

(* the op-log groups a push carries: dictionary rows, sealed records and
   visibility rows, each into its own destination prefix so the applied
   path keeps the local shape (ns/<peer>/dict/<identity>/<name>) *)
let groups ~peer =
  [ ("dict/", "ns/" ^ peer ^ "/dict/"); ("runs/", "ns/" ^ peer ^ "/runs/");
    ("vis/", "ns/" ^ peer ^ "/vis/") ]

let push_group st ~base ~peer:_peer ~tokens:tk ~src ~dst =
  let rows =
    List.filter
      (fun (o : S.op) -> starts_with src o.S.path)
      (S.ops st)
  in
  if rows = [] then Ok { ap_rows = 0; ap_dropped = [] }
  else
    let apply rows =
      let values = List.filter_map (value_json ~src st) (effect_hashes rows) in
      let body =
        J.to_string
          (`Assoc
            [ ("src_prefix", `String src); ("dst_prefix", `String dst)
            ; ("ops", `List (List.map S.json_of_op rows))
            ; ("values", `List values) ])
      in
      match
        http ~meth:"POST"
          ~url:(trim_slash base ^ "/api/fed/ops/apply")
          ~token:tk.peer ~body ()
      with
      | Error e -> Error e
      | Ok (code, body) -> Ok (code, body)
    in
    push_rows ~apply rows

(* Also push the native Midden commit window from commits.csexp into
   ns/<peer> via POST /api/fed/midden/apply so the server's midden_commits
   and midden_heads directly mirror the laptop's Midden entities. *)
let push_midden_commits st ~base ~peer ~tokens:tk =
  let ml = S.midden st in
  let rows = Midden_local.Local_store.commits_with_prev ml in
  if rows = [] then Ok ()
  else
    let opt_s = function Some s -> `String s | None -> `Null in
    let opt_b64 = function
      | Some b -> `String (Base64.encode_string b)
      | None -> `Null
    in
    let commits_json =
      List.map
        (fun ((c : Midden.Log_commits.t), prev_chain) ->
          let schema_bytes =
            Option.bind c.schema (Midden_local.Local_store.get_blob ml)
          in
          let doc_bytes =
            Option.bind c.head (Midden_local.Local_store.get_blob ml)
          in
          `Assoc
            [ ("seq", `Int c.seq);
              ("entity", `String c.entity);
              ("prev", opt_s c.prev);
              ("head", opt_s c.head);
              ("schema", opt_s c.schema);
              ("author", `String c.author);
              ("actor", opt_s c.actor);
              ("reason", opt_s c.reason);
              ("tx", `String c.tx);
              ("at", `String c.at);
              ("chain", `String c.chain);
              ("prev_chain", opt_s prev_chain);
              ("schema_b64", opt_b64 schema_bytes);
              ("doc_b64", opt_b64 doc_bytes) ])
        rows
    in
    let body =
      J.to_string
        (`Assoc
          [ ("peer", `String peer); ("commits", `List commits_json) ])
    in
    match
      http ~meth:"POST"
        ~url:(trim_slash base ^ "/api/fed/midden/apply")
        ~token:tk.peer ~body ()
    with
    | Error e -> Error e
    | Ok (200, _) -> Ok ()
    | Ok (404, _) -> Ok ()
    | Ok (code, b) ->
        Error
          (Printf.sprintf "midden commit apply refused (%d): %s" code
             (String.trim b))

let push st ~base ~peer ~tokens:tk =
  if tk.peer = "" then Error "token file names no peer token (=...)"
  else
    let rec go sent dropped = function
      | [] -> (
          match push_midden_commits st ~base ~peer ~tokens:tk with
          | Error e -> Error e
          | Ok () -> Ok { ap_rows = sent; ap_dropped = List.rev dropped })
      | (src, dst) :: rest -> (
          match push_group st ~base ~peer ~tokens:tk ~src ~dst with
          | Error e -> Error e
          | Ok a ->
              go (sent + a.ap_rows) (List.rev_append a.ap_dropped dropped) rest)
    in
    go 0 [] (groups ~peer)

(* -- pull -------------------------------------------------------------- *)

let fetch_value st ~base ~token h =
  match
    http ~meth:"GET"
      ~url:(Printf.sprintf "%s/api/fed/value/%s" (trim_slash base) h)
      ~token ()
  with
  | Error e -> Error e
  | Ok (200, body) -> (
      match J.from_string body with
      | exception _ -> Error "value body is not JSON"
      | j -> (
          match (row_string j "kind", row_string j "payload") with
          | Some "tree", Some payload -> (
              match Tuna.Canon.of_string payload with
              | Error (_, m) ->
                  Error (Printf.sprintf "value %s is not canonical: %s" h m)
              | Ok t ->
                  if Tuna.Hash.hex_of_tree t <> h then
                    Error (Printf.sprintf "value %s does not rehash" h)
                  else (
                    ignore (S.put_value st t);
                    Ok ()))
          | Some "bytes", Some payload -> (
              match Base64.decode payload with
              | Error (`Msg m) -> Error m
              | Ok b ->
                  if Tuna.Hash.hex_of_string b <> h then
                    Error (Printf.sprintf "value %s does not rehash" h)
                  else (
                    ignore (S.put_bytes st b);
                    Ok ()))
          | _ -> Error (Printf.sprintf "value %s needs kind and payload" h)))
  | Ok (code, _) -> Error (Printf.sprintf "value %s answered %d" h code)

(* the value fetches a window needs: every hash its effect rows cite, each
   rehashed on receipt before anything keeps it *)
let rec fetch_values st ~base ~token = function
  | [] -> Ok ()
  | h :: rest -> (
      match fetch_value st ~base ~token h with
      | Error e -> Error e
      | Ok () -> fetch_values st ~base ~token rest)

let window_url ~base ~prefix ~after_seq =
  Printf.sprintf "%s/api/fed/ops?prefix=%s&after_seq=%d&limit=5000"
    (trim_slash base) prefix after_seq

(* walk the whole window from seq 0 in pages (the page's last_seq advances
   over rows this caller may not see, so the walk stays contiguous) *)
let pull_window st ~base ~token ~prefix =
  let rec pages after acc =
    match
      http ~meth:"GET" ~url:(window_url ~base ~prefix ~after_seq:after)
        ~token ()
    with
    | Error e -> Error e
    | Ok (code, body) when code <> 200 ->
        Error
          (Printf.sprintf "ops pull answered %d: %s" code (String.trim body))
    | Ok (_, body) -> (
        match J.from_string body with
        | exception _ -> Error "ops pull is not JSON"
        | j ->
            let ops =
              match J.Util.member "ops" j with `List l -> l | _ -> []
            in
            let rows = List.filter_map row_of_json ops in
            let hashes =
              List.filter_map (fun (_, vh) -> vh) rows
              |> List.sort_uniq compare
            in
            (match fetch_values st ~base ~token hashes with
            | Error e -> Error e
            | Ok () ->
                let last =
                  match J.Util.member "last_seq" j with
                  | `Int n -> n
                  | _ -> after
                in
                let more =
                  match J.Util.member "more" j with
                  | `Bool b -> b
                  | _ -> false
                in
                let acc = List.rev_append rows acc in
                if more && last > after then pages last acc
                else Ok (List.rev acc)))
  in
  pages 0 []

(* PULL: the owner's server dictionary, rehashed on receipt; the rows come
   back too, so the caller can report divergence against them *)
let pull st ~base ~tokens:tk =
  if tk.owner = "" then Error "token file names no owner bearer (=...)"
  else
    match whoami ~base ~token:tk.owner with
    | Error e -> Error e
    | Ok owner ->
        pull_window st ~base ~token:tk.owner ~prefix:("dict/" ^ owner ^ "/")

(* -- the divergence report --------------------------------------------- *)

let divergence_report st ~base ~peer ~tokens:tk =
  let local_prefix = "dict/" ^ S.identity st ^ "/" in
  let ns_prefix = "ns/" ^ peer ^ "/dict/" in
  match whoami ~base ~token:tk.owner with
  | Error e -> Error e
  | Ok owner -> (
  let server_prefix = "dict/" ^ owner ^ "/" in
  match pull_window st ~base ~token:tk.owner ~prefix:ns_prefix with
  | Error e -> Error e
  | Ok ns_rows -> (
      match pull_window st ~base ~token:tk.owner ~prefix:server_prefix with
      | Error e -> Error e
      | Ok server_rows ->
          let local = fold_dict ~prefix:local_prefix (List.map (fun (o : S.op) -> (o.S.path, o.S.value_hash)) (S.ops st)) in
          let server = fold_dict ~prefix:server_prefix server_rows in
          let pushed = fold_dict ~prefix:ns_prefix ns_rows in
          Ok (divergences ~local ~server, pushed)))
