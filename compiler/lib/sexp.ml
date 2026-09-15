(* Tuna surface language: a small s-expression lambda calculus.

   Grammar (whitespace, ";" line comments):
     <term> ::= (lambda (<var>+) <term>)   multi-arg sugar for nested lambdas
              | (<term> <term>+)           left-assoc application
              | <var>                      identifier (- _ a-z A-Z 0-9, not all ternary digits)
              | 0                          leaf literal
              | %<ternary>                 literal tree by ternary encoding

   The reader produces the IR directly (named lambdas, a source span
   and a unique id on every node — ir.ml) and enforces closure at read
   time: unbound variables raise Ir.Error carrying the occurrence's IR
   path. No defines at this layer (the REPL adds a dictionary later —
   see the ?dictionary parameter). Diagnostics address programs by IR
   path; source spans are a human courtesy only (borg/call-sites.borg,
   provenance). *)

exception Lex_error of int * string

open Ir

(* ---------- lexer ---------- *)

let is_delim c =
  c = '(' || c = ')' || c = ';' || c = '%' || c = ' ' || c = '\t' || c = '\n'
  || c = '\r'

type token = LP | RP | Atom of int * string

let lex (src : string) : token list =
  let toks = ref [] in
  let push t = toks := t :: !toks in
  let n = String.length src in
  let i = ref 0 in
  while !i < n do
    let c = src.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
    else if c = ';' then
      while !i < n && src.[!i] <> '\n' do incr i done
    else if c = '(' then (
      push LP;
      incr i)
    else if c = ')' then (
      push RP;
      incr i)
    else if c = '%' then begin
      let start = !i in
      incr i;
      let j = ref !i in
      while !j < n && (src.[!j] = '0' || src.[!j] = '1' || src.[!j] = '2') do
        incr j
      done;
      if !j = !i then raise (Lex_error (start, "empty tree literal after %"));
      push (Atom (start, String.sub src start (!j - start)));
      i := !j
    end
    else begin
      let start = !i in
      while !i < n && not (is_delim src.[!i]) do incr i done;
      push (Atom (start, String.sub src start (!i - start)))
    end
  done;
  List.rev !toks

(* ---------- parser: tokens -> IR with spans + scope checking ---------- *)

(* A variable atom: identifier characters, not all digits and not all
   ternary digits (bare digit runs are ambiguous — they must be written
   with % to be tree literals).  Quoted strings are reserved for prim
   names inside (prim ...) and never name variables. *)
let is_var_atom a =
  let all_ternary =
    String.length a > 0 && String.for_all (fun c -> c = '0' || c = '1' || c = '2') a
  in
  let all_digits =
    String.length a > 0 && String.for_all (fun c -> c >= '0' && c <= '9') a
  in
  String.length a > 0
  && (not all_digits)
  && (not all_ternary)
  && String.for_all
       (fun c ->
         c = '-' || c = '_'
         || (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9'))
       a

type pending_unbound = { var_id : int; name : string; off : int }

(* parse ?dictionary: the REPL's name->tree dictionary (M9).  A free
   occurrence of a name in the dictionary is read as a tree LITERAL of
   the bound value, in place — capture-safe by construction, because a
   lambda parameter that shadows the name is already in [scope] when
   the occurrence is parsed (it becomes a Var, not a literal).  A free
   occurrence NOT in the dictionary is still a pending unbound error. *)
let parse ?(dictionary : (string * Tuna.Tree.t) list = []) (src : string) : Ir.t =
  let dict = Hashtbl.create 8 in
  List.iter (fun (n, t) -> Hashtbl.replace dict n t) dictionary;
  let toks = Array.of_list (lex src) in
  let n = Array.length toks in
  let pos = ref 0 in
  let id = ref 0 in
  let pending = ref [] in
  let fresh () = incr id; !id in
  let peek () = if !pos < n then Some toks.(!pos) else None in
  let advance () = incr pos in
  let expect_rp start what =
    match peek () with
    | Some RP -> advance ()
    | Some LP ->
        raise (Ir.Error ([], Printf.sprintf "%s: expected ')' at offset %d, found '('" what start))
    | Some (Atom (_, a)) ->
        raise (Ir.Error ([], Printf.sprintf "%s: expected ')' at offset %d, found %S" what start a))
    | None -> raise (Ir.Error ([], Printf.sprintf "%s: unterminated list at offset %d" what start))
  in
  let rec parse_term scope =
    match peek () with
    | None -> raise (Ir.Error ([], "unexpected end of input"))
    | Some LP -> (
        advance ();
        let start = !pos - 1 in
        match peek () with
        | Some (Atom (_, "lambda")) -> parse_lambda start scope
        | Some (Atom (_, "prim")) ->
            (* (prim "name" args...): boundary call. "prim" is reserved as
               the head atom of this form. *)
            advance ();
            let pname =
              match peek () with
              | Some (Atom (soff, s)) when
                  String.length s >= 2
                  && s.[0] = '"'
                  && s.[String.length s - 1] = '"' ->
                  advance ();
                  let nm = String.sub s 1 (String.length s - 2) in
                  if nm = "" then
                    raise
                      (Ir.Error
                         ([], Printf.sprintf "prim: empty name at offset %d" soff));
                  nm
              | Some (Atom (_, a)) ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf
                          "prim: expected a quoted name, found %S at offset %d" a
                          start))
              | Some RP ->
                  raise
                    (Ir.Error
                       ([], Printf.sprintf "prim: missing name at offset %d" start))
              | Some LP ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf
                          "prim: expected a quoted name at offset %d, found '('"
                          start))
              | None ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf "prim: unterminated form at offset %d" start))
            in
            let args = ref [] in
            let rec collect () =
              match peek () with
              | Some (Atom (_, _)) | Some LP ->
                  args := parse_term scope :: !args;
                  collect ()
              | _ -> ()
            in
            collect ();
            expect_rp start "prim";
            let sp : Ir.span = { off = start; len = !pos - start } in
            Prim { id = fresh (); span = sp; name = pname; args = List.rev !args }
        | _ ->
            (* application: head first, then args; (f a b) folds
               left-assoc as App(App(f,a),b). Every App shares the whole
               form's span (spans are a courtesy; IR paths are exact). *)
            let head = parse_term scope in
            let args = ref [] in
            let rec collect () =
              match peek () with
              | Some (Atom (_, _)) | Some LP ->
                  args := parse_term scope :: !args;
                  collect ()
              | _ -> ()
            in
            collect ();
            expect_rp start "application";
            let sp : Ir.span = { off = start; len = !pos - start } in
            List.fold_left
              (fun acc arg -> App { id = fresh (); span = sp; fn = acc; arg })
              head (List.rev !args))
    | Some RP ->
        raise (Ir.Error ([], Printf.sprintf "unexpected ')' at offset %d" !pos))
    | Some (Atom (off, a)) -> (
        advance ();
        match a.[0] with
        | '%' ->
            let tern = String.sub a 1 (String.length a - 1) in
            (match Tuna.Canon.of_string tern with
            | Ok tree ->
                let sp : Ir.span = { off; len = String.length a } in
                Tree_lit ({ id = fresh (); span = sp; tree })
            | Error (o, msg) ->
                raise
                  (Ir.Error
                     ([],
                      Printf.sprintf "bad tree literal at offset %d: %s" (off + 1 + o)
                        msg)))
        | _ ->
            if a = "0" then
              let sp : Ir.span = { off; len = 1 } in
              Leaf_lit ({ id = fresh (); span = sp })
            else if is_var_atom a then (
              match List.find_opt (fun name -> name = a) scope with
              | Some _ ->
                  let sp : Ir.span = { off; len = String.length a } in
                  Var ({ id = fresh (); span = sp; name = a })
              | None -> (
                  (try
                     (* dictionary-bound name (M9): the REPL's defines are
                        read as literal trees — compile IS reduction keeps
                        holding, and the value's tree appears in the tags
                        under the occurrence's own span *)
                     let tree = Hashtbl.find dict a in
                     let sp : Ir.span = { off; len = String.length a } in
                     Tree_lit ({ id = fresh (); span = sp; tree })
                   with Not_found ->
                     let vid = fresh () in
                     pending :=
                       { var_id = vid; name = a; off } :: !pending;
                     (* placeholder node; will abort after the walk below *)
                     Var ({ id = vid; span = { off; len = String.length a }; name = a }))))
            else
              raise
                (Ir.Error
                   ([],
                    Printf.sprintf
                      "unexpected token %S at offset %d (tree literals need %%)" a
                      off)))
  and parse_lambda start scope =
    advance () (* 'lambda' *);
    let params = ref [] in
    (match peek () with
    | Some LP -> advance ()
    | _ ->
        raise
          (Ir.Error
             ([],
              Printf.sprintf "lambda: expected '(' before parameter list at offset %d" start)));
    let rec params_loop () =
      match peek () with
      | Some RP -> advance ()
      | Some (Atom (_, a)) when is_var_atom a ->
          advance ();
          params := a :: !params;
          params_loop ()
      | Some _ ->
          raise (Ir.Error ([], Printf.sprintf "lambda: bad parameter at offset %d" start))
      | None ->
          raise
            (Ir.Error ([], Printf.sprintf "lambda: unterminated parameter list at offset %d" start))
    in
    params_loop ();
    (match !params with
    | [] ->
        raise (Ir.Error ([], Printf.sprintf "lambda: empty parameter list at offset %d" start))
    | _ -> ());
    (* (lambda (x y ...) b) desugars to nested Lams, outermost param
       outermost. *)
    let rec desugar scope' = function
      | [] -> parse_term scope'
      | p :: rest -> Lam
          ({
             id = fresh ();
             span = { off = start; len = 0 } (* fixed up below *);
             param = p;
             body = desugar (p :: scope') rest;
           })
    in
    let lam = desugar scope (List.rev !params) in
    expect_rp start "lambda";
    let sp : Ir.span = { off = start; len = !pos - start } in
    (match lam with
     | Lam ({ span = _; _ } as r) -> Ir.Lam { r with span = sp }
     | _ -> assert false)
  in
  let t = parse_term [] in
  (match !pending with
  | [] -> ()
  | p :: _ ->
      let path = match Ir.find_id t p.var_id with Some pth -> pth | None -> [] in
      raise
        (Ir.Error (path, Printf.sprintf "unbound variable %S at offset %d" p.name p.off)));
  if !pos <> n then
    raise (Ir.Error ([], Printf.sprintf "trailing tokens after top-level term at offset %d" !pos));
  t
