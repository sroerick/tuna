(* Tuna surface language: a small s-expression lambda calculus.

   Grammar (whitespace, ";" line comments):
     <term> ::= (lambda (<var>+) <term>)   multi-arg sugar for nested lambdas
              | (let ((<var> <term>)+) <term>)   sequential let (desugars)
              | (<term> <term>+)           left-assoc application
              | <var>                      identifier (- _ a-z A-Z 0-9, not all ternary digits)
              | 0                          leaf literal
              | <digits>                   integer literal -> canonical int law 5
              | -<digits>                  negative integer literal (law 5)
              | %<ternary>                 literal tree by ternary encoding
              | [<term>*]                  list construction -> cons applications
              | "..."                      string literal -> the Cstr string tree

   Dialect builtins (reader-level, shadowable by a lambda param or a
   dictionary entry): pair/cons alias the leaf, which is extensionally
   the fork constructor (apply Leaf a = Stem a; apply (Stem a) b =
   Fork (a, b) — both wrapper applications, zero triage steps).

   Dialect sugar (borg/dialect.borg) is a TOTAL reader fold into this
   grammar: brackets desugar to pair-applications, decimal atoms to
   Tree_lit of the canonical law-5 int (Tuna.Int_enc), let to nested
   lambda applications, and keyed literals {...} to the bracket list of
   [key value] pairs the record vocabulary already reads (a :name key
   reads as the dictionary name key-name; borg/dialect.borg v0.2).  A
   sugar form and its hand-written twin compile to the identical tree
   and step count (acceptance 12.1). *)

exception Lex_error of int * string

open Ir

(* ---------- lexer ---------- *)

let is_delim c =
  c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}' || c = ':'
  || c = ';' || c = '%' || c = ' ' || c = '\t' || c = '\n' || c = '\r'

type token = LP | RP | LB | RB | LC | RC | Atom of int * string


(* [lex_with_spans] is the ONE scanner (the F13 single-reader law): the
   token stream the parser consumes, plus each token's char span
   (start, length).  Provenance spans are TOKEN-GRID (off = token
   index, len = token count -- the parser counts tokens, not chars), so
   resolving a span to text re-lexes the retained source through this
   table instead of keeping a second lexer that could drift. *)
let lex_with_spans (src : string) : token list * (int * int) list =
  let toks = ref [] in
  let spans = ref [] in
  let push ~start ~len t =
    toks := t :: !toks;
    spans := (start, len) :: !spans
  in
  let n = String.length src in
  let i = ref 0 in
  while !i < n do
    let c = src.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
    else if c = ';' then
      while !i < n && src.[!i] <> '\n' do incr i done
    else if c = '(' then (
      push ~start:!i ~len:1 LP;
      incr i)
    else if c = ')' then (
      push ~start:!i ~len:1 RP;
      incr i)
    else if c = '[' then (
      push ~start:!i ~len:1 LB;
      incr i)
    else if c = ']' then (
      push ~start:!i ~len:1 RB;
      incr i)
    else if c = '{' then (
      push ~start:!i ~len:1 LC;
      incr i)
    else if c = '}' then (
      push ~start:!i ~len:1 RC;
      incr i)
    else if c = ':' then begin
      (* a keyed-literal key: ":name" lexes as an atom spelled with the
         leading colon; the parser maps it to the dictionary name
         "key-name" inside {...}. *)
      let start = !i in
      incr i;
      let j = ref !i in
      while
        !j < n
        && src.[!j] <> ':' && src.[!j] <> '{' && src.[!j] <> '}'
        && src.[!j] <> '[' && src.[!j] <> ']' && src.[!j] <> '(' && src.[!j] <> ')'
        && not (src.[!j] = ' ' || src.[!j] = '\t' || src.[!j] = '\n' || src.[!j] = '\r')
      do incr j done;
      if !j = !i then raise (Lex_error (start, "empty key after ':'"));
      push ~start ~len:(!j - start) (Atom (start, String.sub src start (!j - start)));
      i := !j
    end
    else if c = '%' then begin
      let start = !i in
      incr i;
      let j = ref !i in
      while !j < n && (src.[!j] = '0' || src.[!j] = '1' || src.[!j] = '2') do
        incr j
      done;
      if !j = !i then raise (Lex_error (start, "empty tree literal after %"));
      push ~start ~len:(!j - start) (Atom (start, String.sub src start (!j - start)));
      i := !j
    end
    else begin
      let start = !i in
      if src.[start] = '"' then begin
        (* quoted string literal: consume to the closing quote, allow
           spaces (unlike identifiers).  Unterminated -> lex error. *)
        incr i;
        while !i < n && src.[!i] <> '"' do incr i done;
        if !i >= n then raise (Lex_error (start, "unterminated string literal"));
        incr i  (* closing quote *)
      end else
        while !i < n && not (is_delim src.[!i]) do incr i done;
      push ~start ~len:(!i - start) (Atom (start, String.sub src start (!i - start)))
    end
  done;
  (List.rev !toks, List.rev !spans)

let lex (src : string) : token list = fst (lex_with_spans src)

(* ---------- parser: tokens -> IR with spans + scope checking ---------- *)

(* A variable atom: identifier characters, not all digits and not all
   ternary digits (bare digit runs are ambiguous — they must be written
   with % to be tree literals).  Quoted strings lex as their own atoms
   (prim names inside (prim ...), string literals as terms). *)
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

(* Dialect builtins (reader-level, shadowable: a lambda param or a
   dictionary entry wins over these).  pair/cons alias the leaf, which
   is extensionally the fork constructor: apply Leaf a = Stem a and
   apply (Stem a) b = Fork (a, b) are both wrapper applications with
   zero triage steps, so (pair x y) = Fork (x, y) for free. *)
let builtin_tree = function
  | "pair" | "cons" -> Some Tuna.Tree.Leaf
  | _ -> None

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
    | Some RB ->
        raise (Ir.Error ([], Printf.sprintf "%s: expected ')' at offset %d, found ']'" what start))
    | Some LB ->
        raise (Ir.Error ([], Printf.sprintf "%s: expected ')' at offset %d, found '['" what start))
    | Some RC ->
        raise (Ir.Error ([], Printf.sprintf "%s: expected ')' at offset %d, found '}'" what start))
    | Some LC ->
        raise (Ir.Error ([], Printf.sprintf "%s: expected ')' at offset %d, found '{'" what start))
    | None -> raise (Ir.Error ([], Printf.sprintf "%s: unterminated list at offset %d" what start))
  in
    let resolve_atom scope off (a : string) : Ir.t =
    (* resolve a bare name: lambda scope > dictionary > builtin > pending
       unbound.  Shared by ordinary occurrences and keyed-literal keys
       ("key-<name>"), so a :name key reads exactly as the atom key-name. *)
    if List.find_opt (fun name -> name = a) scope <> None then
      Var ({ id = fresh (); span = { off; len = String.length a }; name = a })
    else
      try
        let tree = Hashtbl.find dict a in
        Tree_lit ({ id = fresh (); span = { off; len = String.length a }; tree })
      with Not_found -> (
        match builtin_tree a with
        | Some tree ->
            Tree_lit ({ id = fresh (); span = { off; len = String.length a }; tree })
        | None ->
            let vid = fresh () in
            pending := { var_id = vid; name = a; off } :: !pending;
            Var ({ id = vid; span = { off; len = String.length a }; name = a }))
  in
  (* [a b c] and every keyed-literal pair share this construction: the
     cons chain (pair a (pair b (pair c 0))).  pair is the Leaf literal,
     the terminal 0 the leaf literal. *)
  let pair_chain sp (elems : Ir.t list) : Ir.t =
    let pair_head () = Tree_lit { id = fresh (); span = sp; tree = Tuna.Tree.Leaf } in
    let tail = Leaf_lit { id = fresh (); span = sp } in
    List.fold_right
      (fun e acc ->
        App
          { id = fresh (); span = sp
          ; fn = App { id = fresh (); span = sp; fn = pair_head (); arg = e }
          ; arg = acc })
      elems tail
  in
  let rec parse_term scope =
      match peek () with
      | None -> raise (Ir.Error ([], "unexpected end of input"))
      | Some LP -> (
          advance ();
          let start = !pos - 1 in
          match peek () with
          | Some (Atom (_, "lambda")) -> parse_lambda start scope
          | Some (Atom (_, "let")) -> parse_let start scope
          | Some (Atom (_, "runtime")) -> parse_runtime start scope
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
              | Some LB ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf
                          "prim: expected a quoted name at offset %d, found '['"
                          start))
              | Some RB ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf
                          "prim: expected a quoted name at offset %d, found ']'"
                          start))
              | Some LC ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf
                          "prim: expected a quoted name at offset %d, found '{'"
                          start))
              | Some RC ->
                  raise
                    (Ir.Error
                       ([],
                        Printf.sprintf
                          "prim: expected a quoted name at offset %d, found '}'"
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
              | Some (Atom (_, _)) | Some LP | Some LB | Some LC ->
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
              | Some (Atom (_, _)) | Some LP | Some LB | Some LC ->
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
    | Some RB ->
        raise (Ir.Error ([], Printf.sprintf "unexpected ']' at offset %d" !pos))
    | Some LB ->
        advance ();
        let start = !pos - 1 in
        let elems = ref [] in
        let rec collect () =
          match peek () with
          | Some RB -> advance ()
          | Some (Atom (_, _)) | Some LP | Some LB | Some LC ->
              elems := parse_term scope :: !elems;
              collect ()
          | Some RP ->
              raise
                (Ir.Error
                   ([], Printf.sprintf "[list]: expected ']' at offset %d, found ')'" start))
          | Some RC ->
              raise
                (Ir.Error
                   ([], Printf.sprintf "[list]: expected ']' at offset %d, found '}'" start))
          | None ->
              raise
                (Ir.Error
                   ([], Printf.sprintf "[list]: unterminated at offset %d" start))
        in
        collect ();
        let sp : Ir.span = { off = start; len = !pos - start } in
        pair_chain sp (List.rev !elems)
    | Some RC ->
        raise (Ir.Error ([], Printf.sprintf "unexpected '}' at offset %d" !pos))
    | Some LC ->
        (* {..}: a KEYED LITERAL (borg/dialect.borg v0.2).  Each entry
           is a :name key followed by a value term; the form desugars to
           the bracket list of [key-value value] two-lists the record
           vocabulary already reads.  A :name key resolves to the
           dictionary name "key-name" (scope > dictionary > builtin),
           exactly as a bare atom would, so the schema stays data.
           The desugar is [key value] = (pair key (pair value 0)). *)
        advance ();
        let start = !pos - 1 in
        let pairs = ref [] in
        let rec key_loop () =
          match peek () with
          | Some RC -> advance ()
          | Some (Atom (kot, k)) when String.length k > 0 && k.[0] = ':' -> (
              advance ();
              let name = String.sub k 1 (String.length k - 1) in
              let key_ir = resolve_atom scope kot ("key-" ^ name) in
              let val_ir =
                match peek () with
                | Some RC ->
                    raise
                      (Ir.Error
                         ([],
                          Printf.sprintf "{%s}: needs a value at offset %d" k
                            start))
                | _ -> parse_term scope
              in
              let sp : Ir.span = { off = kot; len = String.length k } in
              pairs := pair_chain sp [ key_ir; val_ir ] :: !pairs;
              key_loop ())
          | Some (Atom (_, a)) ->
              raise
                (Ir.Error
                   ( [],
                     Printf.sprintf
                       "{{}: expected a :key at offset %d, found %S" start a ))
          | _ ->
              raise
                (Ir.Error
                   ([],
                    Printf.sprintf
                      "{{}: expected '}' or a :key at offset %d" start))
        in
        key_loop ();
        let sp : Ir.span = { off = start; len = !pos - start } in
        pair_chain sp (List.rev !pairs)
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
              else if
                String.length a >= 2
                && a.[0] = '"'
                && a.[String.length a - 1] = '"'
              then
                (* string literal: the Cstr string tree (the same
                   encoding the prim boundary decodes).  Compiled IS
                   reduction keeps holding — the literal is already a
                   normal form. *)
                let s = String.sub a 1 (String.length a - 2) in
                let sp : Ir.span = { off; len = String.length a } in
                Tree_lit ({ id = fresh (); span = sp; tree = Tuna.Cstr.encode s })
              else if Tuna.Int_enc.of_decimal_atom a <> None then
                let tree = Option.get (Tuna.Int_enc.of_decimal_atom a) in
                let sp : Ir.span = { off; len = String.length a } in
                Tree_lit ({ id = fresh (); span = sp; tree })
              else if is_var_atom a then resolve_atom scope off a
              else
                raise
                  (Ir.Error
                     ([]
                     , Printf.sprintf
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

  (* (let ((x a) (y b)) B) desugars to the sequential lambda
     application ((lambda (x) ((lambda (y) B) b)) a): each binding's
     scope is its own body (and the later bindings), so lambda-param
     shadowing semantics apply for free.  The sugar is a total fold:
     the hand-written twin compiles to the identical tree and step
     count (borg/dialect.borg L2, acceptance 12.1). *)
  and parse_let start scope =
    advance () (* 'let' *);
    (match peek () with
     | Some LP -> advance ()
     | _ ->
         raise
           (Ir.Error
              ([],
               Printf.sprintf
                 "let: expected '(' before the binding list at offset %d" start)));
    let bindings = ref [] in
    let seen = ref scope in
    let rec binding_loop () =
      match peek () with
      | Some RP -> advance ()
      | Some LP ->
          advance ();
          let bstart = !pos - 1 in
          let name =
            match peek () with
            | Some (Atom (_, a)) when is_var_atom a ->
                advance ();
                a
            | _ ->
                raise
                  (Ir.Error
                     ([],
                      Printf.sprintf "let: expected a binding name at offset %d"
                        bstart))
          in
          (* each binding's rhs sees the bindings BEFORE it (sequential
             let), but not itself (that would be letrec, L5 gated). *)
          let rhs = parse_term !seen in
          expect_rp bstart "let binding";
          bindings := (name, rhs) :: !bindings;
          seen := name :: !seen;
          binding_loop ()
      | _ ->
          raise
            (Ir.Error
               ([], Printf.sprintf "let: malformed binding list at offset %d" start))
    in
    binding_loop ();
    (match !bindings with
     | [] ->
         raise
           (Ir.Error
              ([], Printf.sprintf "let: empty binding list at offset %d" start))
     | _ -> ());
    let body = parse_term (List.rev_map fst !bindings @ scope) in
    expect_rp start "let";
    let sp : Ir.span = { off = start; len = !pos - start } in
    (* bindings were collected in source order; desugar right-nested so
       the LAST binding's lambda is innermost (sequential scope). *)
    let rec desugar = function
      | [] -> body
      | (name, rhs) :: rest ->
          let inner = desugar rest in
          Ir.App
            { id = fresh (); span = sp
            ; fn = Ir.Lam { id = fresh (); span = sp; param = name; body = inner }
            ; arg = rhs }
    in
    desugar (List.rev !bindings)

  (* (runtime (prim "name" args...)): mark a prim call run-time-only.
     compile-IS must not fire it while compiling — prims only execute
     inside a run (grants + journal).  The only reliable protection is
     to make the call depend on a lambda-bound variable (a constant-
     args call in a closed term is what the compile guard rejects), so
     this desugars to threading the OUTERMOST enclosing lambda param
     through each constant argument via the K combinator: each open
     arg a becomes ((K a) outer) — K drops outer at run time, but its
     presence keeps the call non-closed for the compiler, exactly the
     manual/K pattern a program without this form had to write.  A
     zero-arg prim gets the outer param appended (now/uuid ignore
     args).  No enclosing lambda param (top-level) is a compile error:
     without it there is nothing to defer to. *)
  and parse_runtime start scope =
    advance () (* 'runtime' *);
    let term = parse_term scope in
    expect_rp start "runtime";
      match term with
      | Ir.Prim pr -> (
        match List.rev scope with
        | [] ->
            raise
              (Ir.Error
                 ([]
                , "runtime: a (prim ...) call needs an enclosing lambda \
                   parameter to defer it to run time"))
        | outer :: _ ->
            let sp : Ir.span = { off = start; len = !pos - start } in
            (* K = (lambda (k$1 k$2) k$1) — fresh names, can't collide *) 
            let k_lit () =
              Ir.Lam
                { id = fresh (); span = sp; param = "k$1"
                ; body =
                    Ir.Lam
                      { id = fresh (); span = sp; param = "k$2"
                      ; body = Ir.Var { id = fresh (); span = sp; name = "k$1" } } }
            in
            let rec has_var = function
              | Ir.Var _ -> true
              | Ir.Lam { body; _ } -> has_var body
              | Ir.App { fn; arg; _ } -> has_var fn || has_var arg
              | Ir.Prim { args; _ } -> List.exists has_var args
              | Ir.Leaf_lit _ | Ir.Tree_lit _ -> false
            in
            let thread arg =
              if has_var arg then arg
              else
                Ir.App
                  { id = fresh (); span = sp
                  ; fn = Ir.App { id = fresh (); span = sp; fn = k_lit (); arg }
                  ; arg = Ir.Var { id = fresh (); span = sp; name = outer } }
            in
              let args' =
                match pr.args with
                | [] -> [ Ir.Var { id = fresh (); span = sp; name = outer } ]
                | _ -> List.map thread pr.args
              in
            Ir.Prim { pr with args = args' })
    | _ ->
        raise
          (Ir.Error
             ([], "runtime: expected a (prim \"name\" ...) call"))
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
