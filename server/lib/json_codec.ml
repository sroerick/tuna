(* Json_codec: JSON <-> tuna trees through a declared type
   (borg/tool-language.borg T4).  A rim codec: the calculus never sees
   JSON.  The tree side follows the existing conventions exactly -
   stdlib law 1 bools, law 2 lists, law 3 options, law 4 small-nat
   tags, law 5 ints, Cstr strings, dialect records ([tag value]
   two-lists, sorted by tag) - so a sabra program reads an encoded
   request with the vocabulary it already has (rec-get, list-fold, ...).

   Trees are untyped (leaf is false, nil, none and int-free all at
   once), so BOTH directions are type-directed; nothing is guessed.
   The type language, as JSON:

     "bool" | "int" | "string" | "tag" | "tree"
     {"list": T}  {"option": T}
     {"record": {"<field>": [<tag>, T], ...}}
     {"enum": {"<name>": <tag>, ...}}
     {"jay": "int" | "bool"}

   "jay" (borg/typing.borg TY3) is a value typed in Barry Jay's system
   over the ontology O_bin, encoded as a tagged tree (Jay_values): a
   JSON int or bool, or {"ternary": "<tree>"} - a tree CLAIMED to have
   the type, which the tool endpoint checks with his infer before the
   run.  "tree" is the escape hatch: a raw ternary string either way.  Every
   error names the JSON path it happened at ($.obs.counts[2]); an
   unknown record field or enum name, a float, or a tree that does not
   have the declared shape is an error, never a silent default. *)

module J = Yojson.Basic  (* the server's JSON flavor (api.ml, repl_cmd.ml) *)
open Tuna.Tree

type ty =
  | Bool
  | Int
  | String
  | Tag
  | Tree
  | List of ty
  | Option of ty
  | Record of (string * int * ty) list  (* field, tag, type; sorted by tag *)
  | Enum of (string * int) list
  | Jay of Jay_values.ty

let max_tag = 4096
let ( let* ) = Result.bind

let rec nat n = if n = 0 then Leaf else Stem (nat (n - 1))

let rec nat_of = function
  | Leaf -> Some 0
  | Stem t -> Option.map succ (nat_of t)
  | Fork _ -> None

let tag_ok n = n >= 0 && n <= max_tag

(* -- the type language ------------------------------------------------ *)

let rec ty_of_json ?(path = "type") (j : J.t) : (ty, string) result =
  let bad m = Error (Printf.sprintf "%s: %s" path m) in
  match j with
  | `String "bool" -> Ok Bool
  | `String "int" -> Ok Int
  | `String "string" -> Ok String
  | `String "tag" -> Ok Tag
  | `String "tree" -> Ok Tree
  | `String s -> bad (Printf.sprintf "unknown type %S" s)
  | `Assoc [ ("list", t) ] ->
      let* t = ty_of_json ~path:(path ^ ".list") t in
      Ok (List t)
  | `Assoc [ ("option", t) ] ->
      let* t = ty_of_json ~path:(path ^ ".option") t in
      Ok (Option t)
  | `Assoc [ ("record", `Assoc fields) ] ->
      let* fs =
        List.fold_left
          (fun acc (name, spec) ->
            let* acc = acc in
            let fpath = Printf.sprintf "%s.record.%s" path name in
            match spec with
            | `List [ `Int tag; t ] when tag_ok tag ->
                let* t = ty_of_json ~path:fpath t in
                Ok ((name, tag, t) :: acc)
            | _ ->
                Error
                  (Printf.sprintf "%s: a field is [<tag 0..%d>, <type>]" fpath
                     max_tag))
          (Ok []) fields
      in
      let fs = List.sort (fun (_, a, _) (_, b, _) -> compare a b) fs in
      let rec dup = function
        | (a, x, _) :: ((b, y, _) :: _ as rest) ->
            if x = y then Some (a, b, x) else dup rest
        | _ -> None
      in
      (match dup fs with
       | Some (a, b, x) ->
           bad (Printf.sprintf "fields %s and %s share tag %d" a b x)
       | None -> Ok (Record fs))
  | `Assoc [ ("jay", `String "int") ] -> Ok (Jay Jay_values.Int)
  | `Assoc [ ("jay", `String "bool") ] -> Ok (Jay Jay_values.Bool)
  | `Assoc [ ("jay", _) ] -> bad "a jay type is \"int\" or \"bool\""
  | `Assoc [ ("enum", `Assoc names) ] ->
      let* ns =
        List.fold_left
          (fun acc (name, v) ->
            let* acc = acc in
            match v with
            | `Int tag when tag_ok tag -> Ok ((name, tag) :: acc)
            | _ ->
                Error
                  (Printf.sprintf "%s.enum.%s: a tag 0..%d" path name max_tag))
          (Ok []) names
      in
      let tags = List.sort compare (List.map snd ns) in
      if List.length (List.sort_uniq compare tags) <> List.length tags then
        bad "two enum names share a tag"
      else Ok (Enum (List.rev ns))
  | _ ->
      bad
        "expected \"bool\" | \"int\" | \"string\" | \"tag\" | \"tree\" | \
         {\"list\":T} | {\"option\":T} | {\"record\":{...}} | {\"enum\":{...}} | \
         {\"jay\":\"int\"|\"bool\"}"

(* -- ints (law 5) ---------------------------------------------------- *)

let int_tree_of_string s =
  if String.length s > 0 && s.[0] = '-' then
    Tuna.Int_enc.negative_of_decimal (String.sub s 1 (String.length s - 1))
  else Tuna.Int_enc.positive_of_decimal s

(* canonical law-5 int -> OCaml int, or None when the tree is not a
   canonical int or does not fit (JSON numbers here are OCaml ints) *)
let int_of_int_tree t =
  let sign, bits = Math_prims.int_of_tree t in
  if List.length bits > Sys.int_size - 2 then None
  else
    let mag = List.fold_right (fun b acc -> (acc * 2) + b) bits 0 in
    let n = if sign = 1 then -mag else mag in
    (* canonical only: re-encoding must give the same tree back *)
    if int_tree_of_string (string_of_int n) = t then Some n
    else None

(* -- encode: JSON -> tree -------------------------------------------- *)

let rec encode ?(path = "$") (ty : ty) (j : J.t) : (Tuna.Tree.t, string) result =
  let bad m = Error (Printf.sprintf "%s: %s" path m) in
  match (ty, j) with
  | Bool, `Bool b -> Ok (if b then Stem Leaf else Leaf)
  | Int, `Int n -> Ok (int_tree_of_string (string_of_int n))
  | Int, `Float _ -> bad "floats have no tuna encoding; send an int"
  | String, `String s -> Ok (Tuna.Cstr.encode s)
  | Tag, `Int n when tag_ok n -> Ok (nat n)
  | Tag, `Int _ -> bad (Printf.sprintf "a tag is 0..%d" max_tag)
  | Tree, `String t -> (
      match Tuna.Canon.of_string t with
      | Ok t -> Ok t
      | Error (off, m) -> bad (Printf.sprintf "bad ternary at %d: %s" off m))
  | List t, `List xs ->
      let* items =
        List.fold_left
          (fun acc (i, x) ->
            let* acc = acc in
            let* v = encode ~path:(Printf.sprintf "%s[%d]" path i) t x in
            Ok (v :: acc))
          (Ok []) (List.mapi (fun i x -> (i, x)) xs)
      in
      Ok (List.fold_left (fun tl hd -> Fork (hd, tl)) Leaf items)
  | Option _, `Null -> Ok Leaf
  | Option t, x ->
      let* v = encode ~path t x in
      Ok (Stem v)
  | Record fields, `Assoc kvs ->
      let* entries =
        List.fold_left
          (fun acc (k, x) ->
            let* acc = acc in
            match List.find_opt (fun (n, _, _) -> n = k) fields with
            | None -> Error (Printf.sprintf "%s.%s: unknown field" path k)
            | Some (_, tag, t) ->
                let* v = encode ~path:(path ^ "." ^ k) t x in
                Ok ((tag, v) :: acc))
          (Ok []) kvs
      in
      (* canonical record: entries sorted by tag; a JSON object with a
         repeated key is refused rather than resolved by order *)
      let entries = List.sort (fun (a, _) (b, _) -> compare a b) entries in
      let tags = List.map fst entries in
      if List.length (List.sort_uniq compare tags) <> List.length tags then
        bad "a field appears twice"
      else
        Ok
          (List.fold_right
             (fun (tag, v) tl -> Fork (Fork (nat tag, Fork (v, Leaf)), tl))
             entries Leaf)
  | Jay _, `Assoc [ ("ternary", `String t) ] -> (
      match Tuna.Canon.of_string t with
      | Ok t -> Ok t
      | Error (off, m) -> bad (Printf.sprintf "bad ternary at %d: %s" off m))
  | Jay Jay_values.Int, `Int n -> (
      match Jay_values.int_tree n with Some t -> Ok t | None -> bad "int out of range")
  | Jay Jay_values.Int, `Float _ -> bad "floats have no tuna encoding; send an int"
  | Jay Jay_values.Bool, `Bool b -> Ok (Jay_values.bool_tree b)
  | Enum names, `String s -> (
      match List.assoc_opt s names with
      | Some tag -> Ok (nat tag)
      | None -> bad (Printf.sprintf "unknown enum name %S" s))
  | _, _ -> bad "value does not match the declared type"

(* -- decode: tree -> JSON -------------------------------------------- *)

let rec decode ?(path = "$") (ty : ty) (t : Tuna.Tree.t) : (J.t, string) result =
  let bad m = Error (Printf.sprintf "%s: %s" path m) in
  match (ty, t) with
  | Bool, Leaf -> Ok (`Bool false)
  | Bool, Stem Leaf -> Ok (`Bool true)
  | Bool, _ -> bad "not a bool (leaf or stem leaf)"
  | Int, _ -> (
      match int_of_int_tree t with
      | Some n -> Ok (`Int n)
      | None -> bad "not a canonical int that fits a JSON integer")
  | String, _ -> (
      match Tuna.Cstr.decode t with
      | Some s -> Ok (`String s)
      | None -> bad "not a string")
  | Tag, _ -> (
      match nat_of t with Some n -> Ok (`Int n) | None -> bad "not a tag")
  | Tree, _ -> Ok (`String (Tuna.Canon.encode t))
  | List ty, _ ->
      let rec go i acc = function
        | Leaf -> Ok (`List (List.rev acc))
        | Fork (hd, tl) ->
            let* v = decode ~path:(Printf.sprintf "%s[%d]" path i) ty hd in
            go (i + 1) (v :: acc) tl
        | Stem _ -> bad "not a list"
      in
      go 0 [] t
  | Option _, Leaf -> Ok `Null
  | Option ty, Stem v -> decode ~path ty v
  | Option _, Fork _ -> bad "not an option (leaf or stem)"
  | Record fields, _ ->
      let rec go acc = function
        | Leaf -> Ok (`Assoc (List.rev acc))
        | Fork (Fork (k, Fork (v, Leaf)), tl) -> (
            match nat_of k with
            | None -> bad "a record key is not a tag"
            | Some tag -> (
                match List.find_opt (fun (_, g, _) -> g = tag) fields with
                | None -> bad (Printf.sprintf "record tag %d is not declared" tag)
                | Some (name, _, ty) ->
                    let* x = decode ~path:(path ^ "." ^ name) ty v in
                    go ((name, x) :: acc) tl))
        | _ -> bad "not a record (a list of [tag value] two-lists)"
      in
      go [] t
  | Jay Jay_values.Int, _ -> (
      match Jay_values.int_value t with
      | Some n -> Ok (`Int n)
      | None -> bad "not a Jay Int (tagged sign * Bin) that fits a JSON integer")
  | Jay Jay_values.Bool, _ -> (
      match Jay_values.bool_value t with Some b -> Ok (`Bool b) | None -> bad "not a Jay Bool (tt or ff)")
  | Enum names, _ -> (
      match nat_of t with
      | None -> bad "not an enum tag"
      | Some n -> (
          match List.find_opt (fun (_, g) -> g = n) names with
          | Some (name, _) -> Ok (`String name)
          | None -> bad (Printf.sprintf "tag %d is not a declared enum name" n)))
