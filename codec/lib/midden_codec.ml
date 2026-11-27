open Tuna.Tree
module C = Midden.Csexp
module S = Midden.Schema

let ( let* ) = Result.bind

let rec nat n = if n <= 0 then Leaf else Stem (nat (n - 1))

let rec nat_of = function
  | Leaf -> Some 0
  | Stem t -> Option.map succ (nat_of t)
  | Fork _ -> None

let int64_to_tree (n : int64) : Tuna.Tree.t =
  if n < 0L then
    let s = Int64.to_string n in
    Tuna.Int_enc.negative_of_decimal (String.sub s 1 (String.length s - 1))
  else Tuna.Int_enc.positive_of_decimal (Int64.to_string n)

let int64_of_tree (t : Tuna.Tree.t) : int64 option =
  match t with
  | Leaf -> Some 0L
  | _ ->
      let sign, bits = Tuna_prims.Math_prims.int_of_tree t in
      if List.length bits > 63 then None
      else
        let mag =
          List.fold_right
            (fun b acc -> Int64.add (Int64.mul acc 2L) (Int64.of_int b))
            bits 0L
        in
        let n = if sign = 1 then Int64.neg mag else mag in
        if int64_to_tree n = t then Some n else None

let field_tag (schema : S.schema) (name : string) : int option =
  let rec go i = function
    | [] -> None
    | (f : S.field) :: rest ->
        if String.equal f.name name then Some i else go (i + 1) rest
  in
  go 1 schema.fields

let is_optional (f : S.field) = List.mem S.Optional f.modifiers

let encode_atom (atom_ty : S.atom_type) (v : C.t) : (Tuna.Tree.t, string) result =
  match (atom_ty, v) with
  | (S.String | S.Time | S.Bytes), C.Atom s -> Ok (Tuna.Cstr.encode s)
  | S.Int, C.Atom s -> (
      match Int64.of_string_opt s with
      | Some n -> Ok (int64_to_tree n)
      | None -> Error (Printf.sprintf "invalid :int atom %S" s))
  | S.Bool, C.Atom "true" -> Ok (Stem Leaf)
  | S.Bool, C.Atom "false" -> Ok Leaf
  | S.Bool, C.Atom s -> Error (Printf.sprintf "invalid :bool atom %S" s)
  | S.Any, v -> Ok (Tuna.Cstr.encode (C.encode v))
  | _, _ -> Error "midden_codec: csexp value does not match atom type"

let encode_field_value (typ : S.field_type) (v : C.t) : (Tuna.Tree.t, string) result =
  match (typ, v) with
  | S.Atom at, v -> encode_atom at v
  | S.Ref _, C.Atom s -> Ok (Tuna.Cstr.encode s)
  | S.List at, C.List xs ->
      let* items =
        List.fold_left
          (fun acc x ->
            let* acc = acc in
            let* t = encode_atom at x in
            Ok (t :: acc))
          (Ok []) xs
      in
      Ok (List.fold_left (fun tl hd -> Fork (hd, tl)) Leaf items)
  | _, _ -> Error "midden_codec: csexp value does not match field type"

let decode_atom (atom_ty : S.atom_type) (t : Tuna.Tree.t) : (C.t, string) result =
  match atom_ty with
  | S.String | S.Time | S.Bytes -> (
      match Tuna.Cstr.decode t with
      | Some s -> Ok (C.Atom s)
      | None -> Error "midden_codec: expected Cstr tree")
  | S.Int -> (
      match int64_of_tree t with
      | Some n -> Ok (C.Atom (Int64.to_string n))
      | None -> Error "midden_codec: expected canonical Law-5 int tree")
  | S.Bool -> (
      match t with
      | Leaf -> Ok (C.Atom "false")
      | Stem Leaf -> Ok (C.Atom "true")
      | _ -> Error "midden_codec: expected Law-1 bool tree")
  | S.Any -> (
      match Tuna.Cstr.decode t with
      | None -> Error "midden_codec: expected Cstr csexp for :any"
      | Some s -> (
          match C.decode s with
          | Ok v -> Ok v
          | Error e -> Error ("midden_codec: :any " ^ C.string_of_error e)))

let decode_field_value (typ : S.field_type) (t : Tuna.Tree.t) : (C.t, string) result =
  match typ with
  | S.Atom at -> decode_atom at t
  | S.Ref _ -> (
      match Tuna.Cstr.decode t with
      | Some s -> Ok (C.Atom s)
      | None -> Error "midden_codec: expected Cstr UUID for :ref")
  | S.List at ->
      let rec go acc = function
        | Leaf -> Ok (C.List (List.rev acc))
        | Fork (hd, tl) ->
            let* v = decode_atom at hd in
            go (v :: acc) tl
        | Stem _ -> Error "midden_codec: expected Law-2 list tree"
      in
      go [] t

let csexp_to_tree (schema : S.schema) (doc : C.t) : (Tuna.Tree.t, string) result =
  match doc with
  | C.Atom _ -> Error "midden_codec: document must be a record list"
  | C.List raw ->
      let* entries =
        List.fold_left
          (fun acc item ->
            let* acc = acc in
            match item with
            | C.List [ C.Atom k; v ] -> Ok ((k, v) :: acc)
            | _ -> Error "midden_codec: malformed record entry")
          (Ok []) raw
      in
      let indexed =
        List.mapi (fun i (f : S.field) -> (i + 1, f)) schema.fields
      in
      let* pairs =
        List.fold_left
          (fun acc (tag, (f : S.field)) ->
            let* acc = acc in
            match List.assoc_opt f.name entries with
            | None ->
                if is_optional f then Ok acc
                else Error (Printf.sprintf "midden_codec: missing field %S" f.name)
            | Some v ->
                let* t = encode_field_value f.typ v in
                let wrapped = if is_optional f then Stem t else t in
                Ok ((tag, wrapped) :: acc))
          (Ok []) indexed
      in
      let pairs = List.rev pairs in
      Ok
        (List.fold_right
           (fun (tag, v) tl -> Fork (Fork (nat tag, Fork (v, Leaf)), tl))
           pairs Leaf)

let tree_to_csexp (schema : S.schema) (t : Tuna.Tree.t) : (C.t, string) result =
  let indexed =
    List.mapi (fun i (f : S.field) -> (i + 1, f)) schema.fields
  in
  let rec read_pairs acc = function
    | Leaf -> Ok (List.rev acc)
    | Fork (Fork (k, Fork (v, Leaf)), tl) -> (
        match nat_of k with
        | None -> Error "midden_codec: record key is not a small-nat tag"
        | Some tag -> read_pairs ((tag, v) :: acc) tl)
    | _ -> Error "midden_codec: expected dialect v1.1 record ([tag val] list)"
  in
  let* pairs = read_pairs [] t in
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (tag, v) -> Hashtbl.replace seen tag v)
    pairs;
  (* Check no unknown tags *)
  let* () =
    List.fold_left
      (fun acc (tag, _) ->
        let* () = acc in
        if List.mem_assoc tag indexed then Ok ()
        else Error (Printf.sprintf "midden_codec: unknown tag %d for schema %s" tag schema.name))
      (Ok ()) pairs
  in
  let* csexp_fields =
    List.fold_left
      (fun acc (tag, (f : S.field)) ->
        let* acc = acc in
        match Hashtbl.find_opt seen tag with
        | None ->
            if is_optional f then Ok acc
            else Error (Printf.sprintf "midden_codec: missing required field %S (tag %d)" f.name tag)
        | Some v ->
            if is_optional f then
              match v with
              | Leaf -> Ok acc
              | Stem inner ->
                  let* cv = decode_field_value f.typ inner in
                  Ok ((f.name, cv) :: acc)
              | Fork _ ->
                  (* Also tolerate unwrapped value for convenience if schema expects Fork? No, stay strict Law-3! *)
                  Error (Printf.sprintf "midden_codec: optional field %S must be Leaf or Stem" f.name)
            else
              let* cv = decode_field_value f.typ v in
              Ok ((f.name, cv) :: acc))
      (Ok []) indexed
  in
  match Midden.Record.canonical (List.rev csexp_fields) with
  | Error e -> Error ("midden_codec: " ^ Midden.Record.string_of_error e)
  | Ok doc -> (
      match S.validate_document schema doc with
      | Ok canonical -> Ok canonical
      | Error e -> Error ("midden_codec: " ^ S.string_of_error e))
