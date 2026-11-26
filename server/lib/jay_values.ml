(* Jay_values: values and types of Barry Jay's typed tree calculus over
   the ontology O_bin (borg/typing.borg TY2/TY3).

   The encodings are exactly those of reference/jay-typing-rules/
   export/TyBin.v (whose soundness, O_bin_typing_sound, is proved there)
   and his booleans.v; tests/tool_language_tests.ml checks them against
   the trees his Rocq development exported.  The checker is his infer
   over O_bin, embedded from reference/jay-typing-rules/natives. *)

open Tuna.Tree

type ty = Int | Bool

(* his notations: label[n], tagged[f,n], 'I, 'fst, 'snd, 'S2, 'K1,
   pairs u '* v, g 'C f, and swap (department.v) *)
let label n = Fork (Stem (Stem Leaf), Fork (Leaf, n))
let tagged f n = Fork (Stem (Fork (Leaf, f)), Fork (Stem (Stem Leaf), Fork (Leaf, n)))

let tag_of = function
  | Fork (Stem (Fork (Leaf, f)), Fork (Stem (Stem Leaf), Fork (Leaf, n))) -> Some (f, n)
  | _ -> None

let i = Fork (Stem (Stem Leaf), Stem Leaf)
let fst_ = Fork (Stem i, Fork (Leaf, Stem Leaf))
let snd_ = Fork (Stem i, Fork (Leaf, Fork (Leaf, i)))
let s2 u v = Fork (Stem u, v)
let k1 x = Fork (Leaf, x)
let pair u v = s2 (s2 i (k1 u)) (k1 v)
let swap p = s2 i (k1 p)
let compose g f = Fork (Stem (Fork (Leaf, g)), f)

(* names: his bool_nm; TyBin's bin_nm and int_nm *)
let bool_nm = Stem (Stem (Stem (Stem Leaf)))
let bin_nm = Fork (Leaf, Leaf)
let int_nm = Fork (Leaf, Stem Leaf)
let tt = tagged fst_ bool_nm
let ff = tagged snd_ bool_nm
let sel_e = compose fst_ snd_
let sel_o = compose snd_ snd_
let bzero = tagged fst_ bin_nm

let type_tree = function Int -> label int_nm | Bool -> label bool_nm
let name = function Int -> "int" | Bool -> "bool"

(* TyBin bin_pos: least significant bit outermost, no leading zero *)
let rec bin n =
  if n = 0 then bzero else tagged (s2 (if n land 1 = 1 then sel_o else sel_e) (k1 (bin (n lsr 1)))) bin_nm

(* TyBin int_of: sign tt (negative) / ff paired with the magnitude;
   zero is ff * bzero.  None for min_int (no magnitude in an int). *)
let int_tree n =
  if n = min_int then None
  else Some (tagged (swap (pair (if n < 0 then tt else ff) (bin (abs n)))) int_nm)

let bool_tree b = if b then tt else ff

(* inverses: any well-shaped value (a leading zero bit or -0 included),
   None for anything else or a magnitude past an OCaml int *)
let rec bin_value t depth =
  if depth > Sys.int_size then None (* bounds the recursion on a deep tree *)
  else
    match tag_of t with
    | Some (f, n) when n = bin_nm -> (
        if f = fst_ then Some 0
        else
          match f with
          | Fork (Stem sel, Fork (Leaf, m)) when sel = sel_e || sel = sel_o -> (
              match bin_value m (depth + 1) with
              | Some v when v <= max_int / 2 -> Some ((2 * v) + if sel = sel_o then 1 else 0)
              | _ -> None)
          | _ -> None)
    | _ -> None

let int_value t =
  match tag_of t with
  | Some (Fork (Stem i', Fork (Leaf, Fork (Stem (Fork (Stem i'', Fork (Leaf, s))), Fork (Leaf, b)))), n)
    when n = int_nm && i' = i && i'' = i && (s = tt || s = ff) ->
      Option.map (fun m -> if s = tt then -m else m) (bin_value b 0)
  | _ -> None

let bool_value t = if t = tt then Some true else if t = ff then Some false else None

(* the checker: infer[O_bin] *)
let checker_ternary =
  lazy
    (match List.assoc_opt "checker.O_bin" Tuna_interp.Jay_trees.all with
     | Some t -> t
     | None -> failwith "Jay_values: no checker.O_bin in Jay_trees")

let checker_hash = lazy (Tuna.Hash.hex_of_string (Lazy.force checker_ternary))
