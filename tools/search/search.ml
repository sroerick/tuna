(* Brute-force corpus hunt: enumerate trees by size, test behaviors.
   Target: add on intensional chain nats — n = Stem^n Leaf;
   want  add a b = Stem^(a+b) Leaf  (i.e. apply program to a then b).

   NOTE: run_prog's step accounting is intentionally simple (per-
   application cap via apply_cap); exact step totals come later from
   the reference generators, not from this hunt tool. *)

open Tuna.Tree

exception Over

let apply_cap cap a b =
  let steps = ref 0 in
  let rec go a b =
    incr steps;
    if !steps > cap then raise Over;
    match a with
    | Leaf -> Stem b
    | Stem a1 -> Fork (a1, b)
    | Fork (Leaf, a1) -> a1
    | Fork (Stem a1, a2) -> go (go a1 b) (go a2 b)
    | Fork (Fork (a1, a2), a3) ->
        (match b with
         | Leaf -> a1
         | Stem u -> go a2 u
         | Fork (u, v) -> go (go a3 u) v)
  in
  match go a b with
  | r -> Some r
  | exception Over -> None
  | exception Stack_overflow -> None

let chain n =
  let rec go k t = if k = 0 then t else go (k - 1) (Stem t) in
  go n Leaf

let rec gen n =
  if n = 1 then [ Leaf ]
  else
    let acc = ref [] in
    List.iter (fun c -> acc := Stem c :: !acc) (gen (n - 1));
    for i = 1 to n - 2 do
      let l = gen i and r = gen (n - 1 - i) in
      List.iter (fun a -> List.iter (fun b -> acc := Fork (a, b) :: !acc) r) l
    done;
    !acc

let cache : (int, t list) Hashtbl.t = Hashtbl.create 32

let gen_cached n =
  match Hashtbl.find_opt cache n with
  | Some t -> t
  | None ->
      let t = gen n in
      Hashtbl.add cache n t;
      t


(* eval-fold a program over args; every application must stay under cap *)
let run_prog cap p args =
  let rec go acc = function
    | [] -> Some acc
    | arg :: rest -> (
        match apply_cap cap acc arg with Some r -> go r rest | None -> None)
  in
  try go p args with Over -> None

let () =
  let cap = 5000 in
  (* cheap single-test filter: p 1 1 = 2, then confirm on more pairs *)
  for n = 2 to 16 do
    let trees = gen_cached n in
    Printf.printf "size %2d: %8d trees\n%!" n (List.length trees);
    List.iter
      (fun p ->
        if run_prog cap p [ chain 1; chain 1 ] = Some (chain 2) then
          if
            run_prog cap p [ chain 1; chain 2 ] = Some (chain 3)
            && run_prog cap p [ chain 2; chain 3 ] = Some (chain 5)
            && run_prog cap p [ chain 4; chain 0 ] = Some (chain 4)
            && run_prog cap p [ chain 0; chain 7 ] = Some (chain 7)
          then Printf.printf "ADD CANDIDATE size %d: %s\n%!" n (Tuna.Canon.to_string p))
      trees
  done
