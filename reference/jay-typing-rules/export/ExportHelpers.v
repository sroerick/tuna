From BigStep Require Import tree typing soundness rows identifiers booleans functions updates data_types lists department.
From TunaTy Require Import TyBin.
From Stdlib Require Import String List PArith.
From Stdlib Require Ascii.
Import ListNotations.
Open Scope string_scope.
Print Assumptions O_bin_typing_sound.
Fixpoint tern_acc (c : Combination) (acc : list Ascii.ascii) : list Ascii.ascii :=
  match c with
  | Lf => (Ascii.ascii_of_nat 48) :: acc
  | Sm a => (Ascii.ascii_of_nat 49) :: tern_acc a acc
  | Fk a b => (Ascii.ascii_of_nat 50) :: tern_acc a (tern_acc b acc)
  end.
Definition tern (c : Combination) : string := string_of_list_ascii (tern_acc c []).
Definition vec (label : string) (p : Combination) (args : list Combination) (r : Combination) : string :=
  label ++ " " ++ tern p ++ " " ++ String.concat "," (map tern args) ++ " " ++ tern r.

(* fast3 (borg/eval-modes.borg E3): helper programs of his checker; each expected value is the one his
   spec lemma proves (equal_bt, is_concrete_spec, get_tagged_spec,
   get_label_spec, resv by extend_some / extend_none_value). *)
Definition eqv (p q : Combination) : Combination := bt (if combination_eq_dec p q then true else false).
Definition gtv (t : Combination) : Combination := match tag_of t with Some (f, n) => Sm (Fk f n) | None => Lf end.
Definition rsv (t : Combination) : Combination :=
  match tag_of t with Some (f, n) => Fk f n | None => match label_of t with Some n => Sm n | None => Lf end end.
Definition samples : list Combination :=
  [Lf; 'K; 'KI; Bin; Int; Bool; bzero; b5; tt; ff; zero; 'fst; 'snd; 'label[Lf]; 'tagged['K, Lf];
   'S2[sel_e, 'K1 Bin]; D_bin; Nat '* Nat; Fk (Sm (Sm Lf)) (Fk Lf Lf); Fk (Sm (Sm Lf)) (Fk (Sm Lf) Lf)].
Definition vectors : list string :=
  map (fun t => vec "is_concrete" is_concrete [t] (bt (concreteb t))) samples ++
  map (fun t => vec "get_tagged" get_tagged [t] (gtv t)) samples ++
  map (fun t => vec "get_label" get_label [t] (opt (label_of t))) samples ++
  map (fun t => vec "resv" resv [t] (rsv t)) samples ++
  flat_map (fun p => map (fun q => vec "equal" equal [p; q] (eqv p q)) [p; Bin; b5; Lf; 'fst]) samples.
(* row_head with rest = no_rows (whose value at n is K1 Lf), and
   rows_spec through whole tables *)
Definition rowv (m D n : Combination) : Combination := if combination_eq_dec m n then 'K1 (Sm D) else 'K1 Lf.
Definition names : list Combination := [bin_nm; int_nm; Lf; bool_nm; nat_nm; Fk Lf (Fk Lf Lf)].
Definition row_vectors : list string :=
  flat_map (fun m => map (fun n => vec "row_prog" row_prog [m; D_bin; no_rows; n] (rowv m D_bin n)) names) names ++
  map (fun n => vec "rows" (rows B_rows) [n; tp O_bin] (opt (lookup B_rows n))) names.
Redirect "helper_vectors" Eval vm_compute in (vectors ++ row_vectors)%list.
