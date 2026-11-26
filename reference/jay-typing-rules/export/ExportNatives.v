From BigStep Require Import tree typing soundness rows identifiers booleans functions updates data_types lists department.
From TunaTy Require Import TyBin.
From Stdlib Require Import String List PArith.
From Stdlib Require Ascii.
Import ListNotations.
Open Scope string_scope.
Fixpoint tern_acc (c : Combination) (acc : list Ascii.ascii) : list Ascii.ascii :=
  match c with
  | Lf => (Ascii.ascii_of_nat 48) :: acc
  | Sm a => (Ascii.ascii_of_nat 49) :: tern_acc a acc
  | Fk a b => (Ascii.ascii_of_nat 50) :: tern_acc a (tern_acc b acc)
  end.
Definition tern (c : Combination) : string := string_of_list_ascii (tern_acc c []).
Definition vec (label : string) (p : Combination) (args : list Combination) (r : Combination) : string :=
  label ++ " " ++ tern p ++ " " ++ String.concat "," (map tern args) ++ " " ++ tern r.

(* The trees fast3 registers (borg/eval-modes.borg E3).  A family is
   its program at the base parameter Lf and with each parameter in
   turn set to K. *)
Definition nat_line (name : string) (t : Combination) : string := name ++ " " ++ tern t.
Definition natives : list string := [
  nat_line "equal" equal;
  nat_line "is_concrete" is_concrete;
  nat_line "get_tagged" get_tagged;
  nat_line "get_label" get_label;
  nat_line "infer.0" (infer Lf);
  nat_line "infer.1" (infer 'K);
  nat_line "tp.0" (tp Lf);
  nat_line "tp.1" (tp 'K);
  nat_line "row.0" (row Lf Lf Lf);
  nat_line "row.1" (row 'K Lf Lf);
  nat_line "row.2" (row Lf 'K Lf);
  nat_line "row.3" (row Lf Lf 'K)
].
Redirect "natives" Eval vm_compute in natives.
