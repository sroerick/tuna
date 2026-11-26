From BigStep Require Import tree typing soundness rows identifiers booleans functions updates data_types lists department.
From TunaTy Require Import TyInt.
From Stdlib Require Import String List PArith.
From Stdlib Require Ascii.
Import ListNotations.
Open Scope string_scope.
Print Assumptions O_int_typing_sound.
Fixpoint tern_acc (c : Combination) (acc : list Ascii.ascii) : list Ascii.ascii :=
  match c with
  | Lf => (Ascii.ascii_of_nat 48) :: acc
  | Sm a => (Ascii.ascii_of_nat 49) :: tern_acc a acc
  | Fk a b => (Ascii.ascii_of_nat 50) :: tern_acc a (tern_acc b acc)
  end.
Definition tern (c : Combination) : string := string_of_list_ascii (tern_acc c []).
Definition vec (label : string) (p : Combination) (args : list Combination) (r : Combination) : string :=
  label ++ " " ++ tern p ++ " " ++ String.concat "," (map tern args) ++ " " ++ tern r.
Definition ie := infer_expression O_int.
(* each vector is one of TyInt.v's proved Examples *)
Definition vectors : list string := [
  vec "five_Int" (infer O_int) [five] (Sm Int);
  vec "minus_six_Int" (infer O_int) [minus_six] (Sm Int);
  vec "zero_Int" (infer O_int) [zero_int] (Sm Int);
  vec "big_Int" (infer O_int) [big] (Sm Int);
  vec "bad_payload_refused" (infer O_int) [bad_payload] Lf;
  vec "sign_typed" ie [five @ 'fst] (Sm Bool);
  vec "mag_typed" ie [five @ 'snd] (Sm (List Bool));
  vec "sign_eval" five ['fst] ff;
  vec "neg_sign_eval" minus_six ['fst] tt
].
Redirect "int_vectors" Eval vm_compute in vectors.
Redirect "int_sizes" Eval vm_compute in (tree_size five, tree_size big, tree_size Int).
