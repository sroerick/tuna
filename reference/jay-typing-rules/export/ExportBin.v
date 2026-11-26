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
Definition ie := infer_expression O_bin.
(* each vector is one of TyBin.v's proved Examples *)
Definition vectors : list string := [
  vec "bzero_Bin" (infer O_bin) [bzero] (Sm Bin);
  vec "b5_Bin" (infer O_bin) [b5] (Sm Bin);
  vec "bbig_Bin" (infer O_bin) [bbig] (Sm Bin);
  vec "i5_Int" (infer O_bin) [i5] (Sm Int);
  vec "im6_Int" (infer O_bin) [im6] (Sm Int);
  vec "ibig_Int" (infer O_bin) [ibig] (Sm Int);
  vec "bad_bit_refused" (infer O_bin) ['tagged['S2[sel_e, 'K1 tt], bin_nm]] Lf;
  vec "bin_elim_typed" (tp O_bin) [Bin; Nat '* (('K1 Nat) '* ('K1 Nat))] (Sm Nat);
  vec "bin_elim_mixed" (tp O_bin) [Bin; Nat '* (('K1 Bool) '* ('K1 Nat))] Lf;
  vec "b5_eval" b5 [zero '* (('K1 one) '* ('K1 two))] two
].
Redirect "bin_vectors" Eval vm_compute in vectors.
Redirect "bin_sizes" Eval vm_compute in (tree_size bzero, tree_size b5, tree_size bbig, tree_size i5, tree_size ibig).
