(* Export Jay's trees (typing-rules-as-programs @ ddbab88b) as tuna
   ternary strings: 0 leaf, 1 stem, 2 fork.  Each vector is
   label | program | args | expected, where expected is what Jay's
   proved Example states.  *)
From BigStep Require Import tree typing soundness rows identifiers booleans.
From Stdlib Require Import String List.
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

Definition vectors : list string := [
  vec "tt_Bool" (infer O_ex) [tt] (Sm Bool);
  vec "ff_Bool" (infer O_ex) [ff] (Sm Bool);
  vec "zero_Nat" (infer O_ex) [zero] (Sm Nat);
  vec "one_Nat" (infer O_ex) [succ zero] (Sm Nat);
  vec "fst_self" (infer O_ex) ['fst] (Sm 'fst);
  vec "tagged_K_Bool_untyped" (infer O_ex) ['tagged['K, bool_nm]] Lf;
  vec "tagged_fst_undeclared" (infer O_ex) ['tagged['fst, Lf]] Lf;
  vec "Bool_elim_Nat" (tp O_ex) [Bool; Nat '* Nat] (Sm Nat);
  vec "Bool_elim_mixed" (tp O_ex) [Bool; Nat '* Bool] Lf;
  vec "infer_self_typed" (infer O_ex) [infer O_ex] (Sm (infer O_ex))
].
Redirect "bool_vectors" Eval vm_compute in vectors.
