(* Export Jay's trees (typing-rules-as-programs @ ddbab88b) as tuna
   ternary strings: 0 leaf, 1 stem, 2 fork.  Each vector is
   label | program | args | expected, where expected is what Jay's
   proved Example states.  *)
From BigStep Require Import tree typing soundness rows identifiers booleans functions updates data_types lists.
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

Definition ie := infer_expression O_list.
Definition vectors : list string := [
  vec "nil_Nat" (infer O_list) [lists.nil Nat] (Sm (List Nat));
  vec "l12_typed" (infer O_list) [l12] (Sm (List Nat));
  vec "cons_wrong" (infer O_list) [lists.cons Nat tt (lists.nil Nat)] Lf;
  vec "cons_fn_typed" ie [cons_fn Nat @ (zero '* lists.nil Nat)] (Sm (List Nat));
  vec "cons_fn_eval" (cons_fn Nat) [zero '* lists.nil Nat] l0;
  vec "rev_append_eval" (lists.rev_append Nat) [l12 '* l0] l210;
  vec "rev_append_typed" ie [lists.rev_append Nat @ (l12 '* l0)] (Sm (List Nat));
  vec "reverse_eval" (lists.reverse Nat) [l12] l21;
  vec "reverse_typed" ie [lists.reverse Nat @ l12] (Sm (List Nat));
  vec "append_eval" (lists.append Nat) [l12 '* l0] l120;
  vec "append_typed" ie [lists.append Nat @ (l12 '* l0)] (Sm (List Nat));
  vec "length_eval" (lists.length Nat) [l120] (succ two);
  vec "length_typed" ie [lists.length Nat @ l120] (Sm Nat);
  vec "filter_eval" (lists.filter Bool 'I) [lb] ltt;
  vec "filter_typed" ie [lists.filter Bool 'I @ lb] (Sm (List Bool));
  vec "map_eval" (lists.map Nat Nat succ_fn) [l0] (lists.cons Nat one (lists.nil Nat));
  vec "map_typed" ie [lists.map Nat Nat succ_fn @ l12] (Sm (List Nat));
  vec "foldright_eval" (lists.foldright Nat (List Nat) (cons_fn Nat)) [l12 '* l0] l120;
  vec "foldright_typed" ie [lists.foldright Nat (List Nat) (cons_fn Nat) @ (l12 '* l0)] (Sm (List Nat));
  vec "append_pairs_typed" ie [lists.append LP @ (lpe '* lpe)] (Sm (List LP))
].
Redirect "list_vectors" Eval vm_compute in vectors.
