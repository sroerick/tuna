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

(* is_concrete_spec: is_concrete @ n ==> (if concreteb n then K else KI);
   the expected value is that spec's right-hand side, computed. *)
Definition cvec (label : string) (n : Combination) : string :=
  vec label is_concrete [n] (if concreteb n then 'K else 'KI).
Definition vectors : list string := [
  cvec "tp_concrete" (tp O_ex);
  cvec "infer_concrete" (infer O_ex);
  cvec "O_ex_concrete" O_ex;
  cvec "I_concrete" 'I;
  cvec "tt_not_concrete" tt;
  cvec "label_not_concrete" Bool;
  cvec "succ_zero_not_concrete" (succ zero)
].
Definition answers : list Combination :=
  map (fun n => if concreteb n then 'K else 'KI) [tp O_ex; infer O_ex; O_ex; 'I; tt; Bool; succ zero].
Redirect "concrete_answers" Eval vm_compute in answers.
Redirect "concrete_vectors" Eval vm_compute in vectors.
