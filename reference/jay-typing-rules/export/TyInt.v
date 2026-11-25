(* TyInt: a binary Int for tuna, declared in Jay's framework
   (typing-rules-as-programs @ ddbab88b).  Int is a WRAPPED type
   (his section 5.6) whose payload is  sign * magnitude  of type
   Bool * List Bool: the same shape as tuna's v1 int (sign bool,
   LSB-first bit list), rebuilt from his tagged Bool and List cells.
   Soundness: his wrap_decl_sound holds for every payload type, so the
   Int row adds no new obligation; O_int_sound below repeats his
   O_dept_sound for this ontology.  Ours (tuna), MIT like his. *)
From BigStep Require Import tree typing soundness rows identifiers booleans functions updates data_types lists department.
From Stdlib Require Import String List Bool Lia BinNums PArith.
Import ListNotations.

Definition int_nm : Combination := Nat.iter 61 Sm Lf.
Definition Int : Combination := 'label[int_nm].
Definition IntPayload : Combination := Bool '* List Bool.

Definition bit (b : bool) : Combination := if b then tt else ff.
Fixpoint bits (bs : list bool) : Combination :=
  match bs with [] => lists.nil Bool | b :: t => lists.cons Bool (bit b) (bits t) end.
Fixpoint pos_bits (p : positive) : list bool :=
  match p with xH => [true] | xO q => false :: pos_bits q | xI q => true :: pos_bits q end.
(** [int_of neg p] is the Int with sign [neg] and magnitude [p]
    (None = zero, which is always non-negative: one zero, as in v1). *)
Definition int_of (neg : bool) (p : option positive) : Combination :=
  match p with
  | None => 'tagged[swap (ff '* bits []), int_nm]
  | Some q => 'tagged[swap (bit neg '* bits (pos_bits q)), int_nm]
  end.

Definition I_rows : list (Combination * Combination) :=
  (L_rows ++ [(int_nm, D_wrap IntPayload)])%list.
Definition O_int : Combination := rows_f I_rows L_fam.

(** ** Soundness (his O_dept_sound, specialised) *)

Lemma O_int_inv : forall n D, O_int @ n @r tp O_int ==> Sm D ->
  lookup I_rows n = Some D \/
  (n = update_nm /\ D = D_upd update_nm (tp O_int)) \/ (n = mku_nm /\ D = D_mku (tp O_int)) \/
  (exists r, n = list_tag r /\ D = D_list r) \/
  (exists r, n = Fk mkcons_nm r /\ concreteb r = true /\ D = D_mkcons r).
Proof.
  intros n D H; destruct (rows_f_inv _ _ _ _ _ H) as [[D' [HL E]]|[_ HF]].
  - inversion E; subst; left; exact HL.
  - right; apply L_fam_inv; exact HF.
Qed.

Lemma O_int_static : forall n D, lookup I_rows n = Some D -> O_int @ n @r tp O_int ==> Sm D.
Proof.
  intros n D H; destruct (L_fam_partial n) as [p Hp]; unfold O_int; eapply rows_f_hit; eassumption.
Qed.

Lemma O_int_update : O_int @ update_nm @r tp O_int ==> Sm (D_upd update_nm (tp O_int)).
Proof.
  unfold O_int; apply rows_f_miss; [reflexivity|]. unfold L_fam; apply alt_some; [apply upd_fam_update|].
  apply alt_partial, list_entry_partial.
Qed.

Lemma O_int_list : forall r, O_int @ list_tag r @r tp O_int ==> Sm (D_list r).
Proof.
  intros r; unfold O_int; apply rows_f_miss; [reflexivity|]. unfold L_fam.
  apply alt_none; [apply upd_fam_miss; discriminate|].
  apply alt_some; [apply list_entry_hit|]. apply alt_partial.
  pose proof (get_par_spec mkcons_nm (list_tag r)) as Hp; cbn [par_of list_tag] in Hp.
  destruct (combination_eq_dec mkcons_nm list_nm) as [E|]; [discriminate|]; cbn [opt] in Hp.
  Local Transparent get_par is_concrete. unfold mkcons_entry; eexists; ev2.
Qed.
Local Opaque get_par is_concrete.

Theorem O_int_sound : ontology_sound O_int.
Proof.
  intros n D H.
  destruct (O_int_inv n D H) as [HL|[[-> ->]|[[-> ->]|[[r [-> ->]]|[r [-> [Hc ->]]]]]]].
  - unfold I_rows, L_rows, O_rows in HL; cbn [lookup List.app] in HL.
    repeat (destruct (combination_eq_dec _ n) as [<-|]; [inversion HL; subst|]); try discriminate.
    + apply bool_decl_sound.
    + apply nat_decl_sound.
    + eapply ctor_decl_sound; [reflexivity|apply O_int_static; reflexivity|apply guard_of_pair].
    + eapply ctor_decl_sound; [reflexivity|apply O_int_static; reflexivity|apply guard_of_pair].
    + apply wrap_decl_sound.
  - apply upd_decl_sound_g, O_int_update.
  - apply mku_decl_sound_g, O_int_update.
  - apply list_decl_sound.
  - eapply ctor_decl_sound; [cbn; exact Hc|apply O_int_list|apply guard_of_pair].
Qed.

Corollary O_int_typing_sound : typing_sound O_int.
Proof. apply evaluation_preserves_typing, O_int_sound. Qed.

(** ** Examples, proved by running his checker *)

Ltac by_run_big := apply (runp_sound 40); vm_done.

Definition five := int_of false (Some 5%positive).
Definition minus_six := int_of true (Some 6%positive).
Definition zero_int := int_of false None.
Definition big := int_of false (Some 1048579%positive).
Definition bad_payload := 'tagged[swap (ff '* lists.cons Nat zero (lists.nil Nat)), int_nm].

Example O_int_concrete : concreteb O_int = true. Proof. vm_compute; reflexivity. Qed.
Example five_Int : infer O_int @ five ==> Sm Int. Proof. by_run_big. Qed.
Example minus_six_Int : infer O_int @ minus_six ==> Sm Int. Proof. by_run_big. Qed.
Example zero_Int : infer O_int @ zero_int ==> Sm Int. Proof. by_run_big. Qed.
Example big_Int : infer O_int @ big ==> Sm Int. Proof. by_run_big. Qed.
Example bad_payload_refused : infer O_int @ bad_payload ==> Lf. Proof. by_run_big. Qed.
Example sign_typed : infer_expression O_int @ (five @ 'fst) ==> Sm Bool. Proof. by_run_big. Qed.
Example mag_typed : infer_expression O_int @ (five @ 'snd) ==> Sm (List Bool). Proof. by_run_big. Qed.
Example sign_eval : five @ 'fst ==> ff. Proof. by_run_big. Qed.
Example neg_sign_eval : minus_six @ 'fst ==> tt. Proof. by_run_big. Qed.
