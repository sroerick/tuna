(* TyBin: binary naturals and integers for tuna, in Jay's framework
   (typing-rules-as-programs @ ddbab88b).  Ours (tuna), MIT like his.

   Bin is declared like his Nat (booleans.v) but with TWO successors:
     bzero          consumer (b * (e * o)) gives b
     beven[m] = 2m  gives e @ m
     bodd[m]  = 2m+1 gives o @ m
   one tag per bit, under the 3-node name Fk Lf Lf.  Its declaration is
   proved sound by values below, following his nat_interp_sound.
   Int is his WRAPPED type (department.v) over Bool * Bin: one more tag
   per value, sound by his wrap_decl_sound. *)
From BigStep Require Import tree typing soundness rows identifiers booleans functions updates data_types lists department.
From Stdlib Require Import String List Bool Lia BinNums PArith.
Import ListNotations.

Definition bin_nm : Combination := Fk Lf Lf.
Definition Bin : Combination := 'label[bin_nm].
Definition sel_e : Combination := 'fst 'C 'snd.
Definition sel_o : Combination := 'snd 'C 'snd.
Definition bzero : Combination := 'tagged['fst, bin_nm].
Definition beven (m : Combination) : Combination := 'tagged['S2[sel_e, 'K1 m], bin_nm].
Definition bodd (m : Combination) : Combination := 'tagged['S2[sel_o, 'K1 m], bin_nm].

Lemma sel_e_pair : forall b e o, sel_e @ (b '* (e '* o)) ==> e.
Proof. intros; unfold sel_e; apply eval_c; eapply eval_appr; [apply snd_pair|apply fst_pair]. Qed.
Lemma sel_o_pair : forall b e o, sel_o @ (b '* (e '* o)) ==> o.
Proof. intros; unfold sel_o; apply eval_c; eapply eval_appr; [apply snd_pair|apply snd_pair]. Qed.

(** The guard admits the carrier types fst, S2[sel_e, K1 Bin], S2[sel_o, K1 Bin]. *)
Definition bg_prog : Combination := Eval cbn -[equal] in to_tree
  (star "m" (star "C"
    (op equal @@ Ref "C" @@ Val 'fst @@ Val 'K
       @@ (op equal @@ Ref "C" @@ ''S2[Val sel_e, ''K1 (Val Lf @@ (Val Lf @@ Val 'K) @@ (Val 'K @@ Ref "m"))] @@ Val 'K
       @@ (op equal @@ Ref "C" @@ ''S2[Val sel_o, ''K1 (Val Lf @@ (Val Lf @@ Val 'K) @@ (Val 'K @@ Ref "m"))]))))).
Definition bin_guard : Combination := 'wait[bg_prog, bin_nm].

Local Opaque get_pn get_rr.

(** The eliminator: Bin # (B * (E * O)) = Sm B when E # Bin ==> Sm B and O # Bin ==> Sm B. *)
Definition bin_eliminator : Combination := Eval cbn -[equal get_rr get_pn] in to_tree
  (star "n" (star "a" (star "U"
    (op equal @@ Ref "U" @@ ((op get_pn @@ Ref "U") ''* ((op get_pn @@ (op get_rr @@ Ref "U")) ''* (op get_rr @@ (op get_rr @@ Ref "U"))))
       @@ star "U2" (''onStem1 (star "R1"
                        (op equal @@ Ref "R1" @@ (op get_pn @@ Ref "U2")
                           @@ (''onStem1 (star "R2" (op equal @@ Ref "R2" @@ (op get_pn @@ Ref "U2")
                                                        @@ (Val Lf @@ (op get_pn @@ Ref "U2")) @@ Val Lf))
                                 @@ (Ref "a" @@ (op get_rr @@ (op get_rr @@ Ref "U2")) @@ (Val Lf @@ (Val Lf @@ Val 'K) @@ (Val 'K @@ Ref "n"))))
                           @@ Val Lf))
                     @@ (Ref "a" @@ (op get_pn @@ (op get_rr @@ Ref "U2")) @@ (Val Lf @@ (Val Lf @@ Val 'K) @@ (Val 'K @@ Ref "n"))))
       @@ Val ('K1 Lf) @@ Ref "U")))).
Definition D_bin : Combination := bin_guard '* bin_eliminator.



(** ** Soundness of the Bin declaration, by values (after his nat_interp_sound) *)

Local Opaque equal.

Lemma bin_guard_spec : forall C, bin_guard @ C ==> 'K ->
  C = 'fst \/ C = 'S2[sel_e, 'K1 Bin] \/ C = 'S2[sel_o, 'K1 Bin].
Proof.
  intros C H; apply wait_eval_inv in H; unfold bg_prog in H.
  pose proof (equal_bt C 'fst) as E1; pose proof (equal_bt C ('S2[sel_e, 'K1 Bin])) as E2;
  pose proof (equal_bt C ('S2[sel_o, 'K1 Bin])) as E3.
  inv_out E1; inv_out E2; inv_out E3; dedup.
  destruct (combination_eq_dec C 'fst); [left; assumption|].
  destruct (combination_eq_dec C ('S2[sel_e, 'K1 Bin])); [right; left; assumption|].
  destruct (combination_eq_dec C ('S2[sel_o, 'K1 Bin])); [right; right; assumption|].
  cbn [bt] in *; crunch.
Qed.

Lemma bin_eliminator_inv : forall n a U V, bin_eliminator @ n @r a @r U ==> Sm V ->
  exists E O, U = V '* (E '* O) /\ a @ E @r 'label[n] ==> Sm V /\ a @ O @r 'label[n] ==> Sm V.
Proof.
  intros n a U V H; unfold bin_eliminator in H.
  pose proof (get_pn_spec U) as H1; pose proof (get_rr_spec U) as H2.
  pose proof (get_pn_spec (rr U)) as H3; pose proof (get_rr_spec (rr U)) as H4.
  pose proof (equal_bt U (pn U '* (pn (rr U) '* rr (rr U)))) as He; inv_out He; dedup.
  destruct (combination_eq_dec U (pn U '* (pn (rr U) '* rr (rr U)))) as [EU|]; cbn [bt] in *; crunch.
  all: match goal with H : equal @ ?x ==> ?e, H' : ?e @ ?y ==> _ |- _ =>
    let He := fresh "He" in pose proof (equal_bt x y) as He; inv_out He; dedup;
    destruct (combination_eq_dec x y); cbn [bt] in * end; crunch; try discriminate.
  all: match goal with H : equal @ ?x ==> ?e, H' : ?e @ ?y ==> _ |- _ =>
    tryif unify x y then fail else
    (let He := fresh "He" in pose proof (equal_bt x y) as He; inv_out He; dedup;
     destruct (combination_eq_dec x y); cbn [bt] in *) end; crunch; try discriminate.
  all: do 2 eexists; split; [eassumption|split; eapply eval_appl; eassumption].
Qed.


(** His carrier_snd_inv0, for any concrete selector other than K. *)
Lemma carrier_sel_inv : forall O g c P, concreteb g = true -> g <> 'K ->
  infer O @ c ==> Sm ('S2[g, 'K1 P]) -> exists p, c = 'S2[g, 'K1 p] /\ infer O @ p ==> Sm P.
Proof.
  intros O g c P Hg Hk H.
  destruct (type_Fk_inv O _ _ _ H) as [[m E]|[x [y [-> [_ [Hx [Hy _]]]]]]];
    [inversion E; subst; exfalso; apply Hk; reflexivity|].
  destruct (type_Sm_inv O _ _ Hx) as [x' [-> Hx']].
  apply (concrete_type_forces O) in Hx'; [subst x'|exact Hg].
  destruct (type_Fk_inv O _ _ _ Hy) as [[m E]|[y1 [p [-> [_ [Hy1 [Hp _]]]]]]]; [discriminate|].
  pose proof (type_Lf_inv O _ Hy1) as ->.
  exists p; auto.
Qed.

(** The Bin declaration is sound by values, for every ontology. *)
Theorem bin_interp_sound : forall O, decl_sound O bin_nm D_bin.
Proof.
  intros O k C U V c v w HC HE Hc Hv Hk Hw.
  destruct (app2_inv _ _ _ _ HC) as [g [Hg HC']].
  pose proof (eval_deterministic _ _ Hg _ (guard_of_pair _ _)) as ->.
  apply elim_of_inv, bin_eliminator_inv in HE; destruct HE as [E [Od [-> [HEv HOd]]]].
  destruct (pair_type_inv O v V (E '* Od) Hv) as [b [eo [-> [Hb Heo]]]].
  destruct (pair_type_inv O eo E Od Heo) as [e [o [-> [He Ho]]]].
  destruct (bin_guard_spec _ HC') as [->|[->| ->]].
  - apply (concrete_type_forces O) in Hc; [subst c|reflexivity].
    apply evaln_eval in Hw; pose proof (eval_deterministic _ _ Hw _ (fst_pair b (e '* o))) as ->; exact Hb.
  - destruct (carrier_sel_inv O sel_e c Bin) as [p [-> Hp]]; [reflexivity|discriminate|exact Hc|].
    destruct (evaln_S2_inv _ _ _ _ _ Hw) as [j [p1 [p2 [Hj [H1 [H2 H3]]]]]].
    pose proof (eval_deterministic _ _ (evaln_eval _ _ _ H1) _ (sel_e_pair b e o)) as ->.
    assert (E2 : ('K1 p) @ (b '* (e '* o)) ==> p) by evaluate.
    pose proof (eval_deterministic _ _ (evaln_eval _ _ _ H2) _ E2) as ->.
    eapply Hk; [|exact H3|exact He|exact Hp|exact HEv]. lia.
  - destruct (carrier_sel_inv O sel_o c Bin) as [p [-> Hp]]; [reflexivity|discriminate|exact Hc|].
    destruct (evaln_S2_inv _ _ _ _ _ Hw) as [j [p1 [p2 [Hj [H1 [H2 H3]]]]]].
    pose proof (eval_deterministic _ _ (evaln_eval _ _ _ H1) _ (sel_o_pair b e o)) as ->.
    assert (E2 : ('K1 p) @ (b '* (e '* o)) ==> p) by evaluate.
    pose proof (eval_deterministic _ _ (evaln_eval _ _ _ H2) _ E2) as ->.
    eapply Hk; [|exact H3|exact Ho|exact Hp|exact HOd]. lia.
Qed.

Definition int_nm : Combination := Fk Lf (Sm Lf).
Definition Int : Combination := 'label[int_nm].

Definition B_rows : list (Combination * Combination) :=
  (L_rows ++ [(bin_nm, D_bin); (int_nm, D_wrap (Bool '* Bin))])%list.
Definition O_bin : Combination := rows_f B_rows L_fam.

(** ** Soundness of the ontology (his O_dept_sound, specialised) *)

Lemma O_bin_inv : forall n D, O_bin @ n @r tp O_bin ==> Sm D ->
  lookup B_rows n = Some D \/
  (n = update_nm /\ D = D_upd update_nm (tp O_bin)) \/ (n = mku_nm /\ D = D_mku (tp O_bin)) \/
  (exists r, n = list_tag r /\ D = D_list r) \/
  (exists r, n = Fk mkcons_nm r /\ concreteb r = true /\ D = D_mkcons r).
Proof.
  intros n D H; destruct (rows_f_inv _ _ _ _ _ H) as [[D' [HL E]]|[_ HF]].
  - inversion E; subst; left; exact HL.
  - right; apply L_fam_inv; exact HF.
Qed.

Lemma O_bin_static : forall n D, lookup B_rows n = Some D -> O_bin @ n @r tp O_bin ==> Sm D.
Proof.
  intros n D H; destruct (L_fam_partial n) as [p Hp]; unfold O_bin; eapply rows_f_hit; eassumption.
Qed.

Lemma O_bin_update : O_bin @ update_nm @r tp O_bin ==> Sm (D_upd update_nm (tp O_bin)).
Proof.
  unfold O_bin; apply rows_f_miss; [reflexivity|]. unfold L_fam; apply alt_some; [apply upd_fam_update|].
  apply alt_partial, list_entry_partial.
Qed.

Lemma O_bin_list : forall r, O_bin @ list_tag r @r tp O_bin ==> Sm (D_list r).
Proof.
  intros r; unfold O_bin; apply rows_f_miss; [reflexivity|]. unfold L_fam.
  apply alt_none; [apply upd_fam_miss; discriminate|].
  apply alt_some; [apply list_entry_hit|]. apply alt_partial.
  pose proof (get_par_spec mkcons_nm (list_tag r)) as Hp; cbn [par_of list_tag] in Hp.
  destruct (combination_eq_dec mkcons_nm list_nm) as [E|]; [discriminate|]; cbn [opt] in Hp.
  Local Transparent get_par is_concrete. unfold mkcons_entry; eexists; ev2.
Qed.
Local Opaque get_par is_concrete.

Theorem O_bin_sound : ontology_sound O_bin.
Proof.
  intros n D H.
  destruct (O_bin_inv n D H) as [HL|[[-> ->]|[[-> ->]|[[r [-> ->]]|[r [-> [Hc ->]]]]]]].
  - unfold B_rows, L_rows, O_rows in HL; cbn [lookup List.app] in HL.
    repeat (destruct (combination_eq_dec _ n) as [<-|]; [inversion HL; subst|]); try discriminate.
    + apply bool_decl_sound.
    + apply nat_decl_sound.
    + eapply ctor_decl_sound; [reflexivity|apply O_bin_static; reflexivity|apply guard_of_pair].
    + eapply ctor_decl_sound; [reflexivity|apply O_bin_static; reflexivity|apply guard_of_pair].
    + apply bin_interp_sound.
    + apply wrap_decl_sound.
  - apply upd_decl_sound_g, O_bin_update.
  - apply mku_decl_sound_g, O_bin_update.
  - apply list_decl_sound.
  - eapply ctor_decl_sound; [cbn; exact Hc|apply O_bin_list; reflexivity|apply guard_of_pair].
Qed.

Corollary O_bin_typing_sound : typing_sound O_bin.
Proof. apply evaluation_preserves_typing, O_bin_sound. Qed.

(** ** Values from positive numbers *)
Fixpoint bin_pos (p : positive) : Combination :=
  match p with xH => bodd bzero | xO q => beven (bin_pos q) | xI q => bodd (bin_pos q) end.
Definition bin_of (p : option positive) : Combination :=
  match p with None => bzero | Some q => bin_pos q end.
Definition int_of (neg : bool) (p : option positive) : Combination :=
  match p with
  | None => 'tagged[swap (ff '* bzero), int_nm]
  | Some q => 'tagged[swap ((if neg then tt else ff) '* bin_pos q), int_nm]
  end.

Ltac by_run_big := apply (runp_sound 40); vm_done.

Definition b5 := bin_of (Some 5%positive).
Definition bbig := bin_of (Some 1048579%positive).
Definition i5 := int_of false (Some 5%positive).
Definition im6 := int_of true (Some 6%positive).
Definition ibig := int_of false (Some 1048579%positive).

Example O_bin_concrete : concreteb O_bin = true. Proof. vm_compute; reflexivity. Qed.
Example bzero_Bin : infer O_bin @ bzero ==> Sm Bin. Proof. by_run_big. Qed.
Example b5_Bin : infer O_bin @ b5 ==> Sm Bin. Proof. by_run_big. Qed.
Example bbig_Bin : infer O_bin @ bbig ==> Sm Bin. Proof. by_run_big. Qed.
Example i5_Int : infer O_bin @ i5 ==> Sm Int. Proof. by_run_big. Qed.
Example im6_Int : infer O_bin @ im6 ==> Sm Int. Proof. by_run_big. Qed.
Example ibig_Int : infer O_bin @ ibig ==> Sm Int. Proof. by_run_big. Qed.
Example bad_bit_refused : infer O_bin @ 'tagged['S2[sel_e, 'K1 tt], bin_nm] ==> Lf. Proof. by_run_big. Qed.
Example bin_elim_typed : tp O_bin @ Bin @r (Nat '* (('K1 Nat) '* ('K1 Nat))) ==> Sm Nat. Proof. by_run_big. Qed.
Example bin_elim_mixed : tp O_bin @ Bin @r (Nat '* (('K1 Bool) '* ('K1 Nat))) ==> Lf. Proof. by_run_big. Qed.
Example b5_eval : b5 @ (zero '* (('K1 one) '* ('K1 two))) ==> two. Proof. by_run_big. Qed.

Print Assumptions O_bin_typing_sound.
