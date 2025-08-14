From Coq Require Import Bool Lia PeanoNat List.
From compcert Require Import Integers.
From BarocqComp Require Import Error Intop Barray.

Lemma valid_index_logic:
  forall (A: Type) (a: array A) (i: int64),
  valid_index a i = true ->
  U64.to_nat i < (Barray.length a).
Proof.
  unfold valid_index. intros.
  rewrite andb_true_iff in H. destruct H.
  apply Nat.ltb_lt in H0. exact H0.
Qed.

Theorem valid_index_get_ok:
  forall (A: Type) (a: array A) (i: int64),
  valid_index a i = true ->
  exists v, get a i = OK v.
Proof.
  intros. unfold get. rewrite H.
  destruct (nth_error a (U64.to_nat i)) eqn:Enth.
  - exists a0. reflexivity.
  - pose proof (nth_error_None a (U64.to_nat i)).
    destruct H0. specialize (H0 Enth).
    apply valid_index_logic in H.
    unfold length in H. lia.
Qed.

Theorem set_rec_preserve_length :
  forall (A: Type) (a: array A) (n: nat) (v: A),
  Barray.length (set_rec a n v) = Barray.length a.
Proof.
  induction a; destruct n; intros; try reflexivity.
  simpl. rewrite (IHa n v). reflexivity.
Qed.

Theorem valid_index_set_ok:
  forall (A: Type) (a: array A) (i: int64) (v: A),
  valid_index a i = true ->
  exists a',
  (set a i v = OK a' /\ Barray.length a' = Barray.length a).
Proof.
  intros. unfold set. rewrite H.
  exists (set_rec a (U64.to_nat i) v). split.
  - reflexivity.
  - apply set_rec_preserve_length.
Qed.




    
    