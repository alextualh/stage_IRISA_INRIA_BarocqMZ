From compcert Require Import Integers.
From BarocqComp Require Import Error Monads Intop Barray.
From Verimz Require Import Barray_spec Barocqmz.

Record wf_kernel_partial (ks: Br_interface_kernel) := {
  wf_current: valid_index ks.(br_interface_kernel_zones) (U64.of_u32 ks.(br_interface_kernel_current)) = true;
  wf_next: valid_index ks.(br_interface_kernel_zones) (U64.of_u32 ks.(br_interface_kernel_next)) = true;
  wf_next_quantum: Int64.cmpu Cle ks.(br_interface_kernel_next_quantum) Br_interface_QUANTUM_CYC = true
}.

(* Specs for br_Sched.br *)

(*
Par exemple, pour sched_init, nécessite au moins de:
  - Donner l'implem Rocq des fonctions abstraites;
  - Démontrer qu'elles ne touchent pas aux zones ou aux indices current / next) 
*)

Theorem Br_sched_sched_init_correct:
  forall (ks ks_init: Br_interface_kernel),
  Br_sched_sched_init ks = OK ks_init ->
  wf_kernel_partial ks_init.
Proof.
  intros ks ks_init Hsched_init.
  unfold Br_sched_sched_init in Hsched_init.
  unfold MonError.bind in Hsched_init.
  monadInv Hsched_init. inversion EQ6.
  split; simpl.
  - admit.
  - admit.
  - admit.
Admitted.

Theorem Br_sched_sched_yield_correct:
  forall (ks ks': Br_interface_kernel),
  wf_kernel_partial ks ->
  Br_sched_sched_yield ks = OK ks' ->
  wf_kernel_partial ks'.
Admitted.

(* Theorem Br_sched_sched_yield_correct:
  forall (ks: Br_interface_kernel),
  wf_kernel_partial ks ->
  (exists ks',
   Br_sched_sched_yield ks = OK ks' /\
   wf_kernel_partial ks').
Admitted. *)
