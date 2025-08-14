From Coq Require Import ZArith.
From compcert Require Import Integers.
From Verimz Require Import Barocqmz.

(* Preuve triviale : l'égalité définitionnelle *)

Lemma Br_interface_and_with_bitwise_def (p_mie p_uie : int) :
  Br_interface_and_with_bitwise p_mie p_uie = Int.and p_mie (Int.not p_uie).
Proof.
  reflexivity. 
  (* i.e. la fonction fait ce qui est dit dans la formule à droite, 
  c'est bon, la fonction est elle même, Qed. *)
Qed.

Lemma and_not_mask_zero (p_mie p_uie : int) :
  Int.and (Br_interface_and_with_bitwise p_mie p_uie) p_uie = Int.zero.
Proof.
  unfold Br_interface_and_with_bitwise.
  (* on doit utiliser propriétés des opérateurs bitwise :
     Int.and_assoc, Int.and_comm, Int.and_zero, Int.and_not? *)
  (* Preuves sur Int peuvent être dans CompCert; si disponibles : *)
  (* simpl ; rewrite <- Int.and_assoc ; ...  *)
Admitted.  (* laisser en admitted si les lemmes nécessaires ne sont pas importés *)



(* b) Exemple d’une propriété utile (masquage)
Propriété : (p_mie & ~p_uie) & p_uie = 0. (i.e. le résultat masque les bits de p_uie) :

Lemma and_not_mask_zero (p_mie p_uie : int) :
  Int.and (Br_interface_and_with_bitwise p_mie p_uie) p_uie = Int.zero.
Proof.
  unfold Br_interface_and_with_bitwise.
  (* on doit utiliser propriétés des opérateurs bitwise :
     Int.and_assoc, Int.and_comm, Int.and_zero, Int.and_not? *)
  (* Preuves sur Int peuvent être dans CompCert; si disponibles : *)
  (* simpl ; rewrite <- Int.and_assoc ; ...  *)
Admitted.  (* laisser en admitted si les lemmes nécessaires ne sont pas importés *)
Ici la preuve peut requérir des lemmes provenant de la bibliothèque Integers (Require Import Integers).

Si les lemmes manquent, tactiques ou démontrez les lemmes auxiliaires sur Int en basant sur Int.unsigned / Z s’il faut.

4) Techniques de preuve utiles pour ce domaine
unfold pour développer définitions simples.

simpl / cbv delta / compute pour évaluer.

reflexivity quand l’égalité est définitionnelle.

rewrite avec lemmes existants (chercher Search Int.and ou Search (Int.and ...)).

Change / vm_compute / native_compute pour exécuter si besoin (attention aux axiomes).

Si sur les bits : convertir en Z via Int.unsigned et utiliser Z-arith (Zbits etc.).

5) Conseils pratiques
Dépendances : dans Barocqmz_spec.v exactement ce qu’il faut, pas plus :

From Coq Require Import List ZArith.
From compcert Require Import Integers.
From Verimz Require Import Barocqmz.
Nommer lemmes clairement : foo_correct, foo_spec, foo_mask_property.

cas simples (définitionnelles) avant d’attaquer les propriétés sur le comportement.

Tester : créer petits Compute / Eval compute in ... dans un fichier de tests pour valider des exemples concrets.

Ajouter au build : ajouter le fichier de preuves au dune pour qu’il soit compilé.

6) Exemple complet (fichier de preuves)

(* Barocqmz_spec.v *)
From Coq Require Import ZArith.
From compcert Require Import Integers.
From Verimz Require Import Barocqmz.

Lemma Br_interface_and_with_bitwise_def (p_mie p_uie : int) :
  Br_interface_and_with_bitwise p_mie p_uie = Int.and p_mie (Int.not p_uie).
Proof. reflexivity. Qed.
Compilez ensuite avec dune build (ou coqc Barocqmz_spec.v) ; si vous modifiez l’implémentation, recompilez les dépendances.

 *)
