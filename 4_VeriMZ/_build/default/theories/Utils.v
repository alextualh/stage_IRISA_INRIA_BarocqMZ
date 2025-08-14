From Verimz Require Import Int.
From Verimz Require Export Error.
Require Export List.

Export ListNotations.

Open Scope error_monad_scope.

Section List.

  (** Useful functions over lists. *)

  Definition get {A: Type} (l: list A) (n: nat) : option A := nth_error l n.

  Fixpoint set {A: Type} (l: list A) (n: nat) (v: A) : option (list A) :=
    match n, l with
    | O, x :: l' => Some (v :: l')
    | S n', x :: l' =>
        do vl <- set l' n' v;
        Some (x :: vl)
    | _, _ => None
    end.

  Definition get2 {A: Type} (l: list A) (n: intptr) : option A :=
    get l (Intptr.unsigned_to_nat n).

  Definition set2 {A: Type} (l: list A) (n: intptr) (v: A) : option (list A) :=
    set l (Intptr.unsigned_to_nat n) v.

  Fixpoint make {A: Type} (n: nat) (v: A) : list A :=
    match n with
    | O => []
    | S n' => v :: make n' v
    end.
 
End List.