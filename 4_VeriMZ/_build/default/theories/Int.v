From compcert Require Export Integers.
Require Import ZArith.

Module Wordsize_Intptr.
  Definition wordsize := if Archi.ptr64 then 64%nat else 32%nat.
  Remark wordsize_not_zero: wordsize <> 0%nat.
Proof.
  unfold wordsize; destruct Archi.ptr64; congruence.
Qed.

End Wordsize_Intptr.

Strategy opaque [Wordsize_Ptrofs.wordsize].

Module Intptr.

  Include Make(Wordsize_Intptr).

  Definition unsigned_to_nat (iptr: int):= Z.to_nat (Intptr.unsigned iptr).

  Definition of_nat (n: nat) := Intptr.repr (Z.of_nat n).

End Intptr.

Strategy 0 [Wordsize_Ptrofs.wordsize].

Notation intptr := Intptr.int.

Global Opaque Intptr.repr.