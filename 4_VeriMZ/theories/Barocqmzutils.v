From Verimz Require Import Int Pmp.

Definition int_to_Intptr(x : int) : Intptr.int :=
    Intptr.repr (Int.unsigned x).

Definition Intptr_to_int (p : Intptr.int) : Int.int :=
  Int.repr (Intptr.unsigned p).

Definition int_to_int64 (i : int) : int64 :=
  Int64.repr (Int.signed i).

Definition Intptr_to_int64 (i : Intptr.int) : int64 :=
  Int64.repr (Intptr.unsigned i).