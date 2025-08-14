open Int0
open Integers

(** val int_to_Intptr : Int.int -> Intptr.int **)

let int_to_Intptr x =
  Intptr.repr (Int.unsigned x)

(** val coq_Intptr_to_int : Intptr.int -> Int.int **)

let coq_Intptr_to_int p =
  Int.repr (Intptr.unsigned p)

(** val int_to_int64 : Int.int -> Int64.int **)

let int_to_int64 i =
  Int64.repr (Int.signed i)

(** val coq_Intptr_to_int64 : Intptr.int -> Int64.int **)

let coq_Intptr_to_int64 i =
  Int64.repr (Intptr.unsigned i)
