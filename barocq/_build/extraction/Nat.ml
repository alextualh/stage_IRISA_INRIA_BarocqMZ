open Datatypes
open Decimal

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val max : nat -> nat -> nat **)

let rec max n m =
  match n with
  | O -> m
  | S n' -> (match m with
             | O -> n
             | S m' -> S (max n' m'))

(** val to_little_uint : nat -> uint -> uint **)

let rec to_little_uint n acc =
  match n with
  | O -> acc
  | S n0 -> to_little_uint n0 (Little.succ acc)

(** val to_uint : nat -> uint **)

let to_uint n =
  rev (to_little_uint n (D0 Nil))
