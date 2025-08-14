open BinNums
open Datatypes

type radix =
  coq_Z
  (* singleton inductive, whose constructor was Build_radix *)

(** val radix2 : radix **)

let radix2 =
  Zpos (Coq_xO Coq_xH)

(** val iter_nat : ('a1 -> 'a1) -> nat -> 'a1 -> 'a1 **)

let rec iter_nat f n x =
  match n with
  | O -> x
  | S n' -> iter_nat f n' (f x)
