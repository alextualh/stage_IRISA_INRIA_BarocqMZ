open BinNums
open Datatypes

type radix =
  coq_Z
  (* singleton inductive, whose constructor was Build_radix *)

val radix2 : radix

val iter_nat : ('a1 -> 'a1) -> nat -> 'a1 -> 'a1
