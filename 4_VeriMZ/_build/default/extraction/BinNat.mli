open BinPos
open Datatypes

module N :
 sig
  val succ_double : int -> int

  val double : int -> int

  val succ_pos : int -> int

  val sub : int -> int -> int

  val compare : int -> int -> comparison

  val leb : int -> int -> bool

  val pos_div_eucl : int -> int -> int * int

  val coq_lor : int -> int -> int

  val coq_land : int -> int -> int

  val ldiff : int -> int -> int

  val coq_lxor : int -> int -> int

  val testbit : int -> int -> bool
 end
