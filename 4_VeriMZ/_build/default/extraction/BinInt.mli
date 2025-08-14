open BinNat
open BinPos
open Datatypes

module Z :
 sig
  val double : Stdlib.Int.t -> Stdlib.Int.t

  val succ_double : Stdlib.Int.t -> Stdlib.Int.t

  val pred_double : Stdlib.Int.t -> Stdlib.Int.t

  val pos_sub : int -> int -> Stdlib.Int.t

  val add : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val opp : Stdlib.Int.t -> Stdlib.Int.t

  val pred : Stdlib.Int.t -> Stdlib.Int.t

  val sub : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val mul : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val compare : Stdlib.Int.t -> Stdlib.Int.t -> comparison

  val leb : Stdlib.Int.t -> Stdlib.Int.t -> bool

  val ltb : Stdlib.Int.t -> Stdlib.Int.t -> bool

  val to_nat : Stdlib.Int.t -> int

  val of_nat : int -> Stdlib.Int.t

  val of_N : int -> Stdlib.Int.t

  val iter : Stdlib.Int.t -> ('a1 -> 'a1) -> 'a1 -> 'a1

  val pos_div_eucl : int -> Stdlib.Int.t -> Stdlib.Int.t * Stdlib.Int.t

  val div_eucl : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t * Stdlib.Int.t

  val div : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val modulo : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val quotrem : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t * Stdlib.Int.t

  val quot : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val rem : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val odd : Stdlib.Int.t -> bool

  val div2 : Stdlib.Int.t -> Stdlib.Int.t

  val log2 : Stdlib.Int.t -> Stdlib.Int.t

  val testbit : Stdlib.Int.t -> Stdlib.Int.t -> bool

  val shiftl : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val shiftr : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val coq_lor : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val coq_land : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val coq_lxor : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

  val eq_dec : Stdlib.Int.t -> Stdlib.Int.t -> bool
 end
