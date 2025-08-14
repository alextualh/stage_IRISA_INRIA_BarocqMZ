open Archi
open BinInt
open Coqlib
open Zbits
open Zpower

module Wordsize_Intptr :
 sig
  val wordsize : int
 end

module Intptr :
 sig
  val wordsize : int

  val modulus : Stdlib.Int.t

  type int =
    Stdlib.Int.t
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> Stdlib.Int.t

  val coq_Z_mod_modulus : Stdlib.Int.t -> Stdlib.Int.t

  val unsigned : int -> Stdlib.Int.t

  val repr : Stdlib.Int.t -> int

  val zero : int

  val one : int

  val eq : int -> int -> bool

  val add : int -> int -> int

  val sub : int -> int -> int

  val divu : int -> int -> int

  val coq_and : int -> int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val sign_ext : Stdlib.Int.t -> int -> int

  val unsigned_to_nat : int -> int

  val of_nat : int -> int
 end
