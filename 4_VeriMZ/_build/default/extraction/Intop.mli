open BinIntDef
open Errors
open Integers
open Monads

module I32 :
 sig
  val of_u32 : Int.int -> Int.int
 end

module U32 :
 sig
  val coq_mod : Int.int -> Int.int -> Int.int res
 end

module U64 :
 sig
  val of_u32 : Int.int -> Int64.int

  val to_nat : Int64.int -> int
 end
