open BinIntDef
open Datatypes
open Errors
open Integers
open Monads

module I32 :
 sig
  val div : Int.int -> Int.int -> Int.int res

  val coq_mod : Int.int -> Int.int -> Int.int res

  val of_bool : bool -> Int.int

  val to_bool : Int.int -> bool

  val of_u32 : Int.int -> Int.int

  val of_i64 : Int64.int -> Int.int

  val of_u64 : Int64.int -> Int.int
 end

module U32 :
 sig
  val div : Int.int -> Int.int -> Int.int res

  val coq_mod : Int.int -> Int.int -> Int.int res

  val of_bool : bool -> Int.int

  val to_bool : Int.int -> bool

  val of_i32 : Int.int -> Int.int

  val of_i64 : Int64.int -> Int.int

  val of_u64 : Int64.int -> Int.int
 end

module I64 :
 sig
  val div : Int64.int -> Int64.int -> Int64.int res

  val coq_mod : Int64.int -> Int64.int -> Int64.int res

  val of_bool : bool -> Int64.int

  val to_bool : Int64.int -> bool

  val of_u64 : Int64.int -> Int64.int

  val of_i32 : Int.int -> Int64.int

  val of_u32 : Int.int -> Int64.int
 end

module U64 :
 sig
  val div : Int64.int -> Int64.int -> Int64.int res

  val coq_mod : Int64.int -> Int64.int -> Int64.int res

  val of_bool : bool -> Int64.int

  val to_bool : Int64.int -> bool

  val of_i64 : Int64.int -> Int64.int

  val of_i32 : Int.int -> Int64.int

  val of_u32 : Int.int -> Int64.int

  val to_nat : Int64.int -> nat
 end
