open BinIntDef
open Errors
open Integers
open Monads

module I32 =
 struct
  (** val of_u32 : Int.int -> Int.int **)

  let of_u32 x =
    x
 end

module U32 =
 struct
  (** val coq_mod : Int.int -> Int.int -> Int.int res **)

  let coq_mod x y =
    if Int.eq y Int.zero then MonError.fail else MonError.ret (Int.modu x y)
 end

module U64 =
 struct
  (** val of_u32 : Int.int -> Int64.int **)

  let of_u32 x =
    Int64.repr (Int.unsigned x)

  (** val to_nat : Int64.int -> int **)

  let to_nat x =
    Z.to_nat (Int64.unsigned x)
 end
