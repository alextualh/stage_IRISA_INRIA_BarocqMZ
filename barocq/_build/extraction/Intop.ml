open BinIntDef
open Datatypes
open Errors
open Integers
open Monads

module I32 =
 struct
  (** val div : Int.int -> Int.int -> Int.int res **)

  let div x y =
    if (||) (Int.eq y Int.zero)
         ((&&) (Int.eq x (Int.repr Int.min_signed)) (Int.eq y Int.mone))
    then MonError.fail
    else MonError.ret (Int.divs x y)

  (** val coq_mod : Int.int -> Int.int -> Int.int res **)

  let coq_mod x y =
    if (||) (Int.eq y Int.zero)
         ((&&) (Int.eq x (Int.repr Int.min_signed)) (Int.eq y Int.mone))
    then MonError.fail
    else MonError.ret (Int.mods x y)

  (** val of_bool : bool -> Int.int **)

  let of_bool = function
  | true -> Int.one
  | false -> Int.zero

  (** val to_bool : Int.int -> bool **)

  let to_bool x =
    if Int.eq x Int.zero then false else true

  (** val of_u32 : Int.int -> Int.int **)

  let of_u32 x =
    x

  (** val of_i64 : Int64.int -> Int.int **)

  let of_i64 x =
    Int.repr (Int64.signed x)

  (** val of_u64 : Int64.int -> Int.int **)

  let of_u64 x =
    Int.repr (Int64.unsigned x)
 end

module U32 =
 struct
  (** val div : Int.int -> Int.int -> Int.int res **)

  let div x y =
    if Int.eq y Int.zero then MonError.fail else MonError.ret (Int.divu x y)

  (** val coq_mod : Int.int -> Int.int -> Int.int res **)

  let coq_mod x y =
    if Int.eq y Int.zero then MonError.fail else MonError.ret (Int.modu x y)

  (** val of_bool : bool -> Int.int **)

  let of_bool = function
  | true -> Int.one
  | false -> Int.zero

  (** val to_bool : Int.int -> bool **)

  let to_bool x =
    if Int.eq x Int.zero then false else true

  (** val of_i32 : Int.int -> Int.int **)

  let of_i32 x =
    x

  (** val of_i64 : Int64.int -> Int.int **)

  let of_i64 x =
    Int.repr (Int64.signed x)

  (** val of_u64 : Int64.int -> Int.int **)

  let of_u64 x =
    Int.repr (Int64.unsigned x)
 end

module I64 =
 struct
  (** val div : Int64.int -> Int64.int -> Int64.int res **)

  let div x y =
    if (||) (Int64.eq y Int64.zero)
         ((&&) (Int64.eq x (Int64.repr Int64.min_signed))
           (Int64.eq y Int64.mone))
    then MonError.fail
    else MonError.ret (Int64.divs x y)

  (** val coq_mod : Int64.int -> Int64.int -> Int64.int res **)

  let coq_mod x y =
    if (||) (Int64.eq y Int64.zero)
         ((&&) (Int64.eq y (Int64.repr Int64.min_signed))
           (Int64.eq y Int64.mone))
    then MonError.fail
    else MonError.ret (Int64.mods x y)

  (** val of_bool : bool -> Int64.int **)

  let of_bool = function
  | true -> Int64.one
  | false -> Int64.zero

  (** val to_bool : Int64.int -> bool **)

  let to_bool x =
    if Int64.eq x Int64.zero then false else true

  (** val of_u64 : Int64.int -> Int64.int **)

  let of_u64 x =
    x

  (** val of_i32 : Int.int -> Int64.int **)

  let of_i32 x =
    Int64.repr (Int.signed x)

  (** val of_u32 : Int.int -> Int64.int **)

  let of_u32 x =
    Int64.repr (Int.unsigned x)
 end

module U64 =
 struct
  (** val div : Int64.int -> Int64.int -> Int64.int res **)

  let div x y =
    if Int64.eq y Int64.zero
    then MonError.fail
    else MonError.ret (Int64.divu x y)

  (** val coq_mod : Int64.int -> Int64.int -> Int64.int res **)

  let coq_mod x y =
    if Int64.eq y Int64.zero
    then MonError.fail
    else MonError.ret (Int64.modu x y)

  (** val of_bool : bool -> Int64.int **)

  let of_bool = function
  | true -> Int64.one
  | false -> Int64.zero

  (** val to_bool : Int64.int -> bool **)

  let to_bool x =
    if Int64.eq x Int64.zero then false else true

  (** val of_i64 : Int64.int -> Int64.int **)

  let of_i64 x =
    x

  (** val of_i32 : Int.int -> Int64.int **)

  let of_i32 x =
    Int64.repr (Int.signed x)

  (** val of_u32 : Int.int -> Int64.int **)

  let of_u32 x =
    Int64.repr (Int.unsigned x)

  (** val to_nat : Int64.int -> nat **)

  let to_nat x =
    Z.to_nat (Int64.unsigned x)
 end
