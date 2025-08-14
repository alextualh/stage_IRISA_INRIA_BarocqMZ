open Datatypes
open Errors
open Integers
open Intop
open List0
open Monads
open PeanoNat

type 'a array = 'a list

val length : 'a1 array -> int

val valid_index : 'a1 array -> Int64.int -> bool

val get : 'a1 array -> Int64.int -> 'a1 res

val set_rec : 'a1 list -> int -> 'a1 -> 'a1 list

val set : 'a1 array -> Int64.int -> 'a1 -> 'a1 array res
