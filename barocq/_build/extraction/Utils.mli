open BinNums
open Datatypes
open Errors
open Ident
open List0
open MSetPositive
open Monads

val fresh_var : char list -> t MonCounter.coq_M

val fresh_var_err : char list -> t MonCounterErr.coq_M

val nth_err : 'a1 list -> nat -> 'a1 res

val fold_left_err : ('a1 -> 'a2 -> 'a1 res) -> 'a2 list -> 'a1 res -> 'a1 res

val list_is_empty : 'a1 list -> bool

val smem : PositiveSet.t -> positive -> bool

val sadd : PositiveSet.t -> positive -> PositiveSet.t

val sunion : PositiveSet.t -> PositiveSet.t -> PositiveSet.t
