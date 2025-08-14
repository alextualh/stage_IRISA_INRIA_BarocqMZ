open BinNums
open Datatypes

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val max : positive -> positive -> positive

  val leb : positive -> positive -> bool
 end
