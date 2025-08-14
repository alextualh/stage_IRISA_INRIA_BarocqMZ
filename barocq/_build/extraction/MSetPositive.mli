open BinNums

module PositiveSet :
 sig
  type elt = positive

  type tree =
  | Leaf
  | Node of tree * bool * tree

  type t = tree

  val empty : t

  val mem : positive -> t -> bool

  val add : positive -> t -> t

  val union : t -> t -> t

  val rev_append : elt -> elt -> elt

  val rev : elt -> elt

  val xelements : t -> positive -> positive list -> positive list

  val elements : t -> positive list
 end
