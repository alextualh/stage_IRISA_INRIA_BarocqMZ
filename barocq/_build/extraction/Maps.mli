open BinNums
open Coqlib

module PTree :
 sig
  type 'a tree' =
  | Node001 of 'a tree'
  | Node010 of 'a
  | Node011 of 'a * 'a tree'
  | Node100 of 'a tree'
  | Node101 of 'a tree' * 'a tree'
  | Node110 of 'a tree' * 'a
  | Node111 of 'a tree' * 'a * 'a tree'

  type 'a tree =
  | Empty
  | Nodes of 'a tree'

  type 'a t = 'a tree

  val empty : 'a1 t

  val get' : positive -> 'a1 tree' -> 'a1 option

  val get : positive -> 'a1 tree -> 'a1 option

  val set0 : positive -> 'a1 -> 'a1 tree'

  val set' : positive -> 'a1 -> 'a1 tree' -> 'a1 tree'

  val set : positive -> 'a1 -> 'a1 tree -> 'a1 tree

  val prev_append : positive -> positive -> positive

  val prev : positive -> positive

  val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree'

  val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val fold' :
    ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 tree' -> 'a2 -> 'a2

  val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
 end

module PMap :
 sig
  type 'a t = 'a * 'a PTree.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : positive -> 'a1 t -> 'a1

  val set : positive -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module IMap :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : X.t -> 'a1 t -> 'a1

  val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module ZIndexed :
 sig
  type t = coq_Z

  val index : coq_Z -> positive

  val eq : coq_Z -> coq_Z -> bool
 end

module ZMap :
 sig
  type elt = ZIndexed.t

  val elt_eq : ZIndexed.t -> ZIndexed.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : ZIndexed.t -> 'a1 t -> 'a1

  val set : ZIndexed.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end
