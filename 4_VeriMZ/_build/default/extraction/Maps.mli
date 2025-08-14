
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

  val get' : int -> 'a1 tree' -> 'a1 option

  val get : int -> 'a1 tree -> 'a1 option

  val set0 : int -> 'a1 -> 'a1 tree'

  val set' : int -> 'a1 -> 'a1 tree' -> 'a1 tree'

  val set : int -> 'a1 -> 'a1 tree -> 'a1 tree
 end
