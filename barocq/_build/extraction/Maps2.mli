open BinNums
open Errors
open Ident
open List0
open Maps
open Monads
open String0

module StringIndexed :
 sig
  type t = char list

  val eq : char list -> char list -> bool

  val index : Ident.t -> positive
 end

module SMap :
 sig
  type elt = StringIndexed.t

  val elt_eq : StringIndexed.t -> StringIndexed.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : StringIndexed.t -> 'a1 t -> 'a1

  val set : StringIndexed.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module STree :
 sig
  type elt = StringIndexed.t

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val fold : ('a2 -> StringIndexed.t -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
 end

module type DEC_TYPE =
 sig
  type t

  val eq : t -> t -> bool
 end

module MapList :
 functor (X:DEC_TYPE) ->
 sig
  type key = X.t

  val key_eq : X.t -> X.t -> bool

  type 'v t = (key * 'v) list

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 -> 'a1

  val find_err : key -> 'a1 t -> 'a1 res

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map_err : ('a1 -> 'a2 res) -> 'a1 t -> 'a2 t res

  val mem : key -> 'a1 t -> bool

  val nodup : 'a1 t -> bool

  val merge : 'a1 t -> 'a1 t -> 'a1 t

  val fold_left : ('a2 -> key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val empty : 'a1 t
 end

module SMapList :
 sig
  type key = StringIndexed.t

  val key_eq : StringIndexed.t -> StringIndexed.t -> bool

  type 'v t = (key * 'v) list

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val find : key -> 'a1 t -> 'a1 -> 'a1

  val find_err : key -> 'a1 t -> 'a1 res

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map_err : ('a1 -> 'a2 res) -> 'a1 t -> 'a2 t res

  val mem : key -> 'a1 t -> bool

  val nodup : 'a1 t -> bool

  val merge : 'a1 t -> 'a1 t -> 'a1 t

  val fold_left : ('a2 -> key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val empty : 'a1 t
 end
