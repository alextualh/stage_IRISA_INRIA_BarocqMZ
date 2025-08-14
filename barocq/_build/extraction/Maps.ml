open BinNums
open Coqlib

module PTree =
 struct
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

  (** val empty : 'a1 t **)

  let empty =
    Empty

  (** val get' : positive -> 'a1 tree' -> 'a1 option **)

  let rec get' p m =
    match p with
    | Coq_xI q ->
      (match m with
       | Node001 m' -> get' q m'
       | Node011 (_, m') -> get' q m'
       | Node101 (_, m') -> get' q m'
       | Node111 (_, _, m') -> get' q m'
       | _ -> None)
    | Coq_xO q ->
      (match m with
       | Node100 m' -> get' q m'
       | Node101 (m', _) -> get' q m'
       | Node110 (m', _) -> get' q m'
       | Node111 (m', _, _) -> get' q m'
       | _ -> None)
    | Coq_xH ->
      (match m with
       | Node010 x -> Some x
       | Node011 (x, _) -> Some x
       | Node110 (_, x) -> Some x
       | Node111 (_, x, _) -> Some x
       | _ -> None)

  (** val get : positive -> 'a1 tree -> 'a1 option **)

  let get p = function
  | Empty -> None
  | Nodes m' -> get' p m'

  (** val set0 : positive -> 'a1 -> 'a1 tree' **)

  let rec set0 p x =
    match p with
    | Coq_xI q -> Node001 (set0 q x)
    | Coq_xO q -> Node100 (set0 q x)
    | Coq_xH -> Node010 x

  (** val set' : positive -> 'a1 -> 'a1 tree' -> 'a1 tree' **)

  let rec set' p x m =
    match p with
    | Coq_xI q ->
      (match m with
       | Node001 r -> Node001 (set' q x r)
       | Node010 y -> Node011 (y, (set0 q x))
       | Node011 (y, r) -> Node011 (y, (set' q x r))
       | Node100 l -> Node101 (l, (set0 q x))
       | Node101 (l, r) -> Node101 (l, (set' q x r))
       | Node110 (l, y) -> Node111 (l, y, (set0 q x))
       | Node111 (l, y, r) -> Node111 (l, y, (set' q x r)))
    | Coq_xO q ->
      (match m with
       | Node001 r -> Node101 ((set0 q x), r)
       | Node010 y -> Node110 ((set0 q x), y)
       | Node011 (y, r) -> Node111 ((set0 q x), y, r)
       | Node100 l -> Node100 (set' q x l)
       | Node101 (l, r) -> Node101 ((set' q x l), r)
       | Node110 (l, y) -> Node110 ((set' q x l), y)
       | Node111 (l, y, r) -> Node111 ((set' q x l), y, r))
    | Coq_xH ->
      (match m with
       | Node001 r -> Node011 (x, r)
       | Node010 _ -> Node010 x
       | Node011 (_, r) -> Node011 (x, r)
       | Node100 l -> Node110 (l, x)
       | Node101 (l, r) -> Node111 (l, x, r)
       | Node110 (l, _) -> Node110 (l, x)
       | Node111 (l, _, r) -> Node111 (l, x, r))

  (** val set : positive -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let set p x = function
  | Empty -> Nodes (set0 p x)
  | Nodes m' -> Nodes (set' p x m')

  (** val prev_append : positive -> positive -> positive **)

  let rec prev_append i j =
    match i with
    | Coq_xI i' -> prev_append i' (Coq_xI j)
    | Coq_xO i' -> prev_append i' (Coq_xO j)
    | Coq_xH -> j

  (** val prev : positive -> positive **)

  let prev i =
    prev_append i Coq_xH

  (** val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree' **)

  let rec map1' f = function
  | Node001 r -> Node001 (map1' f r)
  | Node010 x -> Node010 (f x)
  | Node011 (x, r) -> Node011 ((f x), (map1' f r))
  | Node100 l -> Node100 (map1' f l)
  | Node101 (l, r) -> Node101 ((map1' f l), (map1' f r))
  | Node110 (l, x) -> Node110 ((map1' f l), (f x))
  | Node111 (l, x, r) -> Node111 ((map1' f l), (f x), (map1' f r))

  (** val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map1 f = function
  | Empty -> Empty
  | Nodes m0 -> Nodes (map1' f m0)

  (** val fold' :
      ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 tree' -> 'a2 -> 'a2 **)

  let rec fold' f i m v =
    match m with
    | Node001 r -> fold' f (Coq_xI i) r v
    | Node010 x -> f v (prev i) x
    | Node011 (x, r) -> fold' f (Coq_xI i) r (f v (prev i) x)
    | Node100 l -> fold' f (Coq_xO i) l v
    | Node101 (l, r) -> fold' f (Coq_xI i) r (fold' f (Coq_xO i) l v)
    | Node110 (l, x) -> f (fold' f (Coq_xO i) l v) (prev i) x
    | Node111 (l, x, r) ->
      fold' f (Coq_xI i) r (f (fold' f (Coq_xO i) l v) (prev i) x)

  (** val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m v =
    match m with
    | Empty -> v
    | Nodes m' -> fold' f Coq_xH m' v
 end

module PMap =
 struct
  type 'a t = 'a * 'a PTree.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init x =
    (x, PTree.empty)

  (** val get : positive -> 'a1 t -> 'a1 **)

  let get i m =
    match PTree.get i (snd m) with
    | Some x -> x
    | None -> fst m

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i x m =
    ((fst m), (PTree.set i x (snd m)))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    ((f (fst m)), (PTree.map1 f (snd m)))
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module IMap =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PMap.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init =
    PMap.init

  (** val get : X.t -> 'a1 t -> 'a1 **)

  let get i m =
    PMap.get (X.index i) m

  (** val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i v m =
    PMap.set (X.index i) v m

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map =
    PMap.map
 end

module ZIndexed =
 struct
  type t = coq_Z

  (** val index : coq_Z -> positive **)

  let index = function
  | Z0 -> Coq_xH
  | Zpos p -> Coq_xO p
  | Zneg p -> Coq_xI p

  (** val eq : coq_Z -> coq_Z -> bool **)

  let eq =
    zeq
 end

module ZMap = IMap(ZIndexed)
