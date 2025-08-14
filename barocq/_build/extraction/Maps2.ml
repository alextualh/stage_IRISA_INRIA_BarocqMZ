open BinNums
open Errors
open Ident
open List0
open Maps
open Monads
open String0

module StringIndexed =
 struct
  type t = char list

  (** val eq : char list -> char list -> bool **)

  let eq =
    string_dec

  (** val index : Ident.t -> positive **)

  let index =
    to_pos
 end

module SMap = IMap(StringIndexed)

module STree =
 struct
  type elt = StringIndexed.t

  type 'x t = 'x PTree.t

  (** val empty : 'a1 t **)

  let empty =
    PTree.empty

  (** val get : elt -> 'a1 t -> 'a1 option **)

  let get k m =
    PTree.get (StringIndexed.index k) m

  (** val set : elt -> 'a1 -> 'a1 t -> 'a1 t **)

  let set k v m =
    PTree.set (StringIndexed.index k) v m

  (** val fold :
      ('a2 -> StringIndexed.t -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m v =
    PTree.fold (fun acc ki vi -> f acc (of_pos ki) vi) m v
 end

module type DEC_TYPE =
 sig
  type t

  val eq : t -> t -> bool
 end

module MapList =
 functor (X:DEC_TYPE) ->
 struct
  type key = X.t

  (** val key_eq : X.t -> X.t -> bool **)

  let key_eq =
    X.eq

  type 'v t = (key * 'v) list

  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec add k v = function
  | [] -> (k, v) :: []
  | p :: l' ->
    let (x, vx) = p in
    if key_eq x k then (k, v) :: l' else (x, vx) :: (add k v l')

  (** val find : key -> 'a1 t -> 'a1 -> 'a1 **)

  let rec find k l default =
    match l with
    | [] -> default
    | p :: l' -> let (x, v) = p in if key_eq x k then v else find k l' default

  (** val find_err : key -> 'a1 t -> 'a1 res **)

  let rec find_err k = function
  | [] -> MonError.fail
  | p :: l' ->
    let (x, v) = p in if key_eq x k then MonError.ret v else find_err k l'

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f l =
    map (fun pat -> let (x, v) = pat in (x, (f v))) l

  (** val map_err : ('a1 -> 'a2 res) -> 'a1 t -> 'a2 t res **)

  let map_err f l =
    mmap (fun pat ->
      let (x, v) = pat in MonError.bind (f v) (fun a -> MonError.ret (x, a)))
      l

  (** val mem : key -> 'a1 t -> bool **)

  let rec mem k = function
  | [] -> false
  | p :: l' -> let (x, _) = p in if key_eq x k then true else mem k l'

  (** val nodup : 'a1 t -> bool **)

  let rec nodup = function
  | [] -> true
  | p :: l' -> let (k, _) = p in if mem k l' then false else nodup l'

  (** val merge : 'a1 t -> 'a1 t -> 'a1 t **)

  let rec merge l1 l2 =
    match l1 with
    | [] -> l2
    | p :: l1' ->
      let (k1, v1) = p in
      if mem k1 l2 then merge l1' l2 else (k1, v1) :: (merge l1' l2)

  (** val fold_left : ('a2 -> key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold_left f l acc =
    fold_left (fun a pat -> let (k, v) = pat in f a k v) l acc

  (** val empty : 'a1 t **)

  let empty =
    []
 end

module SMapList = MapList(StringIndexed)
