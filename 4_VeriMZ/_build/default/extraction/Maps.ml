
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

  (** val get' : int -> 'a1 tree' -> 'a1 option **)

  let rec get' p m =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q ->
      match m with
      | Node001 m' -> get' q m'
      | Node011 (_, m') -> get' q m'
      | Node101 (_, m') -> get' q m'
      | Node111 (_, _, m') -> get' q m'
      | _ -> None)
      (fun q ->
      match m with
      | Node100 m' -> get' q m'
      | Node101 (m', _) -> get' q m'
      | Node110 (m', _) -> get' q m'
      | Node111 (m', _, _) -> get' q m'
      | _ -> None)
      (fun _ ->
      match m with
      | Node010 x -> Some x
      | Node011 (x, _) -> Some x
      | Node110 (_, x) -> Some x
      | Node111 (_, x, _) -> Some x
      | _ -> None)
      p

  (** val get : int -> 'a1 tree -> 'a1 option **)

  let get p = function
  | Empty -> None
  | Nodes m' -> get' p m'

  (** val set0 : int -> 'a1 -> 'a1 tree' **)

  let rec set0 p x =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q -> Node001 (set0 q x))
      (fun q -> Node100 (set0 q x))
      (fun _ -> Node010 x)
      p

  (** val set' : int -> 'a1 -> 'a1 tree' -> 'a1 tree' **)

  let rec set' p x m =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q ->
      match m with
      | Node001 r -> Node001 (set' q x r)
      | Node010 y -> Node011 (y, (set0 q x))
      | Node011 (y, r) -> Node011 (y, (set' q x r))
      | Node100 l -> Node101 (l, (set0 q x))
      | Node101 (l, r) -> Node101 (l, (set' q x r))
      | Node110 (l, y) -> Node111 (l, y, (set0 q x))
      | Node111 (l, y, r) -> Node111 (l, y, (set' q x r)))
      (fun q ->
      match m with
      | Node001 r -> Node101 ((set0 q x), r)
      | Node010 y -> Node110 ((set0 q x), y)
      | Node011 (y, r) -> Node111 ((set0 q x), y, r)
      | Node100 l -> Node100 (set' q x l)
      | Node101 (l, r) -> Node101 ((set' q x l), r)
      | Node110 (l, y) -> Node110 ((set' q x l), y)
      | Node111 (l, y, r) -> Node111 ((set' q x l), y, r))
      (fun _ ->
      match m with
      | Node001 r -> Node011 (x, r)
      | Node010 _ -> Node010 x
      | Node011 (_, r) -> Node011 (x, r)
      | Node100 l -> Node110 (l, x)
      | Node101 (l, r) -> Node111 (l, x, r)
      | Node110 (l, _) -> Node110 (l, x)
      | Node111 (l, _, r) -> Node111 (l, x, r))
      p

  (** val set : int -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let set p x = function
  | Empty -> Nodes (set0 p x)
  | Nodes m' -> Nodes (set' p x m')
 end
