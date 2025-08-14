open BinNums

module PositiveSet =
 struct
  type elt = positive

  type tree =
  | Leaf
  | Node of tree * bool * tree

  type t = tree

  (** val empty : t **)

  let empty =
    Leaf

  (** val mem : positive -> t -> bool **)

  let rec mem i = function
  | Leaf -> false
  | Node (l, o, r) ->
    (match i with
     | Coq_xI i0 -> mem i0 r
     | Coq_xO i0 -> mem i0 l
     | Coq_xH -> o)

  (** val add : positive -> t -> t **)

  let rec add i = function
  | Leaf ->
    (match i with
     | Coq_xI i0 -> Node (Leaf, false, (add i0 Leaf))
     | Coq_xO i0 -> Node ((add i0 Leaf), false, Leaf)
     | Coq_xH -> Node (Leaf, true, Leaf))
  | Node (l, o, r) ->
    (match i with
     | Coq_xI i0 -> Node (l, o, (add i0 r))
     | Coq_xO i0 -> Node ((add i0 l), o, r)
     | Coq_xH -> Node (l, true, r))

  (** val union : t -> t -> t **)

  let rec union m m' =
    match m with
    | Leaf -> m'
    | Node (l, o, r) ->
      (match m' with
       | Leaf -> m
       | Node (l', o', r') -> Node ((union l l'), ((||) o o'), (union r r')))

  (** val rev_append : elt -> elt -> elt **)

  let rec rev_append y x =
    match y with
    | Coq_xI y0 -> rev_append y0 (Coq_xI x)
    | Coq_xO y0 -> rev_append y0 (Coq_xO x)
    | Coq_xH -> x

  (** val rev : elt -> elt **)

  let rev x =
    rev_append x Coq_xH

  (** val xelements : t -> positive -> positive list -> positive list **)

  let rec xelements m i a =
    match m with
    | Leaf -> a
    | Node (l, b, r) ->
      if b
      then xelements l (Coq_xO i) ((rev i) :: (xelements r (Coq_xI i) a))
      else xelements l (Coq_xO i) (xelements r (Coq_xI i) a)

  (** val elements : t -> positive list **)

  let elements m =
    xelements m Coq_xH []
 end
