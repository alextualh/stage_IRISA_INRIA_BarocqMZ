open BinNat
open BinPos
open Datatypes

module Z =
 struct
  (** val double : Stdlib.Int.t -> Stdlib.Int.t **)

  let double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> ((fun p->2*p) p))
      (fun p -> (~-) ((fun p->2*p) p))
      x

  (** val succ_double : Stdlib.Int.t -> Stdlib.Int.t **)

  let succ_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 1)
      (fun p -> ((fun p->1+2*p) p))
      (fun p -> (~-) (Pos.pred_double p))
      x

  (** val pred_double : Stdlib.Int.t -> Stdlib.Int.t **)

  let pred_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (~-) 1)
      (fun p -> (Pos.pred_double p))
      (fun p -> (~-) ((fun p->1+2*p) p))
      x

  (** val pos_sub : int -> int -> Stdlib.Int.t **)

  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> double (pos_sub p q))
        (fun q -> succ_double (pos_sub p q))
        (fun _ -> ((fun p->2*p) p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> pred_double (pos_sub p q))
        (fun q -> double (pos_sub p q))
        (fun _ -> (Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (~-) ((fun p->2*p) q))
        (fun q -> (~-) (Pos.pred_double q))
        (fun _ -> 0)
        y)
      x

  (** val add : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let add = (+)

  (** val opp : Stdlib.Int.t -> Stdlib.Int.t **)

  let opp = (~-)

  (** val pred : Stdlib.Int.t -> Stdlib.Int.t **)

  let pred = Stdlib.Int.pred

  (** val sub : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let sub = (-)

  (** val mul : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let mul = ( * )

  (** val compare : Stdlib.Int.t -> Stdlib.Int.t -> comparison **)

  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt

  (** val leb : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val to_nat : Stdlib.Int.t -> int **)

  let to_nat z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> Pos.to_nat p)
      (fun _ -> 0)
      z

  (** val of_nat : int -> Stdlib.Int.t **)

  let of_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n0 -> (Pos.of_succ_nat n0))
      n

  (** val of_N : int -> Stdlib.Int.t **)

  let of_N = fun p -> p

  (** val iter : Stdlib.Int.t -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let iter n f x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> x)
      (fun p -> Pos.iter f x p)
      (fun _ -> x)
      n

  (** val pos_div_eucl :
      int -> Stdlib.Int.t -> Stdlib.Int.t * Stdlib.Int.t **)

  let rec pos_div_eucl a b =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul ((fun p->2*p) 1) r) 1 in
      if ltb r' b
      then ((mul ((fun p->2*p) 1) q), r')
      else ((add (mul ((fun p->2*p) 1) q) 1), (sub r' b)))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul ((fun p->2*p) 1) r in
      if ltb r' b
      then ((mul ((fun p->2*p) 1) q), r')
      else ((add (mul ((fun p->2*p) 1) q) 1), (sub r' b)))
      (fun _ -> if leb ((fun p->2*p) 1) b then (0, 1) else (1, 0))
      a

  (** val div_eucl :
      Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t * Stdlib.Int.t **)

  let div_eucl a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0, 0))
      (fun a' ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun _ -> pos_div_eucl a' b)
        (fun b' ->
        let (q, r) = pos_div_eucl a' b' in
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ -> ((opp q), 0))
           (fun _ -> ((opp (add q 1)), (add b r)))
           (fun _ -> ((opp (add q 1)), (add b r)))
           r))
        b)
      (fun a' ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun _ ->
        let (q, r) = pos_div_eucl a' b in
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ -> ((opp q), 0))
           (fun _ -> ((opp (add q 1)), (sub b r)))
           (fun _ -> ((opp (add q 1)), (sub b r)))
           r))
        (fun b' -> let (q, r) = pos_div_eucl a' b' in (q, (opp r)))
        b)
      a

  (** val div : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let div a b =
    let (q, _) = div_eucl a b in q

  (** val modulo : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let modulo a b =
    let (_, r) = div_eucl a b in r

  (** val quotrem :
      Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t * Stdlib.Int.t **)

  let quotrem a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0, 0))
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (of_N r)))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (of_N r)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (opp (of_N r))))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (opp (of_N r))))
        b)
      a

  (** val quot : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let quot a b =
    fst (quotrem a b)

  (** val rem : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let rem a b =
    snd (quotrem a b)

  (** val odd : Stdlib.Int.t -> bool **)

  let odd z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> false)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> true)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> true)
        p)
      z

  (** val div2 : Stdlib.Int.t -> Stdlib.Int.t **)

  let div2 z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> (Pos.div2 p))
        (fun _ -> (Pos.div2 p))
        (fun _ -> 0)
        p)
      (fun p -> (~-) (Pos.div2_up p))
      z

  (** val log2 : Stdlib.Int.t -> Stdlib.Int.t **)

  let log2 z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p -> (Pos.size p))
        (fun p -> (Pos.size p))
        (fun _ -> 0)
        p0)
      (fun _ -> 0)
      z

  (** val testbit : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

  let testbit a n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> odd a)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun a0 -> Pos.testbit a0 p)
        (fun a0 -> negb (N.testbit (Pos.pred_N a0) p))
        a)
      (fun _ -> false)
      n

  (** val shiftl : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let shiftl a n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> a)
      (fun p -> Pos.iter (mul ((fun p->2*p) 1)) a p)
      (fun p -> Pos.iter div2 a p)
      n

  (** val shiftr : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let shiftr a n =
    shiftl a (opp n)

  (** val coq_lor : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let coq_lor a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (Pos.coq_lor a0 b0))
        (fun b0 -> (~-) (N.succ_pos (N.ldiff (Pos.pred_N b0) a0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (~-) (N.succ_pos (N.ldiff (Pos.pred_N a0) b0)))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_land (Pos.pred_N a0) (Pos.pred_N b0))))
        b)
      a

  (** val coq_land : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let coq_land a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> 0)
        (fun b0 -> of_N (Pos.coq_land a0 b0))
        (fun b0 -> of_N (N.ldiff a0 (Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> 0)
        (fun b0 -> of_N (N.ldiff b0 (Pos.pred_N a0)))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))
        b)
      a

  (** val coq_lxor : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

  let coq_lxor a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> of_N (Pos.coq_lxor a0 b0))
        (fun b0 -> (~-) (N.succ_pos (N.coq_lxor a0 (Pos.pred_N b0))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lxor (Pos.pred_N a0) b0)))
        (fun b0 -> of_N (N.coq_lxor (Pos.pred_N a0) (Pos.pred_N b0)))
        b)
      a

  (** val eq_dec : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

  let eq_dec x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun p0 -> Pos.eq_dec p p0)
        (fun _ -> false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun _ -> false)
        (fun p0 -> Pos.eq_dec p p0)
        y)
      x
 end
