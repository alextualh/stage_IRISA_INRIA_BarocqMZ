open BinInt
open BinPos
open Coqlib

(** val coq_P_mod_two_p : int -> int -> Stdlib.Int.t **)

let rec coq_P_mod_two_p p n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> 0)
    (fun m ->
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q -> Z.succ_double (coq_P_mod_two_p q m))
      (fun q -> Z.double (coq_P_mod_two_p q m))
      (fun _ -> 1)
      p)
    n

(** val coq_Zshiftin : bool -> Stdlib.Int.t -> Stdlib.Int.t **)

let coq_Zshiftin b x =
  if b then Z.succ_double x else Z.double x

(** val coq_Zzero_ext : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

let coq_Zzero_ext n x =
  Z.iter n (fun rec0 x0 -> coq_Zshiftin (Z.odd x0) (rec0 (Z.div2 x0)))
    (fun _ -> 0) x

(** val coq_Zsign_ext : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t **)

let coq_Zsign_ext n x =
  Z.iter (Z.pred n) (fun rec0 x0 ->
    coq_Zshiftin (Z.odd x0) (rec0 (Z.div2 x0))) (fun x0 ->
    if (&&) (Z.odd x0) (proj_sumbool (zlt 0 n)) then (~-) 1 else 0) x

(** val coq_Z_one_bits :
    int -> Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t list **)

let rec coq_Z_one_bits n x i =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m ->
    if Z.odd x
    then i :: (coq_Z_one_bits m (Z.div2 x) (Z.add i 1))
    else coq_Z_one_bits m (Z.div2 x) (Z.add i 1))
    n

(** val coq_P_is_power2 : int -> bool **)

let rec coq_P_is_power2 p =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun _ -> false)
    (fun q -> coq_P_is_power2 q)
    (fun _ -> true)
    p

(** val coq_Z_is_power2 : Stdlib.Int.t -> Stdlib.Int.t option **)

let coq_Z_is_power2 x =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> None)
    (fun p -> if coq_P_is_power2 p then Some (Z.log2 x) else None)
    (fun _ -> None)
    x

(** val coq_Zsize : Stdlib.Int.t -> Stdlib.Int.t **)

let coq_Zsize x =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> 0)
    (fun p -> (Pos.size p))
    (fun _ -> 0)
    x
