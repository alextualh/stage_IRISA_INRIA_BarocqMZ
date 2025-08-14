open Datatypes

(** val add : int -> int -> int **)

let rec add = (+)

(** val mul : int -> int -> int **)

let rec mul = ( * )

(** val sub : int -> int -> int **)

let rec sub = fun n m -> Stdlib.max 0 (n-m)

(** val eqb : int -> int -> bool **)

let rec eqb n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> true)
      (fun _ -> false)
      m)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> false)
      (fun m' -> eqb n' m')
      m)
    n

(** val pow : int -> int -> int **)

let rec pow n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> Stdlib.Int.succ 0)
    (fun m0 -> mul n (pow n m0))
    m

(** val divmod : int -> int -> int -> int -> int * int **)

let rec divmod x y q u =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> (q, u))
    (fun x' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> divmod x' y (Stdlib.Int.succ q) y)
      (fun u' -> divmod x' y q u')
      u)
    x

(** val div : int -> int -> int **)

let div x y =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> y)
    (fun y' -> fst (divmod x y' 0 y'))
    y

(** val modulo : int -> int -> int **)

let modulo x y =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> x)
    (fun y' -> sub y' (snd (divmod x y' 0 y')))
    y
