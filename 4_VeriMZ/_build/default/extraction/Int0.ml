open Archi
open BinInt
open Coqlib
open Zbits
open Zpower

module Wordsize_Intptr =
 struct
  (** val wordsize : int **)

  let wordsize =
    if ptr64
    then Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    else Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ 0)))))))))))))))))))))))))))))))
 end

module Intptr =
 struct
  (** val wordsize : int **)

  let wordsize =
    Wordsize_Intptr.wordsize

  (** val modulus : Stdlib.Int.t **)

  let modulus =
    two_power_nat wordsize

  type int =
    Stdlib.Int.t
    (* singleton inductive, whose constructor was mkint *)

  (** val intval : int -> Stdlib.Int.t **)

  let intval i =
    i

  (** val coq_Z_mod_modulus : Stdlib.Int.t -> Stdlib.Int.t **)

  let coq_Z_mod_modulus x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> coq_P_mod_two_p p wordsize)
      (fun p ->
      let r = coq_P_mod_two_p p wordsize in
      if zeq r 0 then 0 else Z.sub modulus r)
      x

  (** val unsigned : int -> Stdlib.Int.t **)

  let unsigned n =
    n

  (** val repr : Stdlib.Int.t -> int **)

  let repr =
    coq_Z_mod_modulus

  (** val zero : int **)

  let zero =
    repr 0

  (** val one : int **)

  let one =
    repr 1

  (** val eq : int -> int -> bool **)

  let eq x y =
    if zeq (unsigned x) (unsigned y) then true else false

  (** val add : int -> int -> int **)

  let add x y =
    repr (Z.add (unsigned x) (unsigned y))

  (** val sub : int -> int -> int **)

  let sub x y =
    repr (Z.sub (unsigned x) (unsigned y))

  (** val divu : int -> int -> int **)

  let divu x y =
    repr (Z.div (unsigned x) (unsigned y))

  (** val coq_and : int -> int -> int **)

  let coq_and x y =
    repr (Z.coq_land (unsigned x) (unsigned y))

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))

  (** val sign_ext : Stdlib.Int.t -> int -> int **)

  let sign_ext n x =
    repr (coq_Zsign_ext n (unsigned x))

  (** val unsigned_to_nat : int -> int **)

  let unsigned_to_nat iptr =
    Z.to_nat (unsigned iptr)

  (** val of_nat : int -> int **)

  let of_nat n =
    repr (Z.of_nat n)
 end
