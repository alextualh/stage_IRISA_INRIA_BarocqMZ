open Archi
open BinInt
open BinNums
open BinPos
open Binary
open BinarySingleNaN
open Bits
open Coqlib
open Datatypes
open IEEE754_extra
open Integers
open Zaux
open Zbits

type float = binary64

type float32 = binary32

(** val cmp_of_comparison :
    comparison -> Datatypes.comparison option -> bool **)

let cmp_of_comparison c x =
  match c with
  | Ceq ->
    (match x with
     | Some c0 -> (match c0 with
                   | Eq -> true
                   | _ -> false)
     | None -> false)
  | Cne ->
    (match x with
     | Some c0 -> (match c0 with
                   | Eq -> false
                   | _ -> true)
     | None -> true)
  | Clt ->
    (match x with
     | Some c0 -> (match c0 with
                   | Lt -> true
                   | _ -> false)
     | None -> false)
  | Cle ->
    (match x with
     | Some c0 -> (match c0 with
                   | Gt -> false
                   | _ -> true)
     | None -> false)
  | Cgt ->
    (match x with
     | Some c0 -> (match c0 with
                   | Gt -> true
                   | _ -> false)
     | None -> false)
  | Cge ->
    (match x with
     | Some c0 -> (match c0 with
                   | Lt -> false
                   | _ -> true)
     | None -> false)

(** val quiet_nan_64_payload : positive -> positive **)

let quiet_nan_64_payload p =
  Z.to_pos
    (coq_P_mod_two_p
      (Pos.coq_lor p
        (iter_nat (fun x -> Coq_xO x) (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          O))))))))))))))))))))))))))))))))))))))))))))))))))) Coq_xH)) (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S O)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val quiet_nan_64 : (bool * positive) -> float **)

let quiet_nan_64 = function
| (s, p) -> Binary.B754_nan (s, (quiet_nan_64_payload p))

(** val default_nan_64 : float **)

let default_nan_64 =
  quiet_nan_64 default_nan_64

(** val quiet_nan_32_payload : positive -> positive **)

let quiet_nan_32_payload p =
  Z.to_pos
    (coq_P_mod_two_p
      (Pos.coq_lor p
        (iter_nat (fun x -> Coq_xO x) (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S O)))))))))))))))))))))) Coq_xH)) (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O))))))))))))))))))))))))

(** val quiet_nan_32 : (bool * positive) -> float32 **)

let quiet_nan_32 = function
| (s, p) -> Binary.B754_nan (s, (quiet_nan_32_payload p))

(** val default_nan_32 : float32 **)

let default_nan_32 =
  quiet_nan_32 default_nan_32

module Float =
 struct
  (** val expand_nan_payload : positive -> positive **)

  let expand_nan_payload p =
    Pos.shiftl_nat p (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S O)))))))))))))))))))))))))))))

  (** val expand_nan : bool -> positive -> Binary.binary_float **)

  let expand_nan s p =
    Binary.B754_nan (s, (expand_nan_payload p))

  (** val of_single_nan : float32 -> float **)

  let of_single_nan = function
  | Binary.B754_nan (s, p) ->
    if float_conversion_default_nan
    then default_nan_64
    else if float_of_single_preserves_sNaN
         then expand_nan s p
         else quiet_nan_64 (s, (expand_nan_payload p))
  | _ -> default_nan_64

  (** val reduce_nan_payload : positive -> positive **)

  let reduce_nan_payload p =
    Pos.shiftr_nat (quiet_nan_64_payload p) (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))

  (** val to_single_nan : float -> float32 **)

  let to_single_nan = function
  | Binary.B754_nan (s, p) ->
    if float_conversion_default_nan
    then default_nan_32
    else quiet_nan_32 (s, (reduce_nan_payload p))
  | _ -> default_nan_32

  (** val neg_nan : float -> float **)

  let neg_nan = function
  | Binary.B754_nan (s, p) -> Binary.B754_nan ((negb s), p)
  | _ -> default_nan_64

  (** val abs_nan : float -> float **)

  let abs_nan = function
  | Binary.B754_nan (_, p) -> Binary.B754_nan (false, p)
  | _ -> default_nan_64

  (** val cons_pl :
      float -> (bool * positive) list -> (bool * positive) list **)

  let cons_pl x l =
    match x with
    | Binary.B754_nan (s, p) -> (s, p) :: l
    | _ -> l

  (** val binop_nan : float -> float -> float **)

  let binop_nan x y =
    quiet_nan_64 (choose_nan_64 (cons_pl x (cons_pl y [])))

  (** val zero : float **)

  let zero =
    Binary.B754_zero false

  (** val eq_dec : float -> float -> bool **)

  let eq_dec =
    coq_Beq_dec (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))
      (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO Coq_xH)))))))))))

  (** val neg : float -> float **)

  let neg =
    coq_Bopp (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))))))) neg_nan

  (** val abs : float -> float **)

  let abs =
    coq_Babs (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))))))) abs_nan

  (** val add : float -> float -> float **)

  let add =
    Binary.coq_Bplus (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
      Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_nan Coq_mode_NE

  (** val sub : float -> float -> float **)

  let sub =
    Binary.coq_Bminus (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
      Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_nan Coq_mode_NE

  (** val mul : float -> float -> float **)

  let mul =
    Binary.coq_Bmult (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
      Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_nan Coq_mode_NE

  (** val div : float -> float -> float **)

  let div =
    Binary.coq_Bdiv (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
      Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_nan Coq_mode_NE

  (** val compare : float -> float -> Datatypes.comparison option **)

  let compare f1 f2 =
    Binary.coq_Bcompare (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
      Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f1 f2

  (** val cmp : comparison -> float -> float -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (compare f1 f2)

  (** val of_single : float32 -> float **)

  let of_single =
    coq_Bconv (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))) (Zpos
      (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      Coq_xH))))))))))) of_single_nan Coq_mode_NE

  (** val to_single : float -> float32 **)

  let to_single =
    coq_Bconv (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))
      (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO Coq_xH))))))))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI
      Coq_xH))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH)))))))) to_single_nan Coq_mode_NE

  (** val to_int : float -> Int.int option **)

  let to_int f =
    option_map Int.repr
      (coq_ZofB_range (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
        Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f Int.min_signed
        Int.max_signed)

  (** val to_intu : float -> Int.int option **)

  let to_intu f =
    option_map Int.repr
      (coq_ZofB_range (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
        Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f Z0
        Int.max_unsigned)

  (** val to_long : float -> Int64.int option **)

  let to_long f =
    option_map Int64.repr
      (coq_ZofB_range (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
        Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f Int64.min_signed
        Int64.max_signed)

  (** val to_longu : float -> Int64.int option **)

  let to_longu f =
    option_map Int64.repr
      (coq_ZofB_range (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
        Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f Z0
        Int64.max_unsigned)

  (** val of_int : Int.int -> float **)

  let of_int n =
    coq_BofZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))))))) (Int.signed n)

  (** val of_intu : Int.int -> float **)

  let of_intu n =
    coq_BofZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))))))) (Int.unsigned n)

  (** val of_long : Int64.int -> float **)

  let of_long n =
    coq_BofZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))))))) (Int64.signed n)

  (** val of_longu : Int64.int -> float **)

  let of_longu n =
    coq_BofZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))))))) (Int64.unsigned n)

  (** val from_parsed : positive -> positive -> coq_Z -> float **)

  let from_parsed base intPart expPart =
    coq_Bparse (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))
      (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO Coq_xH))))))))))) base intPart expPart

  (** val to_bits : float -> Int64.int **)

  let to_bits f =
    Int64.repr (bits_of_b64 f)

  (** val of_bits : Int64.int -> float **)

  let of_bits b =
    b64_of_bits (Int64.unsigned b)

  (** val from_words : Int.int -> Int.int -> float **)

  let from_words hi lo =
    of_bits (Int64.ofwords hi lo)
 end

module Float32 =
 struct
  (** val neg_nan : float32 -> float32 **)

  let neg_nan = function
  | Binary.B754_nan (s, p) -> Binary.B754_nan ((negb s), p)
  | _ -> default_nan_32

  (** val abs_nan : float32 -> float32 **)

  let abs_nan = function
  | Binary.B754_nan (_, p) -> Binary.B754_nan (false, p)
  | _ -> default_nan_32

  (** val cons_pl :
      float32 -> (bool * positive) list -> (bool * positive) list **)

  let cons_pl x l =
    match x with
    | Binary.B754_nan (s, p) -> (s, p) :: l
    | _ -> l

  (** val binop_nan : float32 -> float32 -> float32 **)

  let binop_nan x y =
    quiet_nan_32 (choose_nan_32 (cons_pl x (cons_pl y [])))

  (** val zero : float32 **)

  let zero =
    Binary.B754_zero false

  (** val eq_dec : float32 -> float32 -> bool **)

  let eq_dec =
    coq_Beq_dec (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))

  (** val neg : float32 -> float32 **)

  let neg =
    coq_Bopp (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))) neg_nan

  (** val abs : float32 -> float32 **)

  let abs =
    coq_Babs (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))) abs_nan

  (** val add : float32 -> float32 -> float32 **)

  let add =
    Binary.coq_Bplus (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      binop_nan Coq_mode_NE

  (** val sub : float32 -> float32 -> float32 **)

  let sub =
    Binary.coq_Bminus (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      binop_nan Coq_mode_NE

  (** val mul : float32 -> float32 -> float32 **)

  let mul =
    Binary.coq_Bmult (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      binop_nan Coq_mode_NE

  (** val div : float32 -> float32 -> float32 **)

  let div =
    Binary.coq_Bdiv (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      binop_nan Coq_mode_NE

  (** val compare : float32 -> float32 -> Datatypes.comparison option **)

  let compare f1 f2 =
    Binary.coq_Bcompare (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
      (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      Coq_xH)))))))) f1 f2

  (** val cmp : comparison -> float32 -> float32 -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (compare f1 f2)

  (** val to_int : float32 -> Int.int option **)

  let to_int f =
    option_map Int.repr
      (coq_ZofB_range (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))) f Int.min_signed Int.max_signed)

  (** val to_intu : float32 -> Int.int option **)

  let to_intu f =
    option_map Int.repr
      (coq_ZofB_range (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))) f Z0 Int.max_unsigned)

  (** val to_long : float32 -> Int64.int option **)

  let to_long f =
    option_map Int64.repr
      (coq_ZofB_range (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))) f Int64.min_signed Int64.max_signed)

  (** val to_longu : float32 -> Int64.int option **)

  let to_longu f =
    option_map Int64.repr
      (coq_ZofB_range (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))) f Z0 Int64.max_unsigned)

  (** val of_int : Int.int -> float32 **)

  let of_int n =
    coq_BofZ (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      (Int.signed n)

  (** val of_intu : Int.int -> float32 **)

  let of_intu n =
    coq_BofZ (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      (Int.unsigned n)

  (** val of_long : Int64.int -> float32 **)

  let of_long n =
    coq_BofZ (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      (Int64.signed n)

  (** val of_longu : Int64.int -> float32 **)

  let of_longu n =
    coq_BofZ (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      (Int64.unsigned n)

  (** val from_parsed : positive -> positive -> coq_Z -> float32 **)

  let from_parsed base intPart expPart =
    coq_Bparse (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
      base intPart expPart

  (** val to_bits : float32 -> Int.int **)

  let to_bits f =
    Int.repr (bits_of_b32 f)

  (** val of_bits : Int.int -> float32 **)

  let of_bits b =
    b32_of_bits (Int.unsigned b)
 end
