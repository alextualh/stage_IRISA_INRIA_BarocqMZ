open BinNums
open BinarySingleNaN
open Datatypes

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * positive
| F754_finite of bool * positive * coq_Z

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * positive
| B754_finite of bool * positive * coq_Z

(** val coq_B2BSN :
    coq_Z -> coq_Z -> binary_float -> BinarySingleNaN.binary_float **)

let coq_B2BSN _ _ = function
| B754_zero s -> BinarySingleNaN.B754_zero s
| B754_infinity s -> BinarySingleNaN.B754_infinity s
| B754_nan (_, _) -> BinarySingleNaN.B754_nan
| B754_finite (s, m, e) -> BinarySingleNaN.B754_finite (s, m, e)

(** val coq_FF2B : coq_Z -> coq_Z -> full_float -> binary_float **)

let coq_FF2B _ _ = function
| F754_zero s -> B754_zero s
| F754_infinity s -> B754_infinity s
| F754_nan (b, pl) -> B754_nan (b, pl)
| F754_finite (s, m, e) -> B754_finite (s, m, e)

(** val coq_Bsign : coq_Z -> coq_Z -> binary_float -> bool **)

let coq_Bsign _ _ = function
| B754_zero s -> s
| B754_infinity s -> s
| B754_nan (s, _) -> s
| B754_finite (s, _, _) -> s

(** val get_nan_pl : coq_Z -> coq_Z -> binary_float -> positive **)

let get_nan_pl _ _ = function
| B754_nan (_, pl) -> pl
| _ -> Coq_xH

(** val build_nan : coq_Z -> coq_Z -> binary_float -> binary_float **)

let build_nan prec emax x =
  B754_nan ((coq_Bsign prec emax x), (get_nan_pl prec emax x))

(** val coq_BSN2B :
    coq_Z -> coq_Z -> binary_float -> BinarySingleNaN.binary_float ->
    binary_float **)

let coq_BSN2B prec emax nan = function
| BinarySingleNaN.B754_zero s -> B754_zero s
| BinarySingleNaN.B754_infinity s -> B754_infinity s
| BinarySingleNaN.B754_nan -> build_nan prec emax nan
| BinarySingleNaN.B754_finite (s, m, e) -> B754_finite (s, m, e)

(** val coq_BSN2B' :
    coq_Z -> coq_Z -> BinarySingleNaN.binary_float -> binary_float **)

let coq_BSN2B' _ _ = function
| BinarySingleNaN.B754_zero s -> B754_zero s
| BinarySingleNaN.B754_infinity s -> B754_infinity s
| BinarySingleNaN.B754_nan -> assert false (* absurd case *)
| BinarySingleNaN.B754_finite (s, m, e) -> B754_finite (s, m, e)

(** val coq_Bopp :
    coq_Z -> coq_Z -> (binary_float -> binary_float) -> binary_float ->
    binary_float **)

let coq_Bopp prec emax opp_nan x = match x with
| B754_zero sx -> B754_zero (negb sx)
| B754_infinity sx -> B754_infinity (negb sx)
| B754_nan (_, _) -> build_nan prec emax (opp_nan x)
| B754_finite (sx, mx, ex) -> B754_finite ((negb sx), mx, ex)

(** val coq_Babs :
    coq_Z -> coq_Z -> (binary_float -> binary_float) -> binary_float ->
    binary_float **)

let coq_Babs prec emax abs_nan x = match x with
| B754_zero _ -> B754_zero false
| B754_infinity _ -> B754_infinity false
| B754_nan (_, _) -> build_nan prec emax (abs_nan x)
| B754_finite (_, mx, ex) -> B754_finite (false, mx, ex)

(** val coq_Bcompare :
    coq_Z -> coq_Z -> binary_float -> binary_float -> comparison option **)

let coq_Bcompare prec emax f1 f2 =
  coq_Bcompare prec emax (coq_B2BSN prec emax f1) (coq_B2BSN prec emax f2)

(** val coq_Bmult :
    coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode
    -> binary_float -> binary_float -> binary_float **)

let coq_Bmult prec emax mult_nan m x y =
  coq_BSN2B prec emax (mult_nan x y)
    (coq_Bmult prec emax m (coq_B2BSN prec emax x) (coq_B2BSN prec emax y))

(** val binary_normalize :
    coq_Z -> coq_Z -> mode -> coq_Z -> coq_Z -> bool -> binary_float **)

let binary_normalize prec emax mode0 m e szero =
  coq_BSN2B' prec emax (binary_normalize prec emax mode0 m e szero)

(** val coq_Bplus :
    coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode
    -> binary_float -> binary_float -> binary_float **)

let coq_Bplus prec emax plus_nan m x y =
  coq_BSN2B prec emax (plus_nan x y)
    (coq_Bplus prec emax m (coq_B2BSN prec emax x) (coq_B2BSN prec emax y))

(** val coq_Bminus :
    coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode
    -> binary_float -> binary_float -> binary_float **)

let coq_Bminus prec emax minus_nan m x y =
  coq_BSN2B prec emax (minus_nan x y)
    (coq_Bminus prec emax m (coq_B2BSN prec emax x) (coq_B2BSN prec emax y))

(** val coq_Bdiv :
    coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode
    -> binary_float -> binary_float -> binary_float **)

let coq_Bdiv prec emax div_nan m x y =
  coq_BSN2B prec emax (div_nan x y)
    (coq_Bdiv prec emax m (coq_B2BSN prec emax x) (coq_B2BSN prec emax y))
