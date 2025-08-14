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

val coq_B2BSN : coq_Z -> coq_Z -> binary_float -> BinarySingleNaN.binary_float

val coq_FF2B : coq_Z -> coq_Z -> full_float -> binary_float

val coq_Bsign : coq_Z -> coq_Z -> binary_float -> bool

val get_nan_pl : coq_Z -> coq_Z -> binary_float -> positive

val build_nan : coq_Z -> coq_Z -> binary_float -> binary_float

val coq_BSN2B :
  coq_Z -> coq_Z -> binary_float -> BinarySingleNaN.binary_float ->
  binary_float

val coq_BSN2B' :
  coq_Z -> coq_Z -> BinarySingleNaN.binary_float -> binary_float

val coq_Bopp :
  coq_Z -> coq_Z -> (binary_float -> binary_float) -> binary_float ->
  binary_float

val coq_Babs :
  coq_Z -> coq_Z -> (binary_float -> binary_float) -> binary_float ->
  binary_float

val coq_Bcompare :
  coq_Z -> coq_Z -> binary_float -> binary_float -> comparison option

val coq_Bmult :
  coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode ->
  binary_float -> binary_float -> binary_float

val binary_normalize :
  coq_Z -> coq_Z -> mode -> coq_Z -> coq_Z -> bool -> binary_float

val coq_Bplus :
  coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode ->
  binary_float -> binary_float -> binary_float

val coq_Bminus :
  coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode ->
  binary_float -> binary_float -> binary_float

val coq_Bdiv :
  coq_Z -> coq_Z -> (binary_float -> binary_float -> binary_float) -> mode ->
  binary_float -> binary_float -> binary_float
