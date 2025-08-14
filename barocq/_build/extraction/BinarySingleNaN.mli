open BinInt
open BinNums
open BinPos
open Bool
open Datatypes
open Round
open SpecFloat

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan
| B754_finite of bool * positive * coq_Z

val coq_SF2B : coq_Z -> coq_Z -> spec_float -> binary_float

val coq_B2SF : coq_Z -> coq_Z -> binary_float -> spec_float

val coq_Bcompare :
  coq_Z -> coq_Z -> binary_float -> binary_float -> comparison option

type mode =
| Coq_mode_NE
| Coq_mode_ZR
| Coq_mode_DN
| Coq_mode_UP
| Coq_mode_NA

val choice_mode : mode -> bool -> coq_Z -> location -> coq_Z

val overflow_to_inf : mode -> bool -> bool

val binary_overflow : coq_Z -> coq_Z -> mode -> bool -> spec_float

val binary_fit_aux :
  coq_Z -> coq_Z -> mode -> bool -> positive -> coq_Z -> spec_float

val binary_round_aux :
  coq_Z -> coq_Z -> mode -> bool -> coq_Z -> coq_Z -> location -> spec_float

val coq_Bmult :
  coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float

val shl_align_fexp : coq_Z -> coq_Z -> positive -> coq_Z -> positive * coq_Z

val binary_round :
  coq_Z -> coq_Z -> mode -> bool -> positive -> coq_Z -> spec_float

val binary_normalize :
  coq_Z -> coq_Z -> mode -> coq_Z -> coq_Z -> bool -> binary_float

val coq_Fplus_naive :
  bool -> positive -> coq_Z -> bool -> positive -> coq_Z -> coq_Z -> coq_Z

val coq_Bplus :
  coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float

val coq_Bminus :
  coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float

val coq_Bdiv :
  coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float
