open BinInt
open BinNums
open BinPos
open Datatypes
open Zbool
open Zpower

type spec_float =
| S754_zero of bool
| S754_infinity of bool
| S754_nan
| S754_finite of bool * positive * coq_Z

val emin : coq_Z -> coq_Z -> coq_Z

val fexp : coq_Z -> coq_Z -> coq_Z -> coq_Z

val digits2_pos : positive -> positive

val coq_Zdigits2 : coq_Z -> coq_Z

val iter_pos : ('a1 -> 'a1) -> positive -> 'a1 -> 'a1

type location =
| Coq_loc_Exact
| Coq_loc_Inexact of comparison

type shr_record = { shr_m : coq_Z; shr_r : bool; shr_s : bool }

val shr_1 : shr_record -> shr_record

val loc_of_shr_record : shr_record -> location

val shr_record_of_loc : coq_Z -> location -> shr_record

val shr : shr_record -> coq_Z -> coq_Z -> shr_record * coq_Z

val shr_fexp :
  coq_Z -> coq_Z -> coq_Z -> coq_Z -> location -> shr_record * coq_Z

val shl_align : positive -> coq_Z -> coq_Z -> positive * coq_Z

val coq_SFcompare : spec_float -> spec_float -> comparison option

val cond_Zopp : bool -> coq_Z -> coq_Z

val new_location_even : coq_Z -> coq_Z -> location

val new_location_odd : coq_Z -> coq_Z -> location

val new_location : coq_Z -> coq_Z -> location

val coq_SFdiv_core_binary :
  coq_Z -> coq_Z -> coq_Z -> coq_Z -> coq_Z -> coq_Z ->
  (coq_Z * coq_Z) * location
