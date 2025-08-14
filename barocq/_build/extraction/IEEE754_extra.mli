open BinInt
open BinNums
open BinPos
open Binary
open BinarySingleNaN
open Datatypes
open SpecFloat
open Zaux

val coq_Beq_dec :
  coq_Z -> coq_Z -> Binary.binary_float -> Binary.binary_float -> bool

val coq_BofZ : coq_Z -> coq_Z -> coq_Z -> Binary.binary_float

val coq_ZofB : coq_Z -> coq_Z -> Binary.binary_float -> coq_Z option

val coq_ZofB_range :
  coq_Z -> coq_Z -> Binary.binary_float -> coq_Z -> coq_Z -> coq_Z option

val pos_pow : positive -> positive -> positive

val coq_Bparse :
  coq_Z -> coq_Z -> positive -> positive -> coq_Z -> Binary.binary_float

val coq_Bconv :
  coq_Z -> coq_Z -> coq_Z -> coq_Z -> (Binary.binary_float ->
  Binary.binary_float) -> mode -> Binary.binary_float -> Binary.binary_float
