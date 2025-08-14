open AST
open BinNums
open Ctypes

val tint : coq_type

val tuint : coq_type

val tbool : coq_type

val tlong : coq_type

val tulong : coq_type

val tptr : coq_type -> coq_type

val append_bit_pos : bool -> positive -> positive

val append_char_pos_default : char -> positive -> positive

val append_char_pos : char -> positive -> positive

val ident_of_string : char list -> ident

val decode_8_bits : (positive -> char list) -> positive -> char list

val string_of_ident : positive -> char list
