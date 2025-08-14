open BinNums
open Ctypesdefs
open Datatypes
open DecimalString
open Nat
open String0

type t = char list

val of_string : char list -> t

val to_string : t -> char list

val of_str_nat : nat -> t

val of_pos : positive -> t

val to_pos : t -> positive

val concat : t -> t -> t

val prefix_with : char list -> t -> t

val compare : t -> t -> comparison

val eq_dec : char list -> char list -> bool
