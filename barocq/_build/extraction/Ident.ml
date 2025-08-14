open BinNums
open Ctypesdefs
open Datatypes
open DecimalString
open Nat
open String0

type t = char list

(** val of_string : char list -> t **)

let of_string str =
  str

(** val to_string : t -> char list **)

let to_string i =
  i

(** val of_str_nat : nat -> t **)

let of_str_nat n =
  let s = NilEmpty.string_of_uint (to_uint n) in of_string s

(** val of_pos : positive -> t **)

let of_pos =
  string_of_ident

(** val to_pos : t -> positive **)

let to_pos =
  ident_of_string

(** val concat : t -> t -> t **)

let concat i1 i2 =
  let s1 = to_string i1 in let s2 = to_string i2 in of_string (append s1 s2)

(** val prefix_with : char list -> t -> t **)

let prefix_with str i =
  let s = to_string i in of_string (append str s)

(** val compare : t -> t -> comparison **)

let compare =
  compare

(** val eq_dec : char list -> char list -> bool **)

let eq_dec =
  string_dec
