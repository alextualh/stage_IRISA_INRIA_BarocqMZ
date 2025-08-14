open Archi
open BinInt
open Integers
open List0

val bytes_of_int : int -> Stdlib.Int.t -> Byte.int list

val int_of_bytes : Byte.int list -> Stdlib.Int.t

val rev_if_be : Byte.int list -> Byte.int list

val encode_int : int -> Stdlib.Int.t -> Byte.int list

val decode_int : Byte.int list -> Stdlib.Int.t
