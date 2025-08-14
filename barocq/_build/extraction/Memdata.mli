open AST
open Archi
open BinInt
open BinNums
open Datatypes
open Integers
open List0
open Values

val size_chunk : memory_chunk -> coq_Z

type quantity =
| Q32
| Q64

type memval =
| Undef
| Byte of Byte.int
| Fragment of coq_val * quantity * nat

val bytes_of_int : nat -> coq_Z -> Byte.int list

val int_of_bytes : Byte.int list -> coq_Z

val rev_if_be : Byte.int list -> Byte.int list

val encode_int : nat -> coq_Z -> Byte.int list

val decode_int : Byte.int list -> coq_Z
