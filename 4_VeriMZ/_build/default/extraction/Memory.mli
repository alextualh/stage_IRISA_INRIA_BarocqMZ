open Compare_dec
open Datatypes
open Error
open Integers
open Nat

type mem_val = Byte.int

type mem = mem_val list

type mem_addr = int

val valid_access_dec : mem -> mem_addr -> int -> bool

val read_aux : mem -> mem_addr -> int -> mem_val list option

val read : mem -> mem_addr -> int -> mem_val list option

val write_aux : mem -> mem_addr -> mem_val list -> mem option

val write : mem -> mem_addr -> mem_val list -> mem option

val access_overlap_dec : mem_addr -> int -> mem_addr -> int -> bool

val access_contained : mem_addr -> int -> mem_addr -> int -> bool
