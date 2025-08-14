open Int0
open Integers

type machine = { mtime : Int64.int; mtimecmp : Int64.int; mie : Intptr.int }

val read_mtime : machine -> Int64.int

val read_mtimecmp : machine -> Int64.int

val write_mtime : machine -> Int64.int -> machine

val write_mtimecmp : machine -> Int64.int -> machine

val write_mie : machine -> Intptr.int -> machine
