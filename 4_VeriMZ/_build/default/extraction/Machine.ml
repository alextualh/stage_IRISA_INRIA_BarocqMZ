open Int0
open Integers

type machine = { mtime : Int64.int; mtimecmp : Int64.int; mie : Intptr.int }

(** val read_mtime : machine -> Int64.int **)

let read_mtime m =
  m.mtime

(** val read_mtimecmp : machine -> Int64.int **)

let read_mtimecmp m =
  m.mtimecmp

(** val write_mtime : machine -> Int64.int -> machine **)

let write_mtime mc v =
  { mtime = v; mtimecmp = mc.mtimecmp; mie = mc.mie }

(** val write_mtimecmp : machine -> Int64.int -> machine **)

let write_mtimecmp mc v =
  { mtime = mc.mtime; mtimecmp = v; mie = mc.mie }

(** val write_mie : machine -> Intptr.int -> machine **)

let write_mie mc v =
  { mtime = mc.mtime; mtimecmp = mc.mtimecmp; mie = v }
