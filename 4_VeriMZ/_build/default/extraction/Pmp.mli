open Datatypes
open List0
open Memory
open Nat

type pmp_mode =
| OFF
| TOR
| NA4
| NAPOT of int

type permission =
| Readable
| Writable
| Executable

type pmp_register = { mode : pmp_mode; perm_read : bool; perm_write : 
                      bool; perm_exec : bool }

type pmp_entry = pmp_register * mem_addr

type pmp_config = pmp_entry list

type pmp_region = mem_addr * int

val pmp_entry_to_region : pmp_entry -> pmp_entry option -> pmp_region option

val pmp_permission_check : pmp_entry -> permission -> bool

val pmp_access_check_rec :
  pmp_config -> mem -> mem_addr -> int -> permission -> pmp_entry option ->
  bool

val pmp_access_check_b :
  pmp_config -> mem -> mem_addr -> int -> permission -> bool

type pmp_access =
| MLoad of mem_addr * int
| MStore of mem_addr * mem_val list
| MFetch of mem_addr * int

val pmp_load : pmp_config -> mem -> mem_addr -> int -> mem_val list option

val pmp_store : pmp_config -> mem -> mem_addr -> mem_val list -> mem option

val pmp_fetch : pmp_config -> mem -> mem_addr -> int -> mem_val list option

val get_bit : int -> int -> bool

val byte_of_word : int -> int -> int

val cfg_byte_at : int list -> int -> int

val count_trailing_ones_fuel : int -> int -> int

val count_trailing_ones : int -> int

val a_bits_of_byte : int -> int

val decode_mode_from : int -> int -> pmp_mode

val build_entries : int list -> int list -> int -> int -> pmp_entry list

val array_int_to_pmp_config : int list -> int list -> pmp_config
