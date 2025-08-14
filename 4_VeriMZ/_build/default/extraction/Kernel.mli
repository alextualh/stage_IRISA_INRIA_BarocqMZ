open Asm
open Barocqmz
open Barocqmzutils
open Barray
open Errors
open Int0
open Integers
open Machine
open Memory
open Monads
open Oracle
open Pmp
open RecordSet

val interrupt_code : interrupt -> Intptr.int

val mie_intrp : interrupt -> Intptr.int

val mie_machine_software : Intptr.int

val ms_as_machine : coq_Br_interface_kernel -> machine

type zone_descr = { zd_bpc : Intptr.int; zd_prog : program }

val save_context :
  coq_Br_interface_kernel -> Int.int array -> coq_Br_interface_kernel res

val restore_context : coq_Br_interface_zone -> Int.int array

type kernel_entry =
| InitEntry
| TrapEntry of trap

type state =
| KernelState of kernel_entry * coq_Br_interface_kernel * zone_descr list
   * mem
| ZoneState of Int.int * Int.int array * coq_Br_interface_kernel
   * zone_descr list * mem

type event =
| EvInterrupt of interrupt
| EvMemoryAccess of pmp_access
| EvException of coq_exception
| EvRestoreZone of Int.int
| EvNone

val int_to_Intptr : Int.int -> Intptr.int

val is_interrupt_enabled : coq_Br_interface_kernel -> interrupt -> bool

val get_current_zone : coq_Br_interface_kernel -> coq_Br_interface_zone res

val step : oracle -> state -> ((state * event) * oracle) res
