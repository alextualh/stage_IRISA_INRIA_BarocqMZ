open Barocqmzutils
open Barray
open Errors
open Integers
open Intop
open Machine
open Monads
open RecordSet

type coq_Br_interface_inbox = { br_interface_inbox_full : Int.int;
                                br_interface_inbox_msgs : Int.int array }

type coq_Br_interface_zone = { br_interface_zone_id : Int.int;
                               br_interface_zone_regs : Int.int array;
                               br_interface_zone_pmpcfg : Int.int array;
                               br_interface_zone_pmpaddr : Int.int array;
                               br_interface_zone_ustatus : Int.int;
                               br_interface_zone_uepc : Int.int;
                               br_interface_zone_uie : Int.int;
                               br_interface_zone_ucause : Int.int;
                               br_interface_zone_utval : Int.int;
                               br_interface_zone_excpt_handlers : Int.int
                                                                  array;
                               br_interface_zone_intrp_deleg : Int.int }

type coq_Br_interface_intrp_handler = { br_interface_intrp_handler_zone : 
                                        Int.int;
                                        br_interface_intrp_handler_handler : 
                                        Int.int }

type coq_Br_interface_state = machine

type coq_Br_interface_kernel = { br_interface_kernel_current : Int.int;
                                 br_interface_kernel_deadline : Int64.int;
                                 br_interface_kernel_next : Int.int;
                                 br_interface_kernel_next_quantum : Int64.int;
                                 br_interface_kernel_mie : Int.int;
                                 br_interface_kernel_zones : coq_Br_interface_zone
                                                             array;
                                 br_interface_kernel_intrp_handlers : 
                                 coq_Br_interface_intrp_handler array;
                                 br_interface_kernel_inboxes : coq_Br_interface_inbox
                                                               array;
                                 br_interface_kernel_ms : coq_Br_interface_state }

val coq_Br_interface_QUANTUM_CYC : Int64.int

val coq_Br_interface_N_ZONES : Int.int

val coq_Br_interface_read_mtime : machine -> Int64.int

val coq_Br_interface_write_mtimecmp : machine -> Int64.int -> machine

val coq_Br_interface_csr_write :
  coq_Br_interface_state -> Int.int -> Int.int -> coq_Br_interface_state res

val coq_Br_interface_MCAUSE_INTRP : Int.int -> Int.int res

val coq_Br_interface_binary_or : Int.int -> Int.int -> Int.int res

val coq_Br_interface_USTATUS_IE : Int.int

val coq_Br_interface_USTATUS_PIE : Int.int

val coq_Br_interface_MIE_TIMER : Int.int

val coq_Br_interface_mie : Int.int

val coq_Br_interface_PC : Int.int

val coq_Br_interface_and_with_bitwise : Int.int -> Int.int -> Int.int

val coq_Br_sched_sched_yield :
  coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_sched_sched_init :
  coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_sched_sched_intrp :
  coq_Br_interface_kernel -> Int.int -> coq_Br_interface_kernel res

val coq_Br_sched_sched :
  coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_N_ECALLS : Int.int

val coq_Br_ecall_ECL_EXCPT_VECT : Int.int

val coq_Br_ecall_ECL_INTRP_VECT : Int.int

val coq_Br_ecall_ECL_CSRS_MIE : Int.int

val coq_Br_ecall_ECL_CSRC_MIE : Int.int

val coq_Br_ecall_ECL_SEND : Int.int

val coq_Br_ecall_ECL_RECV : Int.int

val coq_Br_ecall_A0 : Int.int

val coq_Br_ecall_A1 : Int.int

val coq_Br_ecall_A2 : Int.int

val coq_Br_ecall_A3 : Int.int

val coq_Br_ecall_A4 : Int.int

val coq_Br_ecall_A7 : Int.int

val coq_Br_ecall_N_EXCEPTIONS : Int.int

val coq_Br_ecall_N_INTERRUPTS : Int.int

val coq_Br_ecall_ecall_excpt_vect :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_intrp_vect_step_2 :
  Int.int -> Int.int -> Int.int -> Int.int -> coq_Br_interface_kernel ->
  coq_Br_interface_kernel res

val coq_Br_ecall_ecall_intrp_vect :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_csrs_mie_CALLER :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_csrs_mie_KERNEL :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_csrc_mie_CALLER :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_csrc_mie_KERNEL :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_send :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_ecall_recv :
  Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_get_current_regA7 : coq_Br_interface_kernel -> Int.int res

val coq_Br_ecall_handle_ecall_wrapped :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_ecall_handle_ecall :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_EXCPT_ILLEGAL_INSTR : Int.int

val coq_Br_trap_EXCPT_ECALL_UMODE : Int.int

val coq_Br_trap_INTRP_MACHINE_TIMER : Int.int

val coq_Br_trap_OP_URET : Int.int

val coq_Br_trap_set_ks_mie_and_bitwise :
  coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_get_current_zone_handler :
  Int.int -> coq_Br_interface_kernel -> Int.int res

val coq_Br_trap_get_i_handler_ZONE :
  Int.int -> coq_Br_interface_kernel -> Int.int res

val coq_Br_trap_get_i_handler_HANDLER :
  Int.int -> coq_Br_interface_kernel -> Int.int res

val coq_Br_trap_wrapped_handle_user_intrp :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_handle_user_intrp :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_handle_user_excpt :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_handle_uret :
  coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_handle_illegal_instruction :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res

val coq_Br_trap_handle_trap :
  Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res
