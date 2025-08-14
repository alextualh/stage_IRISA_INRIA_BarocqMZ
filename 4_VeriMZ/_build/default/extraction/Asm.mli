open Barocqmzutils
open Barray
open BinInt
open Errors
open Int0
open Integers
open List0
open Maps
open Memdata
open Memory
open Pmp

type op_code =
| OpUret
| OpOther

type interrupt =
| IntrpMachineExternal
| IntrpMachineSoftware
| IntrpMachineTimer

type ecall =
| EcallExcptVect of Intptr.int * Intptr.int
| EcallIntrpVect of Intptr.int * Intptr.int
| EcallCsrsMie
| EcallCsrcMie

type coq_exception =
| ExcInstrAccessFault of pmp_access
| ExcLoadAccessFault of pmp_access
| ExcStoreAccessFault of pmp_access
| ExcEcall of ecall
| ExcIllegalInstr of op_code

type trap =
| TrpInterrupt of interrupt
| TrpException of coq_exception

val interrupt_bit_shift : Stdlib.Int.t

val interrupt_bit_mask : Stdlib.Int.t

val interrupt_code_of : interrupt -> int

val mk_mcause_for_interrupt : interrupt -> Int.int

val mtval_of_interrupt : interrupt -> Int.int

val exception_code : coq_exception -> Intptr.int

val mem_addr_to_int : int -> Int.int

val op_code_to_int_default : op_code -> Int.int

val mtval_of_exception : coq_exception -> Int.int

val mk_mcause_for_exception : coq_exception -> Int.int

val trap_to_mcause : trap -> Int.int * Int.int

type ireg =
| X1
| X2
| X3

type preg =
| PC
| IR of ireg

type label = int

type genv = Intptr.int PTree.t

type instruction =
| Iadd of ireg * ireg * ireg
| Isub of ireg * ireg * ireg
| Ili of ireg * Intptr.int
| Ilb of ireg * ireg * Intptr.int
| Isb of ireg * ireg * Intptr.int
| Ilabel of label
| Ijump of label
| Iecall of ecall
| Iuret

type code = instruction list

type program = { prog_code : code; prog_genv : genv }

type outcome =
| Next of Int.int array * mem * pmp_access option
| Exception of coq_exception
| Stuck

val instr_size : int

val addr_of_intptr : Intptr.int -> int

val code_ireg : ireg -> Int.int

val code_preg : preg -> Int64.int

val get_reg_in_set : Int.int array -> preg -> Intptr.int

val set_reg_in_set : Int.int array -> preg -> Int.int -> Int.int array

val nextinstr : Int.int array -> Int.int array

val array_int_to_list_nat : Int.int array -> int list

val pmp_exec_instr :
  instruction -> Int.int array -> genv -> mem -> Int.int array -> Int.int
  array -> outcome
