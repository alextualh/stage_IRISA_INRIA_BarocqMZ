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

(** val interrupt_bit_shift : Stdlib.Int.t **)

let interrupt_bit_shift =
  ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) 1))))

(** val interrupt_bit_mask : Stdlib.Int.t **)

let interrupt_bit_mask =
  Z.shiftl 1 interrupt_bit_shift

(** val interrupt_code_of : interrupt -> int **)

let interrupt_code_of = function
| IntrpMachineExternal ->
  Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))
| IntrpMachineSoftware ->
  Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))
| IntrpMachineTimer ->
  Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))

(** val mk_mcause_for_interrupt : interrupt -> Int.int **)

let mk_mcause_for_interrupt i =
  Int.repr (Z.add interrupt_bit_mask (Z.of_nat (interrupt_code_of i)))

(** val mtval_of_interrupt : interrupt -> Int.int **)

let mtval_of_interrupt _ =
  Int.zero

(** val exception_code : coq_exception -> Intptr.int **)

let exception_code exc =
  let code_n =
    match exc with
    | ExcInstrAccessFault _ -> 1
    | ExcLoadAccessFault _ -> ((fun p->1+2*p) ((fun p->2*p) 1))
    | ExcStoreAccessFault _ -> ((fun p->1+2*p) ((fun p->1+2*p) 1))
    | ExcEcall _ -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
    | ExcIllegalInstr _ -> ((fun p->2*p) 1)
  in
  Intptr.repr code_n

(** val mem_addr_to_int : int -> Int.int **)

let mem_addr_to_int a =
  Int.repr (Z.of_nat a)

(** val op_code_to_int_default : op_code -> Int.int **)

let op_code_to_int_default _ =
  Int.zero

(** val mtval_of_exception : coq_exception -> Int.int **)

let mtval_of_exception = function
| ExcInstrAccessFault pmpa ->
  (match pmpa with
   | MLoad (addr, _) -> mem_addr_to_int addr
   | MStore (addr, _) -> mem_addr_to_int addr
   | MFetch (addr, _) -> mem_addr_to_int addr)
| ExcLoadAccessFault pmpa ->
  (match pmpa with
   | MLoad (addr, _) -> mem_addr_to_int addr
   | MStore (addr, _) -> mem_addr_to_int addr
   | MFetch (addr, _) -> mem_addr_to_int addr)
| ExcStoreAccessFault pmpa ->
  (match pmpa with
   | MLoad (addr, _) -> mem_addr_to_int addr
   | MStore (addr, _) -> mem_addr_to_int addr
   | MFetch (addr, _) -> mem_addr_to_int addr)
| ExcEcall _ -> Int.zero
| ExcIllegalInstr _opc -> op_code_to_int_default _opc

(** val mk_mcause_for_exception : coq_exception -> Int.int **)

let mk_mcause_for_exception e =
  coq_Intptr_to_int (exception_code e)

(** val trap_to_mcause : trap -> Int.int * Int.int **)

let trap_to_mcause = function
| TrpInterrupt i -> ((mk_mcause_for_interrupt i), (mtval_of_interrupt i))
| TrpException e -> ((mk_mcause_for_exception e), (mtval_of_exception e))

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

(** val instr_size : int **)

let instr_size =
  Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))

(** val addr_of_intptr : Intptr.int -> int **)

let addr_of_intptr =
  Intptr.unsigned_to_nat

(** val code_ireg : ireg -> Int.int **)

let code_ireg = function
| X1 -> Int.repr 1
| X2 -> Int.repr ((fun p->2*p) 1)
| X3 -> Int.repr ((fun p->1+2*p) 1)

(** val code_preg : preg -> Int64.int **)

let code_preg = function
| PC -> Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) 1))
| IR r -> Int64.repr (Int.signed (code_ireg r))

(** val get_reg_in_set : Int.int array -> preg -> Intptr.int **)

let get_reg_in_set rs _ =
  match get rs (code_preg PC) with
  | OK val_index -> int_to_Intptr val_index
  | Error _ -> Intptr.zero

(** val set_reg_in_set : Int.int array -> preg -> Int.int -> Int.int array **)

let set_reg_in_set rs _ new_val =
  match set rs (code_preg PC) new_val with
  | OK reg_set -> reg_set
  | Error _ -> rs

(** val nextinstr : Int.int array -> Int.int array **)

let nextinstr rs =
  set_reg_in_set rs PC
    (coq_Intptr_to_int
      (Intptr.add (get_reg_in_set rs PC) (Intptr.of_nat instr_size)))

(** val array_int_to_list_nat : Int.int array -> int list **)

let array_int_to_list_nat a =
  map (fun x -> Z.to_nat (Int.unsigned x)) a

(** val pmp_exec_instr :
    instruction -> Int.int array -> genv -> mem -> Int.int array -> Int.int
    array -> outcome **)

let pmp_exec_instr i rs ge m pmpcfg_array pmpaddr_array =
  let pmpcfg =
    array_int_to_pmp_config (array_int_to_list_nat pmpcfg_array)
      (array_int_to_list_nat pmpaddr_array)
  in
  (match pmp_fetch pmpcfg m (addr_of_intptr (get_reg_in_set rs PC)) instr_size with
   | Some _ ->
     (match i with
      | Iadd (rd, r1, r2) ->
        let v =
          Intptr.add (get_reg_in_set rs (IR r1)) (get_reg_in_set rs (IR r2))
        in
        Next ((nextinstr (set_reg_in_set rs (IR rd) (coq_Intptr_to_int v))),
        m, None)
      | Isub (rd, r1, r2) ->
        let v =
          Intptr.sub (get_reg_in_set rs (IR r1)) (get_reg_in_set rs (IR r2))
        in
        Next ((nextinstr (set_reg_in_set rs (IR rd) (coq_Intptr_to_int v))),
        m, None)
      | Ili (rd, v) ->
        Next ((nextinstr (set_reg_in_set rs (IR rd) (coq_Intptr_to_int v))),
          m, None)
      | Ilb (rd, r1, ofs) ->
        let addr = addr_of_intptr (Intptr.add (get_reg_in_set rs (IR r1)) ofs)
        in
        let pmpa = MLoad (addr, (Stdlib.Int.succ 0)) in
        (match pmp_load pmpcfg m addr (Stdlib.Int.succ 0) with
         | Some lv ->
           let v =
             Intptr.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
               (Intptr.repr (decode_int lv))
           in
           Next
           ((nextinstr (set_reg_in_set rs (IR rd) (coq_Intptr_to_int v))), m,
           (Some pmpa))
         | None -> Exception (ExcLoadAccessFault pmpa))
      | Isb (r1, r2, ofs) ->
        let addr = addr_of_intptr (Intptr.add (get_reg_in_set rs (IR r2)) ofs)
        in
        let lv =
          encode_int (Stdlib.Int.succ 0)
            (Intptr.unsigned (get_reg_in_set rs (IR r1)))
        in
        let pmpa = MStore (addr, lv) in
        (match pmp_store pmpcfg m addr lv with
         | Some m' -> Next ((nextinstr rs), m', (Some pmpa))
         | None -> Exception (ExcStoreAccessFault pmpa))
      | Ilabel _ -> Next ((nextinstr rs), m, None)
      | Ijump lbl ->
        (match PTree.get lbl ge with
         | Some addr ->
           Next ((set_reg_in_set rs PC (coq_Intptr_to_int addr)), m, None)
         | None -> Stuck)
      | Iecall c -> Exception (ExcEcall c)
      | Iuret -> Exception (ExcIllegalInstr OpUret))
   | None -> Stuck)
