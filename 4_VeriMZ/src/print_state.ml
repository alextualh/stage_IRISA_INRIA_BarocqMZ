open Verimz.Pmp
open Verimz.Machine
open Verimz.Kernel
open Verimz.Asm
open Verimz.Oracle
open Printf

open Verimz.Barocqmz

let list_to_string f l =
  let str = List.fold_left (fun acc e -> acc ^ f e ^ "; ") "[" l in
  String.sub str 0 (String.length str - 2) ^ "]"

let int_list_to_string = list_to_string string_of_int

let pmp_access_to_string = function
  | MLoad (addr, s) -> sprintf "Load at %d of size %d" addr s
  | MStore (addr, lv) ->
      sprintf "Store values %s at %d" (int_list_to_string lv) addr
  | MFetch (addr, s) -> sprintf "Fetch at %d of size %d" addr s

let ecall_to_string = function
  | EcallExcptVect (n, addr) -> sprintf "EcallExcptVect (%d, %d)" n addr
  | EcallIntrpVect (n, addr) -> sprintf "EcallIntrpVect (%d, %d)" n addr
  | EcallCsrsMie -> "EcallCsrsMie"
  | EcallCsrcMie -> "EcallCsrcMie"

let op_code_to_string = function
  | OpUret -> "uret"
  | OpOther -> "unknown_op_code"

let exception_to_string = function
  | ExcLoadAccessFault a ->
      sprintf "LoadAccessFault coming from %s" (pmp_access_to_string a)
  | ExcStoreAccessFault a ->
      sprintf "StoreAccessFault coming from %s" (pmp_access_to_string a)
  | ExcInstrAccessFault a ->
      sprintf "InstrAccessfault comming from %s" (pmp_access_to_string a)
  | ExcEcall ec -> sprintf "Ecall %s" (ecall_to_string ec)
  | ExcIllegalInstr op -> sprintf "IllegalInstr %s" (op_code_to_string op)

let interrupt_ot_string = function
  | IntrpMachineExternal -> "MachineExternal"
  | IntrpMachineSoftware -> "MachineSoftware"
  | IntrpMachineTimer -> "MachineTimer"

let event_to_string = function
  | EvInterrupt intrp -> sprintf "Interrupt (%s)" (interrupt_ot_string intrp)
  | EvMemoryAccess pmpa ->
      sprintf "MemoryAccess (%s)" (pmp_access_to_string pmpa)
  | EvException exc -> sprintf "Exception (%s)" (exception_to_string exc)
  | EvRestoreZone n -> sprintf "RestoreZone %d" n
  | EvNone -> "\u{2205}"

let kernel_to_string k =
  sprintf
    "current zone: %d, deadline: %d, next zone: %d, next quantum: %d"
    k.br_interface_kernel_current
    k.br_interface_kernel_deadline
    k.br_interface_kernel_deadline
    k.br_interface_kernel_next_quantum

let machine_to_string mc =
  sprintf "mtime: %d, mtimecmp: %d, mie: 0x%x" mc.mtime mc.mtimecmp mc.mie

let kernel_state_to_string ks =
  sprintf "(%s, %s)" (kernel_to_string ks) (machine_to_string ks.br_interface_kernel_ms)

let regset_to_string (rs : int list) : string =
  Printf.sprintf
    "PC: %d, X1: %d, X2: %d, X3: %d"
    (reg_val rs PC)
    (reg_val rs (IR X1))
    (reg_val rs (IR X2))
    (reg_val rs (IR X3))

let kernel_entry_to_string = function
  | InitEntry -> "InitEntry"
  | TrapEntry (TrpException exc) ->
      sprintf "TrapEntry (TrpException %s)" (exception_to_string exc)
  | TrapEntry (TrpInterrupt intrp) ->
      sprintf "TrapEntry (TrpInterrupt %s)" (interrupt_ot_string intrp)

let state_to_string = function
  | KernelState (entry, ks, _, _) ->
      sprintf
        "## Kernel, entry by %s\n## %s"
        (kernel_entry_to_string entry)
        (kernel_state_to_string ks)
  | ZoneState (n, rs, ks, _, _) ->
      sprintf
        "## Zone %d, (Regs %s),\n## %s"
        n
        (regset_to_string rs)
        (kernel_state_to_string ks)

let zone_event_to_string = function
  | ZevInterrupt (iev, _) ->
      let iev_str =
        begin
          match iev with
          | IevMachineExternal -> "MachineExternal"
          | IevMachineSoftware -> "MachineSoftware"
        end
      in
      Printf.sprintf "ZevInterrupt %s" iev_str
  | ZevTimerUpdate _ -> "ZevTimerUpdate"

let zevents_to_string = list_to_string zone_event_to_string
