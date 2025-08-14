From Verimz Require Import Int Machine Memory Pmp Oracle Asm Barocqmz Barocqmzutils.
(* Asm *)
From Coq Require Import Bool List BinIntDef.
From compcert Require Import Integers.
From RecordUpdate Require Import RecordUpdate.
From BarocqComp Require Import Error Barray Intop.
Import BoolNotations ListNotations. 

(* 'Utils' : conflict notation 'do' *)
Require Import ZArith Nat Bool.

(* test import caml capital letter first or not ? *)
Record truc_bidule_kernel := mk_truc_bidule {
  truc_bidule_kernel_dummy: int
}.


(** * Kernel implementation and meta-semantics. *)
  (* Inductive interrupt :=
    | IntrpMachineExternal : interrupt
    | IntrpMachineSoftware : interrupt
    | IntrpMachineTimer : interrupt. *)

Definition interrupt_code (intrp: interrupt) : Intptr.int :=
  let code_n : nat :=
    match intrp with
    | IntrpMachineSoftware => 3%nat
    | IntrpMachineTimer => 7%nat
    | IntrpMachineExternal => 11%nat
    end
  in Intptr.of_nat code_n.

  Definition mie_intrp (intrp: interrupt) := Intptr.shl Intptr.one (interrupt_code intrp).

  Definition mie_machine_timer := mie_intrp IntrpMachineTimer.

  Definition mie_machine_software := mie_intrp IntrpMachineSoftware.
  
  Definition mie_machine_external := mie_intrp IntrpMachineExternal.

(* Parameter QUANTUM : int64. *)
(* generated from barocq code but name changed, so changed in extraction.V *)


(* ============================================================= shallow generated ROCQ from Barocq code of BarocqMZ =============================================== *)
(* in another file... *)
(*_________ END of AUTOMATICALLY GENERATED ROCQ from barocq code of barocqMZ with -gen-shallow _________ *)


(* take in rocq generated but have to be modified for the meta semantic *)

(* No need cuz finnaly i've modified barocqmz file directly 
(* Definition Br_interface_state := machine. *)
Axiom Br_interface_state_eq : Barocqmz.Br_interface_state = machine.

Definition ms_as_machine (ks : Br_interface_kernel) : machine :=
  eq_rect _ (fun T => T) (br_interface_kernel_ms ks)
          _ Br_interface_state_eq.
  (* ks.(br_interface_kernel_ms) replaced by (ms_as_machine ks) to access ms field and CAST as machine *)
  (* conversion machine -> Barocqmz.Br_interface_state *)
(* Definition machine_as_Br_interface_state (m : machine) : Barocqmz.Br_interface_state :=
  eq_rect Type machine (fun T => T) m Barocqmz.Br_interface_state Br_interface_state_eq.
 *)
(* machine -> Br_interface_state *)
Definition machine_as_ms (machine_ins : machine) : Br_interface_state :=
  eq_rect _ (fun T => T) machine_ins
          _ (eq_sym Br_interface_state_eq). *)

  Definition ms_as_machine (ks : Br_interface_kernel) : machine :=
    (br_interface_kernel_ms ks).

(* ======== Kernel meta-semantics ======= *)

Section Semantics.

  Open Scope asm.

  (** [zone_descr] records the static information about a zone (base pc and program). *)
  Record zone_descr := mk_zone_descr {
    zd_bpc: intptr;
    zd_prog: program;
  }.

  (* Locate res.
  Print res.  *)

  Print Br_interface_kernel.

  Check Barray.get.
(* return dans la monade ie ret : A -> M A génère res de A depuis A*)


(** [save_context ks regs] backups the registers [regs] of the current zone being executed. *)
Definition save_context (ks: Br_interface_kernel) (regs: array int) : res Br_interface_kernel :=
    (* unwrap : *)
    let* current_zone := Barray.get ks.(br_interface_kernel_zones) (int_to_int64 ks.(br_interface_kernel_current)) in
    (* modif : *)
    let current_zone := current_zone <| br_interface_zone_regs := regs |> in 
    (* rewrap *)
    let* zones := Barray.set ks.(br_interface_kernel_zones) (int_to_int64 ks.(br_interface_kernel_current)) current_zone in
    let ks1 := ks <| br_interface_kernel_zones := zones |> in 
    ret ks1.



    (** [restore_context z] restores the registers of the zone [z] to be executed. *)
  Definition restore_context (z: Br_interface_zone) : array int := br_interface_zone_regs z.



(* ALL this needed for : 'state' enum *)
  (* from original Kernel.v, pasted here, see for other option *)
    (* already defind before in this file : see begining for mie_machine_ ... *)
  (* Inductive interrupt :=
    | IntrpMachineExternal : interrupt
    | IntrpMachineSoftware : interrupt
    | IntrpMachineTimer : interrupt. *)
      (** Software exceptions are raised by an unauthorized pmp access or by the "ecall" and "uret" instructions. *)
    (* Inductive exception :=
    | ExcInstrAccessFault : pmp_access -> exception
    | ExcLoadAccessFault : pmp_access -> exception
    | ExcStoreAccessFault : pmp_access -> exception
    | ExcEcall : ecall -> exception
    | ExcIllegalInstr : op_code -> exception. *)

  (* KERNEL_ENTRY *)
    Inductive kernel_entry :=
    | InitEntry : kernel_entry
    | TrapEntry : trap -> kernel_entry.
(* ALL this needed for :  *)
  (** The kernel meta-semantics is a state machine that alternates between kernel execution and zone execution. *)
  Inductive state :=
    | KernelState : kernel_entry -> Br_interface_kernel -> list zone_descr -> mem -> state
    | ZoneState : int -> array int -> Br_interface_kernel -> list zone_descr -> mem -> state.

  
  
    (** The possible events that will form an execution trace. *)
  Inductive event :=
    | EvInterrupt : interrupt -> event
    | EvMemoryAccess : pmp_access -> event
    | EvException : exception -> event
    | EvRestoreZone : int -> event
    | EvNone : event.

    (* needed for is_intrp_enabled : *)
  (* Definition interrupt_code (intrp: interrupt) : intptr :=
    let code_n := match intrp with
    | IntrpMachineSoftware => 3
    | IntrpMachineTimer => 7
    | IntrpMachineExternal => 11
    end in Intptr.of_nat code_n.
    Definition mie_intrp (intrp: interrupt) := Intptr.shl Intptr.one (interrupt_code intrp). *)

  (* Usefull to CONVERT kernel field int to Intptr (used many times in original veriMZ) *)
  Definition int_to_Intptr(x : int) : Intptr.int :=
    Intptr.repr (Int.unsigned x).
  
  (* is machine intrp enable at true = 1 (mie) ? *)
  Definition is_interrupt_enabled (ks: Br_interface_kernel) (intrp: interrupt) : bool :=
    Intptr.eq (
      (* 'shift right unsigned' de 'intrp_code intrp' bits*)
      Intptr.shru   
        (* ET binaire entre mie et masque pour prendre code*)
        (Intptr.and (int_to_Intptr (ks.(br_interface_kernel_mie))) (mie_intrp intrp))
        (interrupt_code intrp)
      ) 
    Intptr.one. (* compare 'eq' entre clacul et 1 = 'intptr.one' *)

    (* Utils needed in step() below (and erased by barocqMZ generated rocq) *)
    Definition get_current_zone (ks: Br_interface_kernel) : res Br_interface_zone :=
      let* current_zone := Barray.get ks.(br_interface_kernel_zones) (int_to_int64 ks.(br_interface_kernel_current)) in
      ret current_zone.

    Print get. (* ok ! *)
    (* Definition get2 {A: Type} (l: list A) (n: int) : option A :=
      get l n. *)


  Definition Intptr_to_int (p : Intptr.int) : Int.int :=
  Int.repr (Intptr.unsigned p).


  (** [step orc st] makes a transition between two states. [orc] is the timer oracle.
      The possible transitions are :
      - If the kernel is currently executing, then the next state reached will be the execution of a zone.
        If the kernel state was reached by the raise of a trap (interrupt or exception), the corresponding
        handler function is executed before restoring a zone context.
        Exception handling can be delegated to the zone that raised the exception.
      - If a zone is currently executing, then
        - Either the zone continues to execute,
        - Or an interrupt or exception is triggered and control is returned to the kernel. *)

  (* ADD ALEX : *)
Definition code_ireg (r : ireg) : int :=
  match r with
  | X1 => Int.repr 1
  | X2 => Int.repr 2
  | X3 => Int.repr 3
  end.

(* ADD ALEX end *)
Print Br_trap_handle_uret.

(* Set Printing All. *)
Print Br_interface_kernel.
Print machine.
Print Br_interface_state.

Print trap_to_mcause.

(* Print trap_to_mcause ExcInstrAccessFault. *)

  Definition step (orc: oracle) (st: state) : res (state * event * oracle) :=
    match st with
    | KernelState entry ks ldscr m =>
        let mtime := (read_mtime (ms_as_machine ks)) in
        let* (tu, orc') := call_koracle orc in
        do ks' <- match entry with
        | InitEntry => Br_sched_sched_init ks
        | TrapEntry tr => let (mcause, mtval) := trap_to_mcause tr in
                          Br_trap_handle_trap mcause mtval ks
        end;
        do z <- get_current_zone ks';
        let zid := z.(br_interface_zone_id) in
        (* modif machine *)
        let ms' := write_mtime (ms_as_machine ks) (Int64.add mtime tu) in
        (* let ms' := machine_as_ms ms' in *)
        let ks := ks <| br_interface_kernel_ms := ms' |> in        
        (* fin modif machine *)
        let regs := restore_context z in
        let st' := ZoneState zid regs ks' ldscr m in
        OK (st', EvRestoreZone zid, orc')
  (*___ premier cas fini renvoie res et plus ________________
exception_code_of
    ___________________________________________ 2e cas : ____*)
    | ZoneState zid regs ks ldscr m =>
        do (zev, orc') <- call_zoracle orc zid;
        let mtime := (read_mtime (ms_as_machine ks)) in
        let (ointrp, tu) := match zev with
        | ZevInterrupt iev tu =>
            let intrp := match iev with
            | IevMachineExternal => IntrpMachineExternal
            | IevMachineSoftware => IntrpMachineSoftware
            end in
            if is_interrupt_enabled ks intrp then (Some intrp, tu)
            else if (Int64.cmpu Cle (read_mtimecmp (ms_as_machine ks)) mtime) && (is_interrupt_enabled ks IntrpMachineTimer) then
              (Some IntrpMachineTimer, tu)
            else (None, tu)
        | ZevTimerUpdate tu =>
            if (Int64.cmpu Cle (read_mtimecmp (ms_as_machine ks))  mtime) && (is_interrupt_enabled ks IntrpMachineTimer) then
              (Some IntrpMachineTimer, tu)
            else (None, tu)
        end in
        (* modif machine *)
        let ms' := write_mtime (ms_as_machine ks) (Int64.add mtime tu) in
        (* let ms' := machine_as_ms ms' in *)
        let ks := ks <| br_interface_kernel_ms := ms' |> in
        (* fin modif machine *)
        let ks' := ks in
        match ointrp with
        | Some intrp =>
            let ev := EvInterrupt intrp in
            let entry := TrapEntry (TrpInterrupt intrp) in
            do ks' <- save_context ks' regs;
            let st' := KernelState entry ks' ldscr m in
            OK (st', ev, orc')
        | None => 
            do z <- get_current_zone ks';
            do zdscr <- get ldscr (int_to_int64 zid);
            do resg_index_PC <- Barray.get z.(br_interface_zone_regs) (code_preg PC);
            let rpc := Intptr.divu (Intptr.sub (int_to_Intptr resg_index_PC) (zd_bpc zdscr)) (Intptr.of_nat instr_size) in
            let prog := zd_prog zdscr in
            do instr <- get (prog_code prog) (Intptr_to_int64 rpc);
            match (pmp_exec_instr instr regs (prog_genv prog) m (br_interface_zone_pmpcfg z) (br_interface_zone_pmpaddr z)) with
            | Next regs' m' opmpa =>
                let ev := match opmpa with
                | Some pmpa => EvMemoryAccess pmpa
                | None => EvNone
                end in
                let st' := ZoneState zid regs' ks' ldscr m' in
                OK (st', ev, orc')
            | Exception exc =>
                let ev := EvException exc in
                let entry := TrapEntry (TrpException exc) in
                do ks' <- save_context ks' regs;
                let st' := KernelState entry ks' ldscr m in
                OK (st', ev, orc')
            | Stuck => Error nil
            end
        end
    end.

End Semantics.
  
(* MODIF : 
READ : 
1.  regr_mtime ks
    (read_mtime ks.(br_interface_kernel_ms))

2.  (read_mtimecmp ks.(br_interface_kernel_ms))
    (regr_mtimecmp ks)
WRITE : 
3.  regw_mtime ks' (Int64.add mtime tu)
    write_mtime ks.(br_interface_kernel_ms) (Int64.add mtime tu)

4.  ...
get2 replaced by get simple version

5. sur ça :
Definition step (orc: oracle) (st: state) : option (state * event * oracle) :=
  match st with
    | KernelState entry ks ldscr m =>
      let mtime := (read_mtime ks.(br_interface_kernel_ms)) in
      do (tu, orc') <- call_koracle orc;
      do ks' <- match entry with
--> The term "call_koracle orc" has type "option (timer_event * oracle)"
while it is expected to have type "res (?A * ?B)".
alors je change signature call_koracle en retournant res et pas option ? PAREIL ?
  
6. modif signature de pmp_exec_instr pour prendre array int et pas regset
*)


(* before it was : 
From Verimz Require Import Int Utils Machine Memory Pmp Asm Oracle.
Require Import ZArith Nat Bool.
Now it's without UTILS, CONFLICT : 
same notation defined in Error.v here but without ( )
  (* VS compecert-ce commons/Errors.v : 
    Notation "'do' ( X , Y ) <- A ; B" := (bind2 A (fun X Y => B))
    (at level 200, X name, Y name, A at level 100, B at level 200)
    : error_monad_scope.
  *)
*)