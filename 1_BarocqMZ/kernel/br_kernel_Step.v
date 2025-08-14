(* ======== Kernel meta-semantics ======= *)

Section Semantics.

  Open Scope asm.

  (** [zone_descr] records the static information about a zone (base pc and program). *)
  Record zone_descr := mk_zone_descr {
    zd_bpc: intptr;
    zd_prog: program;
  }.

  (** [save_context ks regs] backups the registers [regs] of the current zone being executed. *)
  Definition save_context (ks: kernel_state) (regs: regset) : option kernel_state :=
    do z <- get_current_zone ks;
    let z':= set_zone_regs z regs in
    set_zone ks (zn_id z') z'.

  (** [restore_context z] restores the registers of the zone [z] to be executed. *)
  Definition restore_context (z: zone) : regset := zn_regs z.

  (** The kernel meta-semantics is a state machine that alternates between kernel execution and zone execution. *)
  Inductive state :=
    | KernelState : kernel_entry -> kernel_state -> list zone_descr -> mem -> state
    | ZoneState : intptr -> regset -> kernel_state -> list zone_descr -> mem -> state.

  (** The possible events that will form an execution trace. *)
  Inductive event :=
    | EvInterrupt : interrupt -> event
    | EvMemoryAccess : pmp_access -> event
    | EvException : exception -> event
    | EvRestoreZone : intptr -> event
    | EvNone : event.

  Definition is_interrupt_enabled (ks: kernel_state) (intrp: interrupt) : bool :=
    Intptr.eq (Intptr.shru (Intptr.and (regr_mie ks) (mie_intrp intrp)) (interrupt_code intrp)) Intptr.one.
  
  (** [step orc st] makes a transition between two states. [orc] is the timer oracle.
      The possible transitions are :
      - If the kernel is currently executing, then the next state reached will be the execution of a zone.
        If the kernel state was reached by the raise of a trap (interrupt or exception), the corresponding
        handler function is executed before restoring a zone context.
        Exception handling can be delegated to the zone that raised the exception.
      - If a zone is currently executing, then
        - Either the zone continues to execute,
        - Or an interrupt or exception is triggered and control is returned to the kernel. *)
  Definition step (orc: oracle) (st: state) : option (state * event * oracle) :=
    match st with
    | KernelState entry ks ldscr m =>
        let mtime := regr_mtime ks in
        do tu, orc' <- call_koracle orc;
        do ks' <- match entry with
        | InitEntry => Some (sched_init ks)
        | TrapEntry tr => handle_trap ks tr
        end;
        do z <- get_current_zone ks';
        let zid := zn_id z in
        let ks' := regw_mtime ks' (Int64.add mtime tu) in
        let regs := restore_context z in
        let st' := ZoneState zid regs ks' ldscr m in
        Some (st', EvRestoreZone zid, orc')
    | ZoneState zid regs ks ldscr m =>
        do zev, orc' <- call_zoracle orc zid;
        let mtime := regr_mtime ks in
        let (ointrp, tu) := match zev with
        | ZevInterrupt iev tu =>
            let intrp := match iev with
            | IevMachineExternal => IntrpMachineExternal
            | IevMachineSoftware => IntrpMachineSoftware
            end in
            if is_interrupt_enabled ks intrp then (Some intrp, tu)
            else if (Int64.cmpu Cle (regr_mtimecmp ks) mtime) && (is_interrupt_enabled ks IntrpMachineTimer) then
              (Some IntrpMachineTimer, tu)
            else (None, tu)
        | ZevTimerUpdate tu =>
            if (Int64.cmpu Cle (regr_mtimecmp ks) mtime) && (is_interrupt_enabled ks IntrpMachineTimer) then
              (Some IntrpMachineTimer, tu)
            else (None, tu)
        end in
        let ks' := regw_mtime ks (Int64.add mtime tu) in
        match ointrp with
        | Some intrp =>
            let ev := EvInterrupt intrp in
            let entry := TrapEntry (TrpInterrupt intrp) in
            do ks' <- save_context ks' regs;
            let st' := KernelState entry ks' ldscr m in
            Some (st', ev, orc')
        | None => 
            do z <- get_current_zone ks';
            do zdscr <- get2 ldscr zid;
            let rpc := Intptr.divu (Intptr.sub regs#PC (zd_bpc zdscr)) (Intptr.of_nat instr_size) in
            let prog := zd_prog zdscr in
            do instr <- get2 (prog_code prog) rpc;
            match (pmp_exec_instr instr regs (prog_genv prog) m (zn_pmpcfg z)) with
            | Next regs' m' opmpa =>
                let ev := match opmpa with
                | Some pmpa => EvMemoryAccess pmpa
                | None => EvNone
                end in
                let st' := ZoneState zid regs' ks' ldscr m' in
                Some (st', ev, orc')
            | Exception exc =>
                let ev := EvException exc in
                let entry := TrapEntry (TrpException exc) in
                do ks' <- save_context ks' regs;
                let st' := KernelState entry ks' ldscr m in
                Some (st', ev, orc')
            | Stuck => None
            end
        end
    end.

End Semantics.
  