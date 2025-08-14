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

(** val interrupt_code : interrupt -> Intptr.int **)

let interrupt_code intrp =
  let code_n =
    match intrp with
    | IntrpMachineExternal ->
      Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))
    | IntrpMachineSoftware ->
      Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))
    | IntrpMachineTimer ->
      Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))
  in
  Intptr.of_nat code_n

(** val mie_intrp : interrupt -> Intptr.int **)

let mie_intrp intrp =
  Intptr.shl Intptr.one (interrupt_code intrp)

(** val mie_machine_software : Intptr.int **)

let mie_machine_software =
  mie_intrp IntrpMachineSoftware

(** val ms_as_machine : coq_Br_interface_kernel -> machine **)

let ms_as_machine ks =
  ks.br_interface_kernel_ms

type zone_descr = { zd_bpc : Intptr.int; zd_prog : program }

(** val save_context :
    coq_Br_interface_kernel -> Int.int array -> coq_Br_interface_kernel res **)

let save_context ks regs =
  MonError.bind
    (get ks.br_interface_kernel_zones
      (int_to_int64 ks.br_interface_kernel_current)) (fun current_zone ->
    let current_zone0 =
      set (fun b -> b.br_interface_zone_regs) (fun f ->
        let a = fun r -> f r.br_interface_zone_regs in
        (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
        br_interface_zone_regs = (a x); br_interface_zone_pmpcfg =
        x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
        x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
        x.br_interface_zone_ustatus; br_interface_zone_uepc =
        x.br_interface_zone_uepc; br_interface_zone_uie =
        x.br_interface_zone_uie; br_interface_zone_ucause =
        x.br_interface_zone_ucause; br_interface_zone_utval =
        x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
        x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
        x.br_interface_zone_intrp_deleg })) (fun _ -> regs) current_zone
    in
    MonError.bind
      (Barray.set ks.br_interface_kernel_zones
        (int_to_int64 ks.br_interface_kernel_current) current_zone0)
      (fun zones ->
      let ks1 =
        set (fun b -> b.br_interface_kernel_zones) (fun f ->
          let a = fun r -> f r.br_interface_kernel_zones in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
          br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ -> zones) ks
      in
      MonError.ret ks1))

(** val restore_context : coq_Br_interface_zone -> Int.int array **)

let restore_context z =
  z.br_interface_zone_regs

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

(** val int_to_Intptr : Int.int -> Intptr.int **)

let int_to_Intptr x =
  Intptr.repr (Int.unsigned x)

(** val is_interrupt_enabled :
    coq_Br_interface_kernel -> interrupt -> bool **)

let is_interrupt_enabled ks intrp =
  Intptr.eq
    (Intptr.shru
      (Intptr.coq_and (int_to_Intptr ks.br_interface_kernel_mie)
        (mie_intrp intrp)) (interrupt_code intrp)) Intptr.one

(** val get_current_zone :
    coq_Br_interface_kernel -> coq_Br_interface_zone res **)

let get_current_zone ks =
  MonError.bind
    (get ks.br_interface_kernel_zones
      (int_to_int64 ks.br_interface_kernel_current)) MonError.ret

(** val step : oracle -> state -> ((state * event) * oracle) res **)

let step orc = function
| KernelState (entry, ks, ldscr, m) ->
  MonError.bind2 (call_koracle orc) (fun _ orc' ->
    bind
      (match entry with
       | InitEntry -> coq_Br_sched_sched_init ks
       | TrapEntry tr ->
         let (mcause, mtval) = trap_to_mcause tr in
         coq_Br_trap_handle_trap mcause mtval ks) (fun ks' ->
      bind (get_current_zone ks') (fun z ->
        let zid = z.br_interface_zone_id in
        let regs = restore_context z in
        let st' = ZoneState (zid, regs, ks', ldscr, m) in
        OK ((st', (EvRestoreZone zid)), orc'))))
| ZoneState (zid, regs, ks, ldscr, m) ->
  bind2 (call_zoracle orc zid) (fun zev orc' ->
    let mtime = read_mtime (ms_as_machine ks) in
    let (ointrp, tu) =
      match zev with
      | ZevTimerUpdate tu ->
        if (&&) (Int64.cmpu Cle (read_mtimecmp (ms_as_machine ks)) mtime)
             (is_interrupt_enabled ks IntrpMachineTimer)
        then ((Some IntrpMachineTimer), tu)
        else (None, tu)
      | ZevInterrupt (iev, tu) ->
        let intrp =
          match iev with
          | IevMachineExternal -> IntrpMachineExternal
          | IevMachineSoftware -> IntrpMachineSoftware
        in
        if is_interrupt_enabled ks intrp
        then ((Some intrp), tu)
        else if (&&)
                  (Int64.cmpu Cle (read_mtimecmp (ms_as_machine ks)) mtime)
                  (is_interrupt_enabled ks IntrpMachineTimer)
             then ((Some IntrpMachineTimer), tu)
             else (None, tu)
    in
    let ms' = write_mtime (ms_as_machine ks) (Int64.add mtime tu) in
    let ks0 =
      set (fun b -> b.br_interface_kernel_ms) (fun f ->
        let b = fun r -> f r.br_interface_kernel_ms in
        (fun x -> { br_interface_kernel_current =
        x.br_interface_kernel_current; br_interface_kernel_deadline =
        x.br_interface_kernel_deadline; br_interface_kernel_next =
        x.br_interface_kernel_next; br_interface_kernel_next_quantum =
        x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
        x.br_interface_kernel_mie; br_interface_kernel_zones =
        x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
        x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
        x.br_interface_kernel_inboxes; br_interface_kernel_ms = (b x) }))
        (fun _ -> ms') ks
    in
    (match ointrp with
     | Some intrp ->
       let ev = EvInterrupt intrp in
       let entry = TrapEntry (TrpInterrupt intrp) in
       bind (save_context ks0 regs) (fun ks' ->
         let st' = KernelState (entry, ks', ldscr, m) in OK ((st', ev), orc'))
     | None ->
       bind (get_current_zone ks0) (fun z ->
         bind (get ldscr (int_to_int64 zid)) (fun zdscr ->
           bind (get z.br_interface_zone_regs (code_preg PC))
             (fun resg_index_PC ->
             let rpc =
               Intptr.divu
                 (Intptr.sub (int_to_Intptr resg_index_PC) zdscr.zd_bpc)
                 (Intptr.of_nat instr_size)
             in
             let prog = zdscr.zd_prog in
             bind (get prog.prog_code (coq_Intptr_to_int64 rpc))
               (fun instr ->
               match pmp_exec_instr instr regs prog.prog_genv m
                       z.br_interface_zone_pmpcfg z.br_interface_zone_pmpaddr with
               | Next (regs', m', opmpa) ->
                 let ev =
                   match opmpa with
                   | Some pmpa -> EvMemoryAccess pmpa
                   | None -> EvNone
                 in
                 let st' = ZoneState (zid, regs', ks0, ldscr, m') in
                 OK ((st', ev), orc')
               | Exception exc ->
                 let ev = EvException exc in
                 let entry = TrapEntry (TrpException exc) in
                 bind (save_context ks0 regs) (fun ks' ->
                   let st' = KernelState (entry, ks', ldscr, m) in
                   OK ((st', ev), orc'))
               | Stuck -> Error []))))))
