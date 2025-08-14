From Coq Require Import Bool List BinIntDef.
From compcert Require Import Integers.
From RecordUpdate Require Import RecordUpdate.
From BarocqComp Require Import Error Barray Intop.
Import BoolNotations ListNotations.

Open Scope error_monad_scope.

(* test import caml capital letter first or not ? *)
Record truc_bidule := mk_truc_bidule {
  truc_bidule_dummy: int
}.

Record Br_interface_inbox := mk_Br_interface_inbox {
  br_interface_inbox_full: int;
  br_interface_inbox_msgs: array int
}.

Record Br_interface_zone := mk_Br_interface_zone {
  br_interface_zone_id: int;
  br_interface_zone_regs: array int;
  br_interface_zone_pmpcfg: array int;
  br_interface_zone_pmpaddr: array int;
  br_interface_zone_ustatus: int;
  br_interface_zone_uepc: int;
  br_interface_zone_uie: int;
  br_interface_zone_ucause: int;
  br_interface_zone_utval: int;
  br_interface_zone_excpt_handlers: array int;
  br_interface_zone_intrp_deleg: int
}.

Record Br_interface_intrp_handler := mk_Br_interface_intrp_handler {
  br_interface_intrp_handler_zone: int;
  br_interface_intrp_handler_handler: int
}.

From Verimz Require Import Machine Barocqmzutils.
(* Parameter Br_interface_state : Type. *)
Definition Br_interface_state := machine.

Record Br_interface_kernel := mk_Br_interface_kernel {
  br_interface_kernel_current: int;
  br_interface_kernel_deadline: int64;
  br_interface_kernel_next: int;
  br_interface_kernel_next_quantum: int64;
  br_interface_kernel_mie: int;
  br_interface_kernel_zones: array Br_interface_zone;
  br_interface_kernel_intrp_handlers: array Br_interface_intrp_handler;
  br_interface_kernel_inboxes: array Br_interface_inbox;
  br_interface_kernel_ms: Br_interface_state
}.

Instance eta_Br_interface_inbox : Settable Br_interface_inbox :=
  settable! mk_Br_interface_inbox <br_interface_inbox_full; br_interface_inbox_msgs>.

Instance eta_Br_interface_zone : Settable Br_interface_zone :=
  settable! mk_Br_interface_zone <br_interface_zone_id; br_interface_zone_regs; br_interface_zone_pmpcfg; br_interface_zone_pmpaddr; br_interface_zone_ustatus; br_interface_zone_uepc; br_interface_zone_uie; br_interface_zone_ucause; br_interface_zone_utval; br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg>.

Instance eta_Br_interface_intrp_handler : Settable Br_interface_intrp_handler :=
  settable! mk_Br_interface_intrp_handler <br_interface_intrp_handler_zone; br_interface_intrp_handler_handler>.

Instance eta_Br_interface_kernel : Settable Br_interface_kernel :=
  settable! mk_Br_interface_kernel <br_interface_kernel_current; br_interface_kernel_deadline; br_interface_kernel_next; br_interface_kernel_next_quantum; br_interface_kernel_mie; br_interface_kernel_zones; br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes; br_interface_kernel_ms>.

(* =========================================================================================================  *)
(* Abstract definition, have to be changed from Parameter ... to Definition ... *)

Parameter Br_interface_QUANTUM_CYC : int64.

Parameter Br_interface_N_ZONES : int.

(* Parameter Br_interface_read_mtime : Br_interface_state -> res int64. *)

Definition Br_interface_read_mtime := read_mtime.


Print write_mtimecmp.

(* Parameter Br_interface_write_mtimecmp : Br_interface_state -> int64 -> res Br_interface_state. *)
Definition Br_interface_write_mtimecmp := write_mtimecmp.

(* Parameter Br_interface_csr_write_asm_trap_entry : Br_interface_state -> int -> res Br_interface_state. *)
Definition Br_interface_csr_write_asm_trap_entry (ms : Br_interface_state) (val: int) : res Br_interface_state :=
  ret (write_mie ms (int_to_Intptr val)).
(* bullshit definition, no sense in this model, no even used but define to compile *)


(* Parameter Br_interface_csr_write : Br_interface_state -> int -> int -> res Br_interface_state. *)
(* Definition regw_mie (ks: kernel_state) (v: intptr) : kernel_state :=
  mk_kernel_state (write_mie (ks_mach ks) v) (ks_ker ks). *)
Definition Br_interface_csr_write (ms : Br_interface_state) (reg: int) (val: int) : res Br_interface_state :=
  ret (write_mie ms (int_to_Intptr val)).

(* Parameter Br_interface_MCAUSE_INTRP : int -> res int. *)
Definition Br_interface_MCAUSE_INTRP (mcause : int) : res int :=
  OK (Int.shru (Int.shl mcause (Int.repr 1)) (Int.repr 1)).

(* Parameter Br_interface_binary_or : int -> int -> res int. *)
Definition Br_interface_binary_or (a : int) (b : int) : res int :=
  OK (Int.or a b).

Definition Br_interface_USTATUS_IE : int := Int.repr 1%Z.

Definition Br_interface_USTATUS_PIE : int := Int.repr 16%Z.

Definition Br_interface_MIE_TIMER : int := Int.repr 128%Z.

Definition Br_interface_mie : int := Int.repr 772%Z.

Definition Br_interface_mtvec : int := Int.repr 773%Z.

Definition Br_interface_mstatus : int := Int.repr 774%Z.

Definition Br_interface_PC : int := Int.repr 0%Z.

(* =========================================================================================================  *)




(* =========================================================================================================  *)
(* === TRUE CODE, generated automatically, not changed from barocqMZ source code === *)
(* =========================================================================================================  *)

Definition Br_interface_and_with_bitwise (p_mie: int) (p_uie: int) : int :=
  let u_bitwise := Int.not p_uie in
  let u_result := Int.and p_mie u_bitwise in
  u_result.

Definition Br_sched_sched_yield (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let u_current_id := p_ks.(br_interface_kernel_current) in
  let* u_current := Barray.get u_zones (U64.of_u32 u_current_id) in
  let* u_zones := Barray.set u_zones (U64.of_u32 u_current_id) u_current in
  let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
  let u_ks := u_ks <| br_interface_kernel_current := u_ks.(br_interface_kernel_next) |> in
  let u_ks := u_ks <| br_interface_kernel_deadline := Int64.add (u_ks.(br_interface_kernel_deadline)) (u_ks.(br_interface_kernel_next_quantum)) |> in
  let u_current_index := u_ks.(br_interface_kernel_current) in
  let* u_next_index := U32.mod (Int.add u_current_index (Int.repr 1%Z)) Br_interface_N_ZONES in
  let u_ks := u_ks <| br_interface_kernel_next := u_next_index |> in
  let u_ks := u_ks <| br_interface_kernel_next_quantum := Br_interface_QUANTUM_CYC |> in
  let u_ms := u_ks.(br_interface_kernel_ms) in
  let u_ms := Br_interface_write_mtimecmp u_ms (u_ks.(br_interface_kernel_deadline)) in
  ret (u_ks <| br_interface_kernel_ms := u_ms |>).

Definition Br_sched_sched_init (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let u_current_index := p_ks.(br_interface_kernel_current) in
  let* u_current := Barray.get u_zones (U64.of_u32 u_current_index) in
  let u_current := u_current <| br_interface_zone_intrp_deleg := Int.repr 0%Z |> in
  let* u_zones := Barray.set u_zones (U64.of_u32 u_current_index) u_current in
  let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
  let u_ks := u_ks <| br_interface_kernel_current := Int.repr 0%Z |> in
  let* u_ks :=
    let b0 := Br_interface_read_mtime (u_ks.(br_interface_kernel_ms)) in
    ret (u_ks <| br_interface_kernel_deadline := Int64.add b0 Br_interface_QUANTUM_CYC |>)
  in
  let u_zones := u_ks.(br_interface_kernel_zones) in
  let u_current_index := u_ks.(br_interface_kernel_current) in
  let* u_next_index := U32.mod (Int.add u_current_index (Int.repr 1%Z)) Br_interface_N_ZONES in
  let u_ks := u_ks <| br_interface_kernel_next := u_next_index |> in
  let u_ks := u_ks <| br_interface_kernel_next_quantum := Br_interface_QUANTUM_CYC |> in
  let u_ks := u_ks <| br_interface_kernel_mie := Br_interface_MIE_TIMER |> in
  let u_ms := u_ks.(br_interface_kernel_ms) in
  let u_ms := Br_interface_write_mtimecmp u_ms (u_ks.(br_interface_kernel_deadline)) in
  let u_ks := u_ks <| br_interface_kernel_ms := u_ms |> in
  let* u_ms := Br_interface_csr_write (u_ks.(br_interface_kernel_ms)) Br_interface_mie (u_ks.(br_interface_kernel_mie)) in
  ret (u_ks <| br_interface_kernel_ms := u_ms |>).

Definition Br_sched_sched_intrp (p_ks: Br_interface_kernel) (p_zone_id: int) : res Br_interface_kernel :=
  let u_ks := p_ks <| br_interface_kernel_next := p_ks.(br_interface_kernel_current) |> in
  let u_ks := u_ks <| br_interface_kernel_current := p_zone_id |> in
  let u_now := Br_interface_read_mtime (u_ks.(br_interface_kernel_ms)) in
  let u_ks :=
    if (Int64.ltu u_now (u_ks.(br_interface_kernel_deadline))) then
      u_ks <| br_interface_kernel_next_quantum := Int64.sub (u_ks.(br_interface_kernel_deadline)) u_now |>
    else
      u_ks <| br_interface_kernel_next_quantum := Int64.repr 0%Z |>
  in
  let* u_ks :=
    let b0 := Br_interface_read_mtime (u_ks.(br_interface_kernel_ms)) in
    ret (u_ks <| br_interface_kernel_deadline := Int64.add b0 Br_interface_QUANTUM_CYC |>)
  in
  let u_ms := u_ks.(br_interface_kernel_ms) in
  let u_ms := Br_interface_write_mtimecmp u_ms (u_ks.(br_interface_kernel_deadline)) in
  ret (u_ks <| br_interface_kernel_ms := u_ms |>).

Definition Br_sched_sched (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let* u_ks :=
  let b0 := Br_interface_read_mtime (p_ks.(br_interface_kernel_ms)) in
    if (Int64.cmpu Cle (p_ks.(br_interface_kernel_deadline)) b0) then
      Br_sched_sched_yield p_ks
    else
      ret p_ks
  in
  let* u_ms :=
    let* b1 := Barray.get (u_ks.(br_interface_kernel_zones)) (U64.of_u32 (u_ks.(br_interface_kernel_current))) in
    if (Int.cmpu Cgt (b1.(br_interface_zone_intrp_deleg)) (Int.repr 0%Z)) then
      Br_interface_csr_write (u_ks.(br_interface_kernel_ms)) Br_interface_mie Br_interface_MIE_TIMER
    else
      Br_interface_csr_write (u_ks.(br_interface_kernel_ms)) Br_interface_mie (u_ks.(br_interface_kernel_mie))
  in
  ret (u_ks <| br_interface_kernel_ms := u_ms |>).

Definition Br_ecall_N_ECALLS : int := Int.repr 6%Z.

Definition Br_ecall_ECL_EXCPT_VECT : int := Int.repr 0%Z.

Definition Br_ecall_ECL_INTRP_VECT : int := Int.repr 1%Z.

Definition Br_ecall_ECL_CSRS_MIE : int := Int.repr 2%Z.

Definition Br_ecall_ECL_CSRC_MIE : int := Int.repr 3%Z.

Definition Br_ecall_ECL_SEND : int := Int.repr 4%Z.

Definition Br_ecall_ECL_RECV : int := Int.repr 5%Z.

Definition Br_ecall_A0 : int := Int.repr 10%Z.

Definition Br_ecall_A1 : int := Int.repr 11%Z.

Definition Br_ecall_A2 : int := Int.repr 12%Z.

Definition Br_ecall_A3 : int := Int.repr 13%Z.

Definition Br_ecall_A4 : int := Int.repr 14%Z.

Definition Br_ecall_A5 : int := Int.repr 15%Z.

Definition Br_ecall_A7 : int := Int.repr 17%Z.

Definition Br_ecall_N_EXCEPTIONS : int := Int.repr 12%Z.

Definition Br_ecall_N_INTERRUPTS : int := Int.repr 12%Z.

Definition Br_ecall_ecall_excpt_vect (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_regs := u_caller.(br_interface_zone_regs) in
  let* u_code := Barray.get u_regs (U64.of_u32 Br_ecall_A0) in
  let* u_handler := Barray.get u_regs (U64.of_u32 Br_ecall_A1) in
  let* u_caller :=
    if (Int.ltu u_code Br_ecall_N_EXCEPTIONS) then
      let u_excpt_handlers := u_caller.(br_interface_zone_excpt_handlers) in
      let* u_excpt_handlers := Barray.set u_excpt_handlers (U64.of_u32 u_code) u_handler in
      ret (u_caller <| br_interface_zone_excpt_handlers := u_excpt_handlers |>)
    else
      ret u_caller
  in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  ret (p_ks <| br_interface_kernel_zones := u_zones |>).

Definition Br_ecall_ecall_intrp_vect_step_2 (p_intrp_handler_zone_fielf_id: int) (p_caller_field_id: int) (p_code: int) (p_handler: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_intrp_handlers := p_ks.(br_interface_kernel_intrp_handlers) in
  let* u_intrp_handler := Barray.get u_intrp_handlers (U64.of_u32 p_code) in
  let u_intrp_handler2 :=
    if ((Int.ltu p_code Br_ecall_N_INTERRUPTS) && (Int.eq p_intrp_handler_zone_fielf_id p_caller_field_id)) then
      u_intrp_handler <| br_interface_intrp_handler_handler := p_handler |>
    else
      u_intrp_handler
  in
  let* u_intrp_handlers := Barray.set u_intrp_handlers (U64.of_u32 p_code) u_intrp_handler2 in
  let u_ks2 := p_ks <| br_interface_kernel_intrp_handlers := u_intrp_handlers |> in
  ret u_ks2.

Definition Br_ecall_ecall_intrp_vect (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_regs := u_caller.(br_interface_zone_regs) in
  let* u_code := Barray.get u_regs (U64.of_u32 Br_ecall_A0) in
  let* u_handler := Barray.get u_regs (U64.of_u32 Br_ecall_A1) in
  let u_caller_field_id := u_caller.(br_interface_zone_id) in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  let u_ks1 := p_ks <| br_interface_kernel_zones := u_zones |> in
  let u_intrp_handlers := u_ks1.(br_interface_kernel_intrp_handlers) in
  let* u_intrp_handler := Barray.get u_intrp_handlers (U64.of_u32 u_code) in
  let u_intrp_handler_zone_index := u_intrp_handler.(br_interface_intrp_handler_zone) in
  let* u_intrp_handlers := Barray.set u_intrp_handlers (U64.of_u32 u_code) u_intrp_handler in
  let u_ks2 := u_ks1 <| br_interface_kernel_intrp_handlers := u_intrp_handlers |> in
  let u_zones := u_ks2.(br_interface_kernel_zones) in
  let* u_intrp_handler_zone := Barray.get u_zones (U64.of_u32 u_intrp_handler_zone_index) in
  let u_intrp_handler_zone_fielf_id := u_intrp_handler_zone.(br_interface_zone_id) in
  let* u_zones := Barray.set u_zones (U64.of_u32 u_intrp_handler_zone_index) u_intrp_handler_zone in
  let u_ks3 := u_ks2 <| br_interface_kernel_zones := u_zones |> in
  let* u_ks4 := Br_ecall_ecall_intrp_vect_step_2 u_intrp_handler_zone_fielf_id u_caller_field_id u_code u_handler u_ks3 in
  let u_zones := u_ks4.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_caller :=
    if ((Int.ltu u_code Br_ecall_N_INTERRUPTS) && (Int.eq u_intrp_handler_zone_fielf_id u_caller_field_id)) then
      u_caller <| br_interface_zone_uie := Int.or (u_caller.(br_interface_zone_uie)) (Int.shl (Int.repr 1%Z) u_code) |>
    else
      u_caller
  in
  let u_ustatus := u_caller.(br_interface_zone_ustatus) in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  let u_ks4 := u_ks4 <| br_interface_kernel_zones := u_zones |> in
  if (((Int.ltu u_code Br_ecall_N_INTERRUPTS) && (Int.eq u_intrp_handler_zone_fielf_id u_caller_field_id)) && (Int.cmpu Cgt (Int.and u_ustatus Br_interface_USTATUS_IE) (Int.repr 0%Z))) then
    ret (u_ks4 <| br_interface_kernel_mie := Int.or (u_ks4.(br_interface_kernel_mie)) (Int.shl (Int.repr 1%Z) u_code) |>)
  else
    ret u_ks4.

Definition Br_ecall_ecall_csrs_mie_CALLER (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_caller := u_caller <| br_interface_zone_ustatus := Int.or (u_caller.(br_interface_zone_ustatus)) Br_interface_USTATUS_IE |> in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  ret (p_ks <| br_interface_kernel_zones := u_zones |>).

Definition Br_ecall_ecall_csrs_mie_KERNEL (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let* u_caller := Barray.get (p_ks.(br_interface_kernel_zones)) (U64.of_u32 p_caller_id) in
  ret (p_ks <| br_interface_kernel_mie := Int.or (p_ks.(br_interface_kernel_mie)) (u_caller.(br_interface_zone_uie)) |>).

Definition Br_ecall_ecall_csrc_mie_CALLER (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_caller :=
    let b0 := Br_interface_and_with_bitwise (u_caller.(br_interface_zone_ustatus)) Br_interface_USTATUS_IE in
    u_caller <| br_interface_zone_ustatus := b0 |>
  in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  ret (p_ks <| br_interface_kernel_zones := u_zones |>).

Definition Br_ecall_ecall_csrc_mie_KERNEL (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let* u_caller := Barray.get (p_ks.(br_interface_kernel_zones)) (U64.of_u32 p_caller_id) in
  let b0 := Br_interface_and_with_bitwise (p_ks.(br_interface_kernel_mie)) (u_caller.(br_interface_zone_uie)) in
  ret (p_ks <| br_interface_kernel_mie := b0 |>).

Definition Br_ecall_ecall_send (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_regs := u_caller.(br_interface_zone_regs) in
  let* u_receiver := Barray.get u_regs (U64.of_u32 Br_ecall_A0) in
  let* u_regs := Barray.set u_regs (U64.of_u32 Br_ecall_A0) (Int.repr 0%Z) in
  let* u_regsA1 := Barray.get u_regs (U64.of_u32 Br_ecall_A1) in
  let* u_regsA2 := Barray.get u_regs (U64.of_u32 Br_ecall_A2) in
  let* u_regsA3 := Barray.get u_regs (U64.of_u32 Br_ecall_A3) in
  let* u_regsA4 := Barray.get u_regs (U64.of_u32 Br_ecall_A4) in
  let u_caller := u_caller <| br_interface_zone_regs := u_regs |> in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
  if (Int.cmpu Cge u_receiver Br_interface_N_ZONES) then
    ret u_ks
  else
    let u_inboxes := u_ks.(br_interface_kernel_inboxes) in
    let u_index := Int.add (Int.mul u_receiver Br_interface_N_ZONES) p_caller_id in
    let* u_inbox := Barray.get u_inboxes (U64.of_u32 u_index) in
    let u_full := u_inbox.(br_interface_inbox_full) in
    let u_msgs := u_inbox.(br_interface_inbox_msgs) in
    let* u_msgs :=
      if (Int.cmpu Cgt u_full (Int.repr 0%Z)) then
        ret u_msgs
      else
        let u_full := Int.repr 1%Z in
        let* u_msgs := Barray.set u_msgs (U64.of_u32 (Int.repr 0%Z)) u_regsA1 in
        let* u_msgs := Barray.set u_msgs (U64.of_u32 (Int.repr 1%Z)) u_regsA2 in
        let* u_msgs := Barray.set u_msgs (U64.of_u32 (Int.repr 2%Z)) u_regsA3 in
        Barray.set u_msgs (U64.of_u32 (Int.repr 3%Z)) u_regsA4
    in
    let u_inbox := u_inbox <| br_interface_inbox_msgs := u_msgs |> in
    let u_inbox := u_inbox <| br_interface_inbox_full := u_full |> in
    let* u_inboxes := Barray.set u_inboxes (U64.of_u32 u_index) u_inbox in
    let u_ks := u_ks <| br_interface_kernel_inboxes := u_inboxes |> in
    let u_zones := u_ks.(br_interface_kernel_zones) in
    let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
    let u_regs := u_caller.(br_interface_zone_regs) in
    let* u_regs := Barray.set u_regs (U64.of_u32 Br_ecall_A0) (Int.repr 1%Z) in
    let u_caller := u_caller <| br_interface_zone_regs := u_regs |> in
    let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
    let u_ks := u_ks <| br_interface_kernel_zones := u_zones |> in
    ret u_ks.

Definition Br_ecall_ecall_recv (p_caller_id: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
  let u_regs := u_caller.(br_interface_zone_regs) in
  let* u_sender := Barray.get u_regs (U64.of_u32 Br_ecall_A0) in
  let* u_regs := Barray.set u_regs (U64.of_u32 Br_ecall_A0) (Int.repr 0%Z) in
  let u_caller := u_caller <| br_interface_zone_regs := u_regs |> in
  let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
  let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
  if (Int.cmpu Cge u_sender Br_interface_N_ZONES) then
    ret u_ks
  else
    let u_inboxes := u_ks.(br_interface_kernel_inboxes) in
    let u_index := Int.add (Int.mul p_caller_id Br_interface_N_ZONES) u_sender in
    let* u_inbox := Barray.get u_inboxes (U64.of_u32 u_index) in
    let u_full := u_inbox.(br_interface_inbox_full) in
    let u_msgs := u_inbox.(br_interface_inbox_msgs) in
    let u_zones := u_ks.(br_interface_kernel_zones) in
    let* u_caller := Barray.get u_zones (U64.of_u32 p_caller_id) in
    let u_regs := u_caller.(br_interface_zone_regs) in
    let* u_regs :=
      if (Int.cmpu Cle u_full (Int.repr 0%Z)) then
        ret u_regs
      else
        let u_full := Int.repr 0%Z in
        let* u_regs :=
          let* b0 := Barray.get u_msgs (U64.of_u32 (Int.repr 0%Z)) in
          Barray.set u_regs (U64.of_u32 Br_ecall_A1) b0
        in
        let* u_regs :=
          let* b1 := Barray.get u_msgs (U64.of_u32 (Int.repr 1%Z)) in
          Barray.set u_regs (U64.of_u32 Br_ecall_A2) b1
        in
        let* u_regs :=
          let* b2 := Barray.get u_msgs (U64.of_u32 (Int.repr 2%Z)) in
          Barray.set u_regs (U64.of_u32 Br_ecall_A3) b2
        in
        let* b3 := Barray.get u_msgs (U64.of_u32 (Int.repr 3%Z)) in
        Barray.set u_regs (U64.of_u32 Br_ecall_A4) b3
    in
    let* u_regs := Barray.set u_regs (U64.of_u32 Br_ecall_A0) (Int.repr 1%Z) in
    let u_caller := u_caller <| br_interface_zone_regs := u_regs |> in
    let* u_zones := Barray.set u_zones (U64.of_u32 p_caller_id) u_caller in
    let u_ks := u_ks <| br_interface_kernel_zones := u_zones |> in
    let u_inbox := u_inbox <| br_interface_inbox_msgs := u_msgs |> in
    let u_inbox := u_inbox <| br_interface_inbox_full := u_full |> in
    let* u_inboxes := Barray.set u_inboxes (U64.of_u32 u_index) u_inbox in
    let u_ks := u_ks <| br_interface_kernel_inboxes := u_inboxes |> in
    ret u_ks.

Definition Br_ecall_get_current_regA7 (p_ks: Br_interface_kernel) : res int :=
  let u_id := p_ks.(br_interface_kernel_current) in
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 u_id) in
  Barray.get (u_caller.(br_interface_zone_regs)) (U64.of_u32 Br_ecall_A7).

Definition Br_ecall_handle_ecall_wrapped (p_mcause: int) (p_mtval: int) (p_ks1: Br_interface_kernel) : res Br_interface_kernel :=
  let u_index := p_ks1.(br_interface_kernel_current) in
  let* u_ks2 := Br_ecall_ecall_csrs_mie_KERNEL u_index p_ks1 in
  let* u_regA7 := Br_ecall_get_current_regA7 u_ks2 in
  let u_ecall_id := u_regA7 in
  if (Int.ltu u_ecall_id Br_ecall_N_ECALLS) then
    if (Int.eq u_ecall_id Br_ecall_ECL_EXCPT_VECT) then
      let* u_ks22 := Br_ecall_ecall_excpt_vect u_index u_ks2 in
      ret u_ks22
    else if (Int.eq u_ecall_id Br_ecall_ECL_INTRP_VECT) then
      let* u_ks22 := Br_ecall_ecall_intrp_vect u_index u_ks2 in
      ret u_ks22
    else if (Int.eq u_ecall_id Br_ecall_ECL_CSRS_MIE) then
      let* u_ks22 := Br_ecall_ecall_csrs_mie_CALLER u_index u_ks2 in
      let* u_ks222 := Br_ecall_ecall_csrs_mie_KERNEL u_index u_ks22 in
      ret u_ks222
    else if (Int.eq u_ecall_id Br_ecall_ECL_CSRC_MIE) then
      let* u_ks22 := Br_ecall_ecall_csrc_mie_CALLER u_index u_ks2 in
      let* u_ks222 := Br_ecall_ecall_csrc_mie_KERNEL u_index u_ks22 in
      ret u_ks222
    else if (Int.eq u_ecall_id Br_ecall_ECL_SEND) then
      let* u_ks22 := Br_ecall_ecall_send u_index u_ks2 in
      ret u_ks22
    else if (Int.eq u_ecall_id Br_ecall_ECL_RECV) then
      let* u_ks22 := Br_ecall_ecall_recv u_index u_ks2 in
      ret u_ks22
    else
      ret u_ks2
  else
    ret u_ks2.

Definition Br_ecall_handle_ecall (p_mcause: int) (p_mtval: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_id := p_ks.(br_interface_kernel_current) in
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_caller := Barray.get u_zones (U64.of_u32 u_id) in
  let u_regs := u_caller.(br_interface_zone_regs) in
  let* u_regs :=
    let* b0 := Barray.get u_regs (U64.of_u32 Br_interface_PC) in
    Barray.set u_regs (U64.of_u32 Br_interface_PC) (Int.add b0 (Int.repr 4%Z))
  in
  let u_caller := u_caller <| br_interface_zone_regs := u_regs |> in
  let* u_zones := Barray.set u_zones (U64.of_u32 u_id) u_caller in
  let u_ks1 := p_ks <| br_interface_kernel_zones := u_zones |> in
  Br_ecall_handle_ecall_wrapped p_mcause p_mtval u_ks1.

Definition Br_trap_EXCPT_ILLEGAL_INSTR : int := Int.repr 2%Z.

Definition Br_trap_EXCPT_ECALL_UMODE : int := Int.repr 8%Z.

Definition Br_trap_EXCPT_ECALL_SMODE : int := Int.repr 9%Z.

Definition Br_trap_EXCPT_ECALL_MMODE : int := Int.repr 17%Z.

Definition Br_trap_INTRP_MACHINE_TIMER : int := Int.repr 7%Z.

Definition Br_trap_OP_URET : int := Int.repr 2097267%Z.

Definition Br_trap_set_ks_mie_and_bitwise (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_index := p_ks.(br_interface_kernel_current) in
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_zone := Barray.get u_zones (U64.of_u32 u_index) in
  let u_uie := u_zone.(br_interface_zone_uie) in
  let u_mie := p_ks.(br_interface_kernel_mie) in
  let u_res := Br_interface_and_with_bitwise u_mie u_uie in
  ret (p_ks <| br_interface_kernel_mie := u_res |>).

Definition Br_trap_get_current_zone_HANDLER (p_mcause: int) (p_ks: Br_interface_kernel) : res int :=
  let u_current := p_ks.(br_interface_kernel_current) in
  let* u_zone := Barray.get (p_ks.(br_interface_kernel_zones)) (U64.of_u32 u_current) in
  let* u_handler := Barray.get (u_zone.(br_interface_zone_excpt_handlers)) (U64.of_u32 p_mcause) in
  ret u_handler.

Definition Br_trap_get_current_zone_handler (p_mcause: int) (p_ks: Br_interface_kernel) : res int :=
  let u_index := p_ks.(br_interface_kernel_current) in
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let* u_zone := Barray.get u_zones (U64.of_u32 u_index) in
  let* u_handler := Barray.get (u_zone.(br_interface_zone_excpt_handlers)) (U64.of_u32 p_mcause) in
  ret u_handler.

Definition Br_trap_get_i_handler_ZONE (p_mcause: int) (p_ks: Br_interface_kernel) : res int :=
  let u_i_handlers := p_ks.(br_interface_kernel_intrp_handlers) in
  let* u_i_handler :=
    let* b0 := Br_interface_MCAUSE_INTRP p_mcause in
    Barray.get u_i_handlers (U64.of_u32 b0)
  in
  let u_handler := u_i_handler.(br_interface_intrp_handler_handler) in
  ret (u_i_handler.(br_interface_intrp_handler_zone)).

Definition Br_trap_get_i_handler_HANDLER (p_mcause: int) (p_ks: Br_interface_kernel) : res int :=
  let u_i_handlers := p_ks.(br_interface_kernel_intrp_handlers) in
  let* u_i_handler :=
    let* b0 := Br_interface_MCAUSE_INTRP p_mcause in
    Barray.get u_i_handlers (U64.of_u32 b0)
  in
  let u_handler := u_i_handler.(br_interface_intrp_handler_handler) in
  ret (u_i_handler.(br_interface_intrp_handler_handler)).

Definition Br_trap_wrapped_handle_user_intrp (p_mcause: int) (p_mtval: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_i_handlers := p_ks.(br_interface_kernel_intrp_handlers) in
  let* u_i_handler :=
    let* b0 := Br_interface_MCAUSE_INTRP p_mcause in
    Barray.get u_i_handlers (U64.of_u32 b0)
  in
  let u_handler := u_i_handler.(br_interface_intrp_handler_handler) in
  if (Int.cmpu Cgt u_handler (Int.repr 0%Z)) then
    let u_zones := p_ks.(br_interface_kernel_zones) in
    let u_index := u_i_handler.(br_interface_intrp_handler_zone) in
    let u_current := p_ks.(br_interface_kernel_current) in
    let* u_zone := Barray.get u_zones (U64.of_u32 u_index) in
    let* u_zone :=
      let* b1 := Barray.get (u_zone.(br_interface_zone_regs)) (U64.of_u32 Br_interface_PC) in
      ret (u_zone <| br_interface_zone_uepc := b1 |>)
    in
    let u_zone := u_zone <| br_interface_zone_ucause := p_mcause |> in
    let u_zone := u_zone <| br_interface_zone_utval := p_mtval |> in
    let u_regs := u_zone.(br_interface_zone_regs) in
    let u_handler_casted := u_handler in
    let* u_regs := Barray.set u_regs (U64.of_u32 Br_interface_PC) u_handler_casted in
    let u_zone := u_zone <| br_interface_zone_regs := u_regs |> in
    let u_zone := u_zone <| br_interface_zone_ustatus := Br_interface_USTATUS_PIE |> in
    let u_zone := u_zone <| br_interface_zone_intrp_deleg := Int.repr 1%Z |> in
    let* u_zones := Barray.set u_zones (U64.of_u32 u_index) u_zone in
    let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
    ret u_ks
  else
    ret p_ks.

Definition Br_trap_handle_user_intrp (p_mcause: int) (p_mtval: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let* u_handler := Br_trap_get_i_handler_HANDLER p_mcause p_ks in
  let* u_zone := Br_trap_get_i_handler_ZONE p_mcause p_ks in
  let u_current := p_ks.(br_interface_kernel_current) in
  let* u_ks1 := Br_trap_wrapped_handle_user_intrp p_mcause p_mtval p_ks in
  let* u_ks1 := Br_trap_set_ks_mie_and_bitwise u_ks1 in
  if ((Int.cmpu Cgt u_handler (Int.repr 0%Z)) && (Int.cmpu Cne u_zone u_current)) then
    Br_sched_sched_intrp u_ks1 u_zone
  else
    ret u_ks1.

Definition Br_trap_handle_user_excpt (p_mcause: int) (p_mtval: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let* u_handler := Br_trap_get_current_zone_handler p_mcause p_ks in
  if (Int.cmpu Cgt u_handler (Int.repr 0%Z)) then
    let u_index := p_ks.(br_interface_kernel_current) in
    let u_zones := p_ks.(br_interface_kernel_zones) in
    let* u_zone := Barray.get u_zones (U64.of_u32 u_index) in
    let* u_zone :=
      let* b0 := Barray.get (u_zone.(br_interface_zone_regs)) (U64.of_u32 Br_interface_PC) in
      ret (u_zone <| br_interface_zone_uepc := b0 |>)
    in
    let u_zone := u_zone <| br_interface_zone_ucause := p_mcause |> in
    let u_zone := u_zone <| br_interface_zone_utval := p_mtval |> in
    let u_regs := u_zone.(br_interface_zone_regs) in
    let u_handler_casted := u_handler in
    let* u_regs := Barray.set u_regs (U64.of_u32 Br_interface_PC) u_handler_casted in
    let u_zone := u_zone <| br_interface_zone_regs := u_regs |> in
    let u_ustatus := u_zone.(br_interface_zone_ustatus) in
    let u_zone :=
      if (Int.cmpu Cgt (Int.and u_ustatus Br_interface_USTATUS_PIE) (Int.repr 0%Z)) then
        u_zone <| br_interface_zone_ustatus := Br_interface_USTATUS_PIE |>
      else
        u_zone
    in
    let* u_zones := Barray.set u_zones (U64.of_u32 u_index) u_zone in
    let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
    if (Int.cmpu Cgt (Int.and u_ustatus Br_interface_USTATUS_PIE) (Int.repr 0%Z)) then
      Br_trap_set_ks_mie_and_bitwise u_ks
    else
      ret u_ks
  else
    ret p_ks.

Definition Br_trap_handle_uret (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_zones := p_ks.(br_interface_kernel_zones) in
  let u_index := p_ks.(br_interface_kernel_current) in
  let* u_zone := Barray.get u_zones (U64.of_u32 u_index) in
  let u_zuie := u_zone.(br_interface_zone_uie) in
  let u_ustatus := u_zone.(br_interface_zone_ustatus) in
  let u_regs := u_zone.(br_interface_zone_regs) in
  let* u_regs := Barray.set u_regs (U64.of_u32 Br_interface_PC) (u_zone.(br_interface_zone_uepc)) in
  let u_zone := u_zone <| br_interface_zone_regs := u_regs |> in
  let u_zone := u_zone <| br_interface_zone_ucause := Int.repr 0%Z |> in
  let u_zone := u_zone <| br_interface_zone_uepc := Int.repr 0%Z |> in
  let u_zone := u_zone <| br_interface_zone_utval := Int.repr 0%Z |> in
  let u_zone :=
    if (Int.cmpu Cgt (Int.and u_ustatus Br_interface_USTATUS_PIE) (Int.repr 0%Z)) then
      let u_res := Int.or (u_zone.(br_interface_zone_ustatus)) Br_interface_USTATUS_PIE in
      u_zone <| br_interface_zone_ustatus := u_res |>
    else
      u_zone
  in
  let* u_zones := Barray.set u_zones (U64.of_u32 u_index) u_zone in
  let u_ks := p_ks <| br_interface_kernel_zones := u_zones |> in
  let* u_ks1 :=
    if (Int.cmpu Cgt (Int.and u_ustatus Br_interface_USTATUS_PIE) (Int.repr 0%Z)) then
      let* u_new_mie := Br_interface_binary_or (u_ks.(br_interface_kernel_mie)) u_zuie in
      let u_ks := u_ks <| br_interface_kernel_mie := u_new_mie |> in
      ret u_ks
    else
      let u_ks := u_ks <| br_interface_kernel_mie := u_ks.(br_interface_kernel_mie) |> in
      ret u_ks
  in
  let* b0 := Barray.get (u_ks.(br_interface_kernel_zones)) (U64.of_u32 u_index) in
  if (Int.cmpu Cgt (b0.(br_interface_zone_intrp_deleg)) (Int.repr 0%Z)) then
    let* u_ks :=
      let b1 := Br_interface_read_mtime (u_ks.(br_interface_kernel_ms)) in
      ret (u_ks <| br_interface_kernel_deadline := b1 |>)
    in
    let u_zones := u_ks.(br_interface_kernel_zones) in
    let u_index := u_ks.(br_interface_kernel_current) in
    let* u_current := Barray.get u_zones (U64.of_u32 u_index) in
    let u_current := u_current <| br_interface_zone_intrp_deleg := Int.repr 0%Z |> in
    let* u_zones := Barray.set u_zones (U64.of_u32 u_index) u_current in
    let u_ks := u_ks <| br_interface_kernel_zones := u_zones |> in
    Br_sched_sched_yield u_ks
  else
    ret u_ks.

Definition Br_trap_handle_illegal_instruction (p_mcause: int) (p_mtval: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  if (Int.eq p_mtval Br_trap_OP_URET) then
    Br_trap_handle_uret p_ks
  else
    ret p_ks.

Definition Br_trap_handle_trap (p_mcause: int) (p_mtval: int) (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let u_mcause_casted := I32.of_u32 p_mcause in
  let* u_ks1 :=
    if (Int.lt u_mcause_casted (Int.repr 0%Z)) then
      let* u_res := Br_interface_MCAUSE_INTRP p_mcause in
      if (Int.eq u_res Br_trap_INTRP_MACHINE_TIMER) then
        ret p_ks
      else
        Br_trap_handle_user_intrp p_mcause p_mtval p_ks
    else if (Int.eq p_mcause Br_trap_EXCPT_ILLEGAL_INSTR) then
      Br_trap_handle_illegal_instruction p_mcause p_mtval p_ks
    else if (Int.eq p_mcause Br_trap_EXCPT_ECALL_UMODE) then
      Br_ecall_handle_ecall p_mcause p_mtval p_ks
    else
      Br_trap_handle_user_excpt p_mcause p_mtval p_ks
  in
  Br_sched_sched u_ks1.

Definition Br_kernel_main (p_ks: Br_interface_kernel) : res Br_interface_kernel :=
  let* u_ms := Br_interface_csr_write_asm_trap_entry (p_ks.(br_interface_kernel_ms)) Br_interface_mtvec in
  let u_ks := p_ks <| br_interface_kernel_ms := u_ms |> in
  let* u_ms := Br_interface_csr_write (u_ks.(br_interface_kernel_ms)) Br_interface_mstatus (Int.repr 0%Z) in
  let u_ks := u_ks <| br_interface_kernel_ms := u_ms |> in
  let* u_ks := Br_sched_sched_init u_ks in
  ret u_ks.
