#include "../kernel/src/br_interface.h"

struct Br_interface_inbox;
struct Br_interface_zone;
struct Br_interface_intrp_handler;
struct Br_interface_kernel;
struct Br_interface_inbox {
  unsigned int full;
  unsigned int *msgs;
};

struct Br_interface_zone {
  unsigned int id;
  unsigned int *regs;
  unsigned int *pmpcfg;
  unsigned int *pmpaddr;
  unsigned int ustatus;
  unsigned int uepc;
  unsigned int uie;
  unsigned int ucause;
  unsigned int utval;
  unsigned int *excpt_handlers;
  unsigned int intrp_deleg;
};

struct Br_interface_intrp_handler {
  unsigned int zone;
  unsigned int handler;
};

struct Br_interface_kernel {
  unsigned int current;
  unsigned long long deadline;
  unsigned int next;
  unsigned long long next_quantum;
  unsigned int mie;
  struct Br_interface_zone **zones;
  struct Br_interface_intrp_handler **intrp_handlers;
  struct Br_interface_inbox **inboxes;
  struct Br_interface_state *ms;
};

extern unsigned long long Br_interface_read_mtime(struct Br_interface_state *);
extern struct Br_interface_state *Br_interface_write_mtimecmp(struct Br_interface_state *, unsigned long long);
extern struct Br_interface_state *Br_interface_csr_write_asm_trap_entry(struct Br_interface_state *, int);
extern struct Br_interface_state *Br_interface_csr_write(struct Br_interface_state *, int, unsigned int);
extern unsigned int Br_interface_MCAUSE_INTRP(unsigned int);
extern unsigned int Br_interface_binary_or(unsigned int, unsigned int);
unsigned int Br_interface_and_with_bitwise(unsigned int, unsigned int);
struct Br_interface_kernel *Br_sched_sched_yield(struct Br_interface_kernel *);
struct Br_interface_kernel *Br_sched_sched_init(struct Br_interface_kernel *);
struct Br_interface_kernel *Br_sched_sched_intrp(struct Br_interface_kernel *, unsigned int);
struct Br_interface_kernel *Br_sched_sched(struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_excpt_vect(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_intrp_vect_step_2(unsigned int, unsigned int, unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_intrp_vect(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_csrs_mie_CALLER(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_csrs_mie_KERNEL(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_csrc_mie_CALLER(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_csrc_mie_KERNEL(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_send(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_ecall_recv(unsigned int, struct Br_interface_kernel *);
unsigned int Br_ecall_get_current_regA7(struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_handle_ecall_wrapped(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_ecall_handle_ecall(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_set_ks_mie_and_bitwise(struct Br_interface_kernel *);
unsigned int Br_trap_get_current_zone_HANDLER(unsigned int, struct Br_interface_kernel *);
unsigned int Br_trap_get_current_zone_handler(unsigned int, struct Br_interface_kernel *);
unsigned int Br_trap_get_i_handler_ZONE(unsigned int, struct Br_interface_kernel *);
unsigned int Br_trap_get_i_handler_HANDLER(unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_wrapped_handle_user_intrp(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_handle_user_intrp(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_handle_user_excpt(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_handle_uret(struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_handle_illegal_instruction(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_trap_handle_trap(unsigned int, unsigned int, struct Br_interface_kernel *);
struct Br_interface_kernel *Br_kernel_main(struct Br_interface_kernel *);
extern unsigned long long Br_interface_QUANTUM_CYC;

extern unsigned int Br_interface_N_ZONES;

unsigned int const Br_interface_USTATUS_IE = 1;

unsigned int const Br_interface_USTATUS_PIE = 16;

unsigned int const Br_interface_MIE_TIMER = 128;

int const Br_interface_mie = 772;

int const Br_interface_mtvec = 773;

int const Br_interface_mstatus = 774;

unsigned int const Br_interface_PC = 0;

unsigned int Br_interface_and_with_bitwise(unsigned int $p_mie, unsigned int $p_uie)
{
  register unsigned int $u_bitwise;
  register unsigned int $u_result;
  $u_bitwise = ~$p_uie;
  $u_result = $p_mie & $u_bitwise;
  return $u_result;
}

struct Br_interface_kernel *Br_sched_sched_yield(struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_current_id;
  register struct Br_interface_zone *$u_current;
  register struct Br_interface_zone **$u_zones;
  register unsigned int $b0;
  register unsigned long long $b1;
  register unsigned long long $b2;
  register unsigned int $u_current_index;
  register unsigned int $u_next_index;
  register struct Br_interface_kernel *$u_ks;
  register unsigned long long $b3;
  register struct Br_interface_state *$u_ms;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_current_id = (*$p_ks).current;
  $u_current = *($u_zones + $u_current_id);
  *($u_zones + $u_current_id) = $u_current;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks = $p_ks;
  $b0 = (*$u_ks).next;
  (*$u_ks).current = $b0;
  $u_ks = $u_ks;
  $b1 = (*$u_ks).deadline;
  $b2 = (*$u_ks).next_quantum;
  (*$u_ks).deadline = $b1 + $b2;
  $u_ks = $u_ks;
  $u_current_index = (*$u_ks).current;
  $u_next_index = ($u_current_index + 1) % Br_interface_N_ZONES;
  (*$u_ks).next = $u_next_index;
  $u_ks = $u_ks;
  (*$u_ks).next_quantum = Br_interface_QUANTUM_CYC;
  $u_ks = $u_ks;
  $u_ms = (*$u_ks).ms;
  $b3 = (*$u_ks).deadline;
  $u_ms = Br_interface_write_mtimecmp($u_ms, $b3);
  (*$u_ks).ms = $u_ms;
  $i0 = $u_ks;
  return $i0;
}

struct Br_interface_kernel *Br_sched_sched_init(struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_zone *$u_current;
  register struct Br_interface_state *$b1;
  register unsigned long long $b0;
  register struct Br_interface_zone **$u_zones;
  register unsigned int $u_current_index;
  register unsigned int $u_next_index;
  register unsigned long long $b2;
  register struct Br_interface_kernel *$u_ks;
  register struct Br_interface_state *$b3;
  register unsigned int $b4;
  register struct Br_interface_state *$u_ms;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_current_index = (*$p_ks).current;
  $u_current = *($u_zones + $u_current_index);
  (*$u_current).intrp_deleg = 0;
  $u_current = $u_current;
  *($u_zones + $u_current_index) = $u_current;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks = $p_ks;
  (*$u_ks).current = 0;
  $u_ks = $u_ks;
  $b1 = (*$u_ks).ms;
  $b0 = Br_interface_read_mtime($b1);
  (*$u_ks).deadline = $b0 + Br_interface_QUANTUM_CYC;
  $u_ks = $u_ks;
  $u_zones = (*$u_ks).zones;
  $u_current_index = (*$u_ks).current;
  $u_next_index = ($u_current_index + 1) % Br_interface_N_ZONES;
  (*$u_ks).next = $u_next_index;
  $u_ks = $u_ks;
  (*$u_ks).next_quantum = Br_interface_QUANTUM_CYC;
  $u_ks = $u_ks;
  (*$u_ks).mie = Br_interface_MIE_TIMER;
  $u_ks = $u_ks;
  $u_ms = (*$u_ks).ms;
  $b2 = (*$u_ks).deadline;
  $u_ms = Br_interface_write_mtimecmp($u_ms, $b2);
  (*$u_ks).ms = $u_ms;
  $u_ks = $u_ks;
  $b3 = (*$u_ks).ms;
  $b4 = (*$u_ks).mie;
  $u_ms = Br_interface_csr_write($b3, Br_interface_mie, $b4);
  (*$u_ks).ms = $u_ms;
  $i0 = $u_ks;
  return $i0;
}

struct Br_interface_kernel *Br_sched_sched_intrp(struct Br_interface_kernel *$p_ks, unsigned int $p_zone_id)
{
  register unsigned int $b0;
  register struct Br_interface_state *$b1;
  register unsigned long long $u_now;
  register unsigned long long $b2;
  register unsigned long long $b3;
  register struct Br_interface_state *$b5;
  register unsigned long long $b4;
  register struct Br_interface_kernel *$u_ks;
  register unsigned long long $b6;
  register struct Br_interface_state *$u_ms;
  register struct Br_interface_kernel *$i0;
  $b0 = (*$p_ks).current;
  (*$p_ks).next = $b0;
  $u_ks = $p_ks;
  (*$u_ks).current = $p_zone_id;
  $u_ks = $u_ks;
  $b1 = (*$u_ks).ms;
  $u_now = Br_interface_read_mtime($b1);
  $b2 = (*$u_ks).deadline;
  if ($u_now < $b2) {
    $b3 = (*$u_ks).deadline;
    (*$u_ks).next_quantum = $b3 - $u_now;
    $u_ks = $u_ks;
  } else {
    (*$u_ks).next_quantum = 0LL;
    $u_ks = $u_ks;
  }
  $b5 = (*$u_ks).ms;
  $b4 = Br_interface_read_mtime($b5);
  (*$u_ks).deadline = $b4 + Br_interface_QUANTUM_CYC;
  $u_ks = $u_ks;
  $u_ms = (*$u_ks).ms;
  $b6 = (*$u_ks).deadline;
  $u_ms = Br_interface_write_mtimecmp($u_ms, $b6);
  (*$u_ks).ms = $u_ms;
  $i0 = $u_ks;
  return $i0;
}

struct Br_interface_kernel *Br_sched_sched(struct Br_interface_kernel *$p_ks)
{
  register unsigned long long $b0;
  register struct Br_interface_state *$b2;
  register unsigned long long $b1;
  register struct Br_interface_kernel *$u_ks;
  register unsigned int $b4;
  register unsigned int $b3;
  register struct Br_interface_state *$b5;
  register struct Br_interface_state *$b6;
  register unsigned int $b7;
  register struct Br_interface_state *$u_ms;
  register struct Br_interface_kernel *$i0;
  $b0 = (*$p_ks).deadline;
  $b2 = (*$p_ks).ms;
  $b1 = Br_interface_read_mtime($b2);
  if ($b0 <= $b1) {
    $u_ks = Br_sched_sched_yield($p_ks);
  } else {
    $u_ks = $p_ks;
  }
  $b4 = (*$u_ks).current;
  $b3 = (**((*$u_ks).zones + $b4)).intrp_deleg;
  if ($b3 > 0) {
    $b5 = (*$u_ks).ms;
    $u_ms =
      Br_interface_csr_write
      ($b5, Br_interface_mie, Br_interface_MIE_TIMER);
  } else {
    $b6 = (*$u_ks).ms;
    $b7 = (*$u_ks).mie;
    $u_ms = Br_interface_csr_write($b6, Br_interface_mie, $b7);
  }
  (*$u_ks).ms = $u_ms;
  $i0 = $u_ks;
  return $i0;
}

unsigned int const Br_ecall_N_ECALLS = 6;

unsigned int const Br_ecall_ECL_EXCPT_VECT = 0;

unsigned int const Br_ecall_ECL_INTRP_VECT = 1;

unsigned int const Br_ecall_ECL_CSRS_MIE = 2;

unsigned int const Br_ecall_ECL_CSRC_MIE = 3;

unsigned int const Br_ecall_ECL_SEND = 4;

unsigned int const Br_ecall_ECL_RECV = 5;

unsigned int const Br_ecall_A0 = 10;

unsigned int const Br_ecall_A1 = 11;

unsigned int const Br_ecall_A2 = 12;

unsigned int const Br_ecall_A3 = 13;

unsigned int const Br_ecall_A4 = 14;

unsigned int const Br_ecall_A5 = 15;

unsigned int const Br_ecall_A7 = 17;

unsigned int const Br_ecall_N_EXCEPTIONS = 12;

unsigned int const Br_ecall_N_INTERRUPTS = 12;

struct Br_interface_kernel *Br_ecall_ecall_excpt_vect(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register unsigned int *$u_regs;
  register unsigned int $u_code;
  register unsigned int $u_handler;
  register unsigned int *$u_excpt_handlers;
  register struct Br_interface_zone *$u_caller;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $p_caller_id);
  $u_regs = (*$u_caller).regs;
  $u_code = *($u_regs + Br_ecall_A0);
  $u_handler = *($u_regs + Br_ecall_A1);
  if ($u_code < Br_ecall_N_EXCEPTIONS) {
    $u_excpt_handlers = (*$u_caller).excpt_handlers;
    *($u_excpt_handlers + $u_code) = $u_handler;
    $u_excpt_handlers = $u_excpt_handlers;
    (*$u_caller).excpt_handlers = $u_excpt_handlers;
    $u_caller = $u_caller;
  } else {
    $u_caller = $u_caller;
  }
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $i0 = $p_ks;
  return $i0;
}

struct Br_interface_kernel *Br_ecall_ecall_intrp_vect_step_2(unsigned int $p_intrp_handler_zone_fielf_id, unsigned int $p_caller_field_id, unsigned int $p_code, unsigned int $p_handler, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_intrp_handler *$u_intrp_handler;
  register struct Br_interface_intrp_handler *$u_intrp_handler2;
  register struct Br_interface_intrp_handler **$u_intrp_handlers;
  register struct Br_interface_kernel *$u_ks2;
  $u_intrp_handlers = (*$p_ks).intrp_handlers;
  $u_intrp_handler = *($u_intrp_handlers + $p_code);
  if ($p_code < Br_ecall_N_INTERRUPTS
        && $p_intrp_handler_zone_fielf_id == $p_caller_field_id) {
    (*$u_intrp_handler).handler = $p_handler;
    $u_intrp_handler2 = $u_intrp_handler;
  } else {
    $u_intrp_handler2 = $u_intrp_handler;
  }
  *($u_intrp_handlers + $p_code) = $u_intrp_handler2;
  $u_intrp_handlers = $u_intrp_handlers;
  (*$p_ks).intrp_handlers = $u_intrp_handlers;
  $u_ks2 = $p_ks;
  return $u_ks2;
}

struct Br_interface_kernel *Br_ecall_ecall_intrp_vect(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register unsigned int *$u_regs;
  register unsigned int $u_code;
  register unsigned int $u_handler;
  register unsigned int $u_caller_field_id;
  register struct Br_interface_kernel *$u_ks1;
  register struct Br_interface_intrp_handler *$u_intrp_handler;
  register unsigned int $u_intrp_handler_zone_index;
  register struct Br_interface_intrp_handler **$u_intrp_handlers;
  register struct Br_interface_kernel *$u_ks2;
  register struct Br_interface_zone *$u_intrp_handler_zone;
  register unsigned int $u_intrp_handler_zone_fielf_id;
  register struct Br_interface_kernel *$u_ks3;
  register unsigned int $b0;
  register struct Br_interface_zone *$u_caller;
  register unsigned int $u_ustatus;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$u_ks4;
  register unsigned int $b1;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $p_caller_id);
  $u_regs = (*$u_caller).regs;
  $u_code = *($u_regs + Br_ecall_A0);
  $u_handler = *($u_regs + Br_ecall_A1);
  $u_caller_field_id = (*$u_caller).id;
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks1 = $p_ks;
  $u_intrp_handlers = (*$u_ks1).intrp_handlers;
  $u_intrp_handler = *($u_intrp_handlers + $u_code);
  $u_intrp_handler_zone_index = (*$u_intrp_handler).zone;
  *($u_intrp_handlers + $u_code) = $u_intrp_handler;
  $u_intrp_handlers = $u_intrp_handlers;
  (*$u_ks1).intrp_handlers = $u_intrp_handlers;
  $u_ks2 = $u_ks1;
  $u_zones = (*$u_ks2).zones;
  $u_intrp_handler_zone = *($u_zones + $u_intrp_handler_zone_index);
  $u_intrp_handler_zone_fielf_id = (*$u_intrp_handler_zone).id;
  *($u_zones + $u_intrp_handler_zone_index) = $u_intrp_handler_zone;
  $u_zones = $u_zones;
  (*$u_ks2).zones = $u_zones;
  $u_ks3 = $u_ks2;
  $u_ks4 =
    Br_ecall_ecall_intrp_vect_step_2
    ($u_intrp_handler_zone_fielf_id, $u_caller_field_id, $u_code, $u_handler,
     $u_ks3);
  $u_zones = (*$u_ks4).zones;
  $u_caller = *($u_zones + $p_caller_id);
  if ($u_code < Br_ecall_N_INTERRUPTS
        && $u_intrp_handler_zone_fielf_id == $u_caller_field_id) {
    $b0 = (*$u_caller).uie;
    (*$u_caller).uie = $b0 | 1 << $u_code;
    $u_caller = $u_caller;
  } else {
    $u_caller = $u_caller;
  }
  $u_ustatus = (*$u_caller).ustatus;
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$u_ks4).zones = $u_zones;
  $u_ks4 = $u_ks4;
  if ($u_code < Br_ecall_N_INTERRUPTS
        && $u_intrp_handler_zone_fielf_id == $u_caller_field_id
        && ($u_ustatus & Br_interface_USTATUS_IE) > 0) {
    $b1 = (*$u_ks4).mie;
    (*$u_ks4).mie = $b1 | 1 << $u_code;
    $i0 = $u_ks4;
    return $i0;
  } else {
    return $u_ks4;
  }
}

struct Br_interface_kernel *Br_ecall_ecall_csrs_mie_CALLER(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $b0;
  register struct Br_interface_zone *$u_caller;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $p_caller_id);
  $b0 = (*$u_caller).ustatus;
  (*$u_caller).ustatus = $b0 | Br_interface_USTATUS_IE;
  $u_caller = $u_caller;
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $i0 = $p_ks;
  return $i0;
}

struct Br_interface_kernel *Br_ecall_ecall_csrs_mie_KERNEL(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_zone **$b0;
  register struct Br_interface_zone *$u_caller;
  register unsigned int $b1;
  register unsigned int $b2;
  register struct Br_interface_kernel *$i0;
  $b0 = (*$p_ks).zones;
  $u_caller = *($b0 + $p_caller_id);
  $b1 = (*$p_ks).mie;
  $b2 = (*$u_caller).uie;
  (*$p_ks).mie = $b1 | $b2;
  $i0 = $p_ks;
  return $i0;
}

struct Br_interface_kernel *Br_ecall_ecall_csrc_mie_CALLER(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $b1;
  register unsigned int $b0;
  register struct Br_interface_zone *$u_caller;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $p_caller_id);
  $b1 = (*$u_caller).ustatus;
  $b0 = Br_interface_and_with_bitwise($b1, Br_interface_USTATUS_IE);
  (*$u_caller).ustatus = $b0;
  $u_caller = $u_caller;
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $i0 = $p_ks;
  return $i0;
}

struct Br_interface_kernel *Br_ecall_ecall_csrc_mie_KERNEL(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_zone **$b0;
  register struct Br_interface_zone *$u_caller;
  register unsigned int $b2;
  register unsigned int $b3;
  register unsigned int $b1;
  register struct Br_interface_kernel *$i0;
  $b0 = (*$p_ks).zones;
  $u_caller = *($b0 + $p_caller_id);
  $b2 = (*$p_ks).mie;
  $b3 = (*$u_caller).uie;
  $b1 = Br_interface_and_with_bitwise($b2, $b3);
  (*$p_ks).mie = $b1;
  $i0 = $p_ks;
  return $i0;
}

struct Br_interface_kernel *Br_ecall_ecall_send(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_receiver;
  register unsigned int $u_regsA1;
  register unsigned int $u_regsA2;
  register unsigned int $u_regsA3;
  register unsigned int $u_regsA4;
  register unsigned int $u_index;
  register unsigned int $u_full;
  register unsigned int *$u_msgs;
  register struct Br_interface_inbox *$u_inbox;
  register struct Br_interface_inbox **$u_inboxes;
  register unsigned int *$u_regs;
  register struct Br_interface_zone *$u_caller;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$u_ks;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $p_caller_id);
  $u_regs = (*$u_caller).regs;
  $u_receiver = *($u_regs + Br_ecall_A0);
  *($u_regs + Br_ecall_A0) = 0;
  $u_regs = $u_regs;
  $u_regsA1 = *($u_regs + Br_ecall_A1);
  $u_regsA2 = *($u_regs + Br_ecall_A2);
  $u_regsA3 = *($u_regs + Br_ecall_A3);
  $u_regsA4 = *($u_regs + Br_ecall_A4);
  (*$u_caller).regs = $u_regs;
  $u_caller = $u_caller;
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks = $p_ks;
  if ($u_receiver >= Br_interface_N_ZONES) {
    return $u_ks;
  } else {
    $u_inboxes = (*$u_ks).inboxes;
    $u_index = $u_receiver * Br_interface_N_ZONES + $p_caller_id;
    $u_inbox = *($u_inboxes + $u_index);
    $u_full = (*$u_inbox).full;
    $u_msgs = (*$u_inbox).msgs;
    if ($u_full > 0) {
      $u_msgs = $u_msgs;
    } else {
      $u_full = 1;
      *($u_msgs + 0) = $u_regsA1;
      $u_msgs = $u_msgs;
      *($u_msgs + 1) = $u_regsA2;
      $u_msgs = $u_msgs;
      *($u_msgs + 2) = $u_regsA3;
      $u_msgs = $u_msgs;
      *($u_msgs + 3) = $u_regsA4;
      $u_msgs = $u_msgs;
    }
    (*$u_inbox).msgs = $u_msgs;
    $u_inbox = $u_inbox;
    (*$u_inbox).full = $u_full;
    $u_inbox = $u_inbox;
    *($u_inboxes + $u_index) = $u_inbox;
    $u_inboxes = $u_inboxes;
    (*$u_ks).inboxes = $u_inboxes;
    $u_ks = $u_ks;
    $u_zones = (*$u_ks).zones;
    $u_caller = *($u_zones + $p_caller_id);
    $u_regs = (*$u_caller).regs;
    *($u_regs + Br_ecall_A0) = 1;
    $u_regs = $u_regs;
    (*$u_caller).regs = $u_regs;
    $u_caller = $u_caller;
    *($u_zones + $p_caller_id) = $u_caller;
    $u_zones = $u_zones;
    (*$u_ks).zones = $u_zones;
    $u_ks = $u_ks;
    return $u_ks;
  }
}

struct Br_interface_kernel *Br_ecall_ecall_recv(unsigned int $p_caller_id, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_sender;
  register unsigned int $u_index;
  register unsigned int *$u_msgs;
  register unsigned int $u_full;
  register unsigned int $b0;
  register unsigned int $b1;
  register unsigned int $b2;
  register unsigned int $b3;
  register unsigned int *$u_regs;
  register struct Br_interface_zone *$u_caller;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_inbox *$u_inbox;
  register struct Br_interface_inbox **$u_inboxes;
  register struct Br_interface_kernel *$u_ks;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $p_caller_id);
  $u_regs = (*$u_caller).regs;
  $u_sender = *($u_regs + Br_ecall_A0);
  *($u_regs + Br_ecall_A0) = 0;
  $u_regs = $u_regs;
  (*$u_caller).regs = $u_regs;
  $u_caller = $u_caller;
  *($u_zones + $p_caller_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks = $p_ks;
  if ($u_sender >= Br_interface_N_ZONES) {
    return $u_ks;
  } else {
    $u_inboxes = (*$u_ks).inboxes;
    $u_index = $p_caller_id * Br_interface_N_ZONES + $u_sender;
    $u_inbox = *($u_inboxes + $u_index);
    $u_full = (*$u_inbox).full;
    $u_msgs = (*$u_inbox).msgs;
    $u_zones = (*$u_ks).zones;
    $u_caller = *($u_zones + $p_caller_id);
    $u_regs = (*$u_caller).regs;
    if ($u_full <= 0) {
      $u_regs = $u_regs;
    } else {
      $u_full = 0;
      $b0 = *($u_msgs + 0);
      *($u_regs + Br_ecall_A1) = $b0;
      $u_regs = $u_regs;
      $b1 = *($u_msgs + 1);
      *($u_regs + Br_ecall_A2) = $b1;
      $u_regs = $u_regs;
      $b2 = *($u_msgs + 2);
      *($u_regs + Br_ecall_A3) = $b2;
      $u_regs = $u_regs;
      $b3 = *($u_msgs + 3);
      *($u_regs + Br_ecall_A4) = $b3;
      $u_regs = $u_regs;
    }
    *($u_regs + Br_ecall_A0) = 1;
    $u_regs = $u_regs;
    (*$u_caller).regs = $u_regs;
    $u_caller = $u_caller;
    *($u_zones + $p_caller_id) = $u_caller;
    $u_zones = $u_zones;
    (*$u_ks).zones = $u_zones;
    $u_ks = $u_ks;
    (*$u_inbox).msgs = $u_msgs;
    $u_inbox = $u_inbox;
    (*$u_inbox).full = $u_full;
    $u_inbox = $u_inbox;
    *($u_inboxes + $u_index) = $u_inbox;
    $u_inboxes = $u_inboxes;
    (*$u_ks).inboxes = $u_inboxes;
    $u_ks = $u_ks;
    return $u_ks;
  }
}

unsigned int Br_ecall_get_current_regA7(struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_id;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_zone *$u_caller;
  register unsigned int $i0;
  $u_id = (*$p_ks).current;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $u_id);
  $i0 = *((*$u_caller).regs + Br_ecall_A7);
  return $i0;
}

struct Br_interface_kernel *Br_ecall_handle_ecall_wrapped(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks1)
{
  register unsigned int $u_index;
  register struct Br_interface_kernel *$u_ks2;
  register unsigned int $u_regA7;
  register unsigned int $u_ecall_id;
  register struct Br_interface_kernel *$u_ks222;
  register struct Br_interface_kernel *$u_ks22;
  $u_index = (*$p_ks1).current;
  $u_ks2 = Br_ecall_ecall_csrs_mie_KERNEL($u_index, $p_ks1);
  $u_regA7 = Br_ecall_get_current_regA7($u_ks2);
  $u_ecall_id = $u_regA7;
  if ($u_ecall_id < Br_ecall_N_ECALLS) {
    if ($u_ecall_id == Br_ecall_ECL_EXCPT_VECT) {
      $u_ks22 = Br_ecall_ecall_excpt_vect($u_index, $u_ks2);
      return $u_ks22;
    } else {
      if ($u_ecall_id == Br_ecall_ECL_INTRP_VECT) {
        $u_ks22 = Br_ecall_ecall_intrp_vect($u_index, $u_ks2);
        return $u_ks22;
      } else {
        if ($u_ecall_id == Br_ecall_ECL_CSRS_MIE) {
          $u_ks22 = Br_ecall_ecall_csrs_mie_CALLER($u_index, $u_ks2);
          $u_ks222 = Br_ecall_ecall_csrs_mie_KERNEL($u_index, $u_ks22);
          return $u_ks222;
        } else {
          if ($u_ecall_id == Br_ecall_ECL_CSRC_MIE) {
            $u_ks22 = Br_ecall_ecall_csrc_mie_CALLER($u_index, $u_ks2);
            $u_ks222 = Br_ecall_ecall_csrc_mie_KERNEL($u_index, $u_ks22);
            return $u_ks222;
          } else {
            if ($u_ecall_id == Br_ecall_ECL_SEND) {
              $u_ks22 = Br_ecall_ecall_send($u_index, $u_ks2);
              return $u_ks22;
            } else {
              if ($u_ecall_id == Br_ecall_ECL_RECV) {
                $u_ks22 = Br_ecall_ecall_recv($u_index, $u_ks2);
                return $u_ks22;
              } else {
                return $u_ks2;
              }
            }
          }
        }
      }
    }
  } else {
    return $u_ks2;
  }
}

struct Br_interface_kernel *Br_ecall_handle_ecall(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_id;
  register unsigned int $b0;
  register unsigned int *$u_regs;
  register struct Br_interface_zone *$u_caller;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$u_ks1;
  register struct Br_interface_kernel *$i0;
  $u_id = (*$p_ks).current;
  $u_zones = (*$p_ks).zones;
  $u_caller = *($u_zones + $u_id);
  $u_regs = (*$u_caller).regs;
  $b0 = *($u_regs + Br_interface_PC);
  *($u_regs + Br_interface_PC) = $b0 + 4;
  $u_regs = $u_regs;
  (*$u_caller).regs = $u_regs;
  $u_caller = $u_caller;
  *($u_zones + $u_id) = $u_caller;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks1 = $p_ks;
  $i0 = Br_ecall_handle_ecall_wrapped($p_mcause, $p_mtval, $u_ks1);
  return $i0;
}

unsigned int const Br_trap_EXCPT_ILLEGAL_INSTR = 2;

unsigned int const Br_trap_EXCPT_ECALL_UMODE = 8;

unsigned int const Br_trap_EXCPT_ECALL_SMODE = 9;

unsigned int const Br_trap_EXCPT_ECALL_MMODE = 17;

unsigned int const Br_trap_INTRP_MACHINE_TIMER = 7;

unsigned int const Br_trap_OP_URET = 2097267;

struct Br_interface_kernel *Br_trap_set_ks_mie_and_bitwise(struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_index;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_zone *$u_zone;
  register unsigned int $u_uie;
  register unsigned int $u_mie;
  register unsigned int $u_res;
  register struct Br_interface_kernel *$i0;
  $u_index = (*$p_ks).current;
  $u_zones = (*$p_ks).zones;
  $u_zone = *($u_zones + $u_index);
  $u_uie = (*$u_zone).uie;
  $u_mie = (*$p_ks).mie;
  $u_res = Br_interface_and_with_bitwise($u_mie, $u_uie);
  (*$p_ks).mie = $u_res;
  $i0 = $p_ks;
  return $i0;
}

unsigned int Br_trap_get_current_zone_HANDLER(unsigned int $p_mcause, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_current;
  register struct Br_interface_zone **$b0;
  register struct Br_interface_zone *$u_zone;
  register unsigned int $u_handler;
  $u_current = (*$p_ks).current;
  $b0 = (*$p_ks).zones;
  $u_zone = *($b0 + $u_current);
  $u_handler = *((*$u_zone).excpt_handlers + $p_mcause);
  return $u_handler;
}

unsigned int Br_trap_get_current_zone_handler(unsigned int $p_mcause, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_index;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_zone *$u_zone;
  register unsigned int $u_handler;
  $u_index = (*$p_ks).current;
  $u_zones = (*$p_ks).zones;
  $u_zone = *($u_zones + $u_index);
  $u_handler = *((*$u_zone).excpt_handlers + $p_mcause);
  return $u_handler;
}

unsigned int Br_trap_get_i_handler_ZONE(unsigned int $p_mcause, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_intrp_handler **$u_i_handlers;
  register unsigned int $b0;
  register struct Br_interface_intrp_handler *$u_i_handler;
  register unsigned int $u_handler;
  register unsigned int $i0;
  $u_i_handlers = (*$p_ks).intrp_handlers;
  $b0 = Br_interface_MCAUSE_INTRP($p_mcause);
  $u_i_handler = *($u_i_handlers + $b0);
  $u_handler = (*$u_i_handler).handler;
  $i0 = (*$u_i_handler).zone;
  return $i0;
}

unsigned int Br_trap_get_i_handler_HANDLER(unsigned int $p_mcause, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_intrp_handler **$u_i_handlers;
  register unsigned int $b0;
  register struct Br_interface_intrp_handler *$u_i_handler;
  register unsigned int $u_handler;
  register unsigned int $i0;
  $u_i_handlers = (*$p_ks).intrp_handlers;
  $b0 = Br_interface_MCAUSE_INTRP($p_mcause);
  $u_i_handler = *($u_i_handlers + $b0);
  $u_handler = (*$u_i_handler).handler;
  $i0 = (*$u_i_handler).handler;
  return $i0;
}

struct Br_interface_kernel *Br_trap_wrapped_handle_user_intrp(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_intrp_handler **$u_i_handlers;
  register unsigned int $b0;
  register struct Br_interface_intrp_handler *$u_i_handler;
  register unsigned int $u_handler;
  register unsigned int $u_index;
  register unsigned int $u_current;
  register unsigned int $b1;
  register unsigned int $u_handler_casted;
  register unsigned int *$u_regs;
  register struct Br_interface_zone *$u_zone;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$u_ks;
  $u_i_handlers = (*$p_ks).intrp_handlers;
  $b0 = Br_interface_MCAUSE_INTRP($p_mcause);
  $u_i_handler = *($u_i_handlers + $b0);
  $u_handler = (*$u_i_handler).handler;
  if ($u_handler > 0) {
    $u_zones = (*$p_ks).zones;
    $u_index = (*$u_i_handler).zone;
    $u_current = (*$p_ks).current;
    $u_zone = *($u_zones + $u_index);
    $b1 = *((*$u_zone).regs + Br_interface_PC);
    (*$u_zone).uepc = $b1;
    $u_zone = $u_zone;
    (*$u_zone).ucause = $p_mcause;
    $u_zone = $u_zone;
    (*$u_zone).utval = $p_mtval;
    $u_zone = $u_zone;
    $u_regs = (*$u_zone).regs;
    $u_handler_casted = (unsigned int) $u_handler;
    *($u_regs + Br_interface_PC) = $u_handler_casted;
    $u_regs = $u_regs;
    (*$u_zone).regs = $u_regs;
    $u_zone = $u_zone;
    (*$u_zone).ustatus = Br_interface_USTATUS_PIE;
    $u_zone = $u_zone;
    (*$u_zone).intrp_deleg = 1;
    $u_zone = $u_zone;
    *($u_zones + $u_index) = $u_zone;
    $u_zones = $u_zones;
    (*$p_ks).zones = $u_zones;
    $u_ks = $p_ks;
    return $u_ks;
  } else {
    return $p_ks;
  }
}

struct Br_interface_kernel *Br_trap_handle_user_intrp(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_handler;
  register unsigned int $u_zone;
  register unsigned int $u_current;
  register struct Br_interface_kernel *$u_ks1;
  register struct Br_interface_kernel *$i0;
  $u_handler = Br_trap_get_i_handler_HANDLER($p_mcause, $p_ks);
  $u_zone = Br_trap_get_i_handler_ZONE($p_mcause, $p_ks);
  $u_current = (*$p_ks).current;
  $u_ks1 = Br_trap_wrapped_handle_user_intrp($p_mcause, $p_mtval, $p_ks);
  $u_ks1 = Br_trap_set_ks_mie_and_bitwise($u_ks1);
  if ($u_handler > 0 && $u_zone != $u_current) {
    $i0 = Br_sched_sched_intrp($u_ks1, $u_zone);
    return $i0;
  } else {
    return $u_ks1;
  }
}

struct Br_interface_kernel *Br_trap_handle_user_excpt(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_handler;
  register unsigned int $u_index;
  register unsigned int $b0;
  register unsigned int $u_handler_casted;
  register unsigned int *$u_regs;
  register unsigned int $u_ustatus;
  register struct Br_interface_zone *$u_zone;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$u_ks;
  register struct Br_interface_kernel *$i0;
  $u_handler = Br_trap_get_current_zone_handler($p_mcause, $p_ks);
  if ($u_handler > 0) {
    $u_index = (*$p_ks).current;
    $u_zones = (*$p_ks).zones;
    $u_zone = *($u_zones + $u_index);
    $b0 = *((*$u_zone).regs + Br_interface_PC);
    (*$u_zone).uepc = $b0;
    $u_zone = $u_zone;
    (*$u_zone).ucause = $p_mcause;
    $u_zone = $u_zone;
    (*$u_zone).utval = $p_mtval;
    $u_zone = $u_zone;
    $u_regs = (*$u_zone).regs;
    $u_handler_casted = (unsigned int) $u_handler;
    *($u_regs + Br_interface_PC) = $u_handler_casted;
    $u_regs = $u_regs;
    (*$u_zone).regs = $u_regs;
    $u_zone = $u_zone;
    $u_ustatus = (*$u_zone).ustatus;
    if (($u_ustatus & Br_interface_USTATUS_PIE) > 0) {
      (*$u_zone).ustatus = Br_interface_USTATUS_PIE;
      $u_zone = $u_zone;
    } else {
      $u_zone = $u_zone;
    }
    *($u_zones + $u_index) = $u_zone;
    $u_zones = $u_zones;
    (*$p_ks).zones = $u_zones;
    $u_ks = $p_ks;
    if (($u_ustatus & Br_interface_USTATUS_PIE) > 0) {
      $i0 = Br_trap_set_ks_mie_and_bitwise($u_ks);
      return $i0;
    } else {
      return $u_ks;
    }
  } else {
    return $p_ks;
  }
}

struct Br_interface_kernel *Br_trap_handle_uret(struct Br_interface_kernel *$p_ks)
{
  register unsigned int $u_zuie;
  register unsigned int $u_ustatus;
  register unsigned int $b0;
  register unsigned int *$u_regs;
  register unsigned int $b1;
  register unsigned int $u_res;
  register struct Br_interface_zone *$u_zone;
  register unsigned int $b2;
  register unsigned int $u_new_mie;
  register unsigned int $b3;
  register struct Br_interface_kernel *$u_ks1;
  register unsigned int $b4;
  register struct Br_interface_state *$b6;
  register unsigned long long $b5;
  register unsigned int $u_index;
  register struct Br_interface_zone *$u_current;
  register struct Br_interface_zone **$u_zones;
  register struct Br_interface_kernel *$u_ks;
  register struct Br_interface_kernel *$i0;
  $u_zones = (*$p_ks).zones;
  $u_index = (*$p_ks).current;
  $u_zone = *($u_zones + $u_index);
  $u_zuie = (*$u_zone).uie;
  $u_ustatus = (*$u_zone).ustatus;
  $u_regs = (*$u_zone).regs;
  $b0 = (*$u_zone).uepc;
  *($u_regs + Br_interface_PC) = $b0;
  $u_regs = $u_regs;
  (*$u_zone).regs = $u_regs;
  $u_zone = $u_zone;
  (*$u_zone).ucause = 0;
  $u_zone = $u_zone;
  (*$u_zone).uepc = 0;
  $u_zone = $u_zone;
  (*$u_zone).utval = 0;
  $u_zone = $u_zone;
  if (($u_ustatus & Br_interface_USTATUS_PIE) > 0) {
    $b1 = (*$u_zone).ustatus;
    $u_res = $b1 | Br_interface_USTATUS_PIE;
    (*$u_zone).ustatus = $u_res;
    $u_zone = $u_zone;
  } else {
    $u_zone = $u_zone;
  }
  *($u_zones + $u_index) = $u_zone;
  $u_zones = $u_zones;
  (*$p_ks).zones = $u_zones;
  $u_ks = $p_ks;
  if (($u_ustatus & Br_interface_USTATUS_PIE) > 0) {
    $b2 = (*$u_ks).mie;
    $u_new_mie = Br_interface_binary_or($b2, $u_zuie);
    (*$u_ks).mie = $u_new_mie;
    $u_ks = $u_ks;
    $u_ks1 = $u_ks;
  } else {
    $b3 = (*$u_ks).mie;
    (*$u_ks).mie = $b3;
    $u_ks = $u_ks;
    $u_ks1 = $u_ks;
  }
  $b4 = (**((*$u_ks).zones + $u_index)).intrp_deleg;
  if ($b4 > 0) {
    $b6 = (*$u_ks).ms;
    $b5 = Br_interface_read_mtime($b6);
    (*$u_ks).deadline = $b5;
    $u_ks = $u_ks;
    $u_zones = (*$u_ks).zones;
    $u_index = (*$u_ks).current;
    $u_current = *($u_zones + $u_index);
    (*$u_current).intrp_deleg = 0;
    $u_current = $u_current;
    *($u_zones + $u_index) = $u_current;
    $u_zones = $u_zones;
    (*$u_ks).zones = $u_zones;
    $u_ks = $u_ks;
    $i0 = Br_sched_sched_yield($u_ks);
    return $i0;
  } else {
    return $u_ks;
  }
}

struct Br_interface_kernel *Br_trap_handle_illegal_instruction(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_kernel *$i0;
  if ($p_mtval == Br_trap_OP_URET) {
    $i0 = Br_trap_handle_uret($p_ks);
    return $i0;
  } else {
    return $p_ks;
  }
}

struct Br_interface_kernel *Br_trap_handle_trap(unsigned int $p_mcause, unsigned int $p_mtval, struct Br_interface_kernel *$p_ks)
{
  register int $u_mcause_casted;
  register unsigned int $u_res;
  register struct Br_interface_kernel *$u_ks1;
  register struct Br_interface_kernel *$i0;
  $u_mcause_casted = (int) $p_mcause;
  if ($u_mcause_casted < 0) {
    $u_res = Br_interface_MCAUSE_INTRP($p_mcause);
    if ($u_res == Br_trap_INTRP_MACHINE_TIMER) {
      $u_ks1 = $p_ks;
    } else {
      $u_ks1 = Br_trap_handle_user_intrp($p_mcause, $p_mtval, $p_ks);
    }
  } else {
    if ($p_mcause == Br_trap_EXCPT_ILLEGAL_INSTR) {
      $u_ks1 =
        Br_trap_handle_illegal_instruction
        ($p_mcause, $p_mtval, $p_ks);
    } else {
      if ($p_mcause == Br_trap_EXCPT_ECALL_UMODE) {
        $u_ks1 = Br_ecall_handle_ecall($p_mcause, $p_mtval, $p_ks);
      } else {
        $u_ks1 = Br_trap_handle_user_excpt($p_mcause, $p_mtval, $p_ks);
      }
    }
  }
  $i0 = Br_sched_sched($u_ks1);
  return $i0;
}

struct Br_interface_kernel *Br_kernel_main(struct Br_interface_kernel *$p_ks)
{
  register struct Br_interface_state *$b0;
  register struct Br_interface_state *$b1;
  register struct Br_interface_state *$u_ms;
  register struct Br_interface_kernel *$u_ks;
  $b0 = (*$p_ks).ms;
  $u_ms = Br_interface_csr_write_asm_trap_entry($b0, Br_interface_mtvec);
  (*$p_ks).ms = $u_ms;
  $u_ks = $p_ks;
  $b1 = (*$u_ks).ms;
  $u_ms = Br_interface_csr_write($b1, Br_interface_mstatus, 0);
  (*$u_ks).ms = $u_ms;
  $u_ks = $u_ks;
  $u_ks = Br_sched_sched_init($u_ks);
  return $u_ks;
}


