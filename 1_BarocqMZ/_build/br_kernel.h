#pragma once

#include "../kernel/src/br_interface.h"

typedef int i32;
typedef unsigned int u32;
typedef long long i64;
typedef unsigned long long u64;

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

extern unsigned long long Br_interface_QUANTUM_CYC;

extern unsigned int Br_interface_N_ZONES;

extern unsigned int const Br_interface_USTATUS_IE;

extern unsigned int const Br_interface_USTATUS_PIE;

extern unsigned int const Br_interface_MIE_TIMER;

extern int const Br_interface_mie;

extern int const Br_interface_mtvec;

extern int const Br_interface_mstatus;

extern unsigned int const Br_interface_PC;

extern unsigned int const Br_ecall_N_ECALLS;

extern unsigned int const Br_ecall_ECL_EXCPT_VECT;

extern unsigned int const Br_ecall_ECL_INTRP_VECT;

extern unsigned int const Br_ecall_ECL_CSRS_MIE;

extern unsigned int const Br_ecall_ECL_CSRC_MIE;

extern unsigned int const Br_ecall_ECL_SEND;

extern unsigned int const Br_ecall_ECL_RECV;

extern unsigned int const Br_ecall_A0;

extern unsigned int const Br_ecall_A1;

extern unsigned int const Br_ecall_A2;

extern unsigned int const Br_ecall_A3;

extern unsigned int const Br_ecall_A4;

extern unsigned int const Br_ecall_A5;

extern unsigned int const Br_ecall_A7;

extern unsigned int const Br_ecall_N_EXCEPTIONS;

extern unsigned int const Br_ecall_N_INTERRUPTS;

extern unsigned int const Br_trap_EXCPT_ILLEGAL_INSTR;

extern unsigned int const Br_trap_EXCPT_ECALL_UMODE;

extern unsigned int const Br_trap_EXCPT_ECALL_SMODE;

extern unsigned int const Br_trap_EXCPT_ECALL_MMODE;

extern unsigned int const Br_trap_INTRP_MACHINE_TIMER;

extern unsigned int const Br_trap_OP_URET;

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

