#ifndef TRAP_H
#define TRAP_H

#include <stdint.h>

#define MCAUSE_INTRP(mcause) ((mcause << 1) >> 1)

#define EXCPT_ILLEGAL_INSTR 0x2
#define EXCPT_ECALL_UMODE   0x8
#define EXCPT_ECALL_SMODE   0x9
#define EXCPT_ECALL_MMODE   0x11

#define INTRP_MACHINE_TIMER 0x7

#define OP_URET 0x200073

void handle_trap(uintptr_t mcause, uintptr_t mtval);
void handle_user_intrp(uintptr_t mcause, uintptr_t mtval);
void handle_user_excpt(uintptr_t mcause, uintptr_t mtval);
void handle_illegal_instruction(uintptr_t mcause, uintptr_t mtval);
void handle_ecall(uintptr_t mcause, uintptr_t mtval);

#endif // TRAP_H