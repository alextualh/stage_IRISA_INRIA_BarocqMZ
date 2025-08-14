#include "regs.h"

.extern __global_pointer$

.global asm_trap_entry
.global asm_restore_context

.align 4

asm_trap_entry:
	// Loads regs, stores user a0
	csrrw	a0, mscratch, a0
	beqz 	a0, hang

	REG_S	ra, (REGSIZE * RA)(a0)
	REG_S	sp, (REGSIZE * SP)(a0)
	REG_S	gp, (REGSIZE * GP)(a0)
	REG_S	tp, (REGSIZE * TP)(a0)
	REG_S	t0, (REGSIZE * T0)(a0)
	REG_S	t1, (REGSIZE * T1)(a0)
	REG_S	t2, (REGSIZE * T2)(a0)
	REG_S	s0, (REGSIZE * S0)(a0)
	REG_S	s1, (REGSIZE * S1)(a0)
	REG_S	a1, (REGSIZE * A1)(a0)
	REG_S	a2, (REGSIZE * A2)(a0)
	REG_S	a3, (REGSIZE * A3)(a0)
	REG_S	a4, (REGSIZE * A4)(a0)
	REG_S	a5, (REGSIZE * A5)(a0)
	REG_S	a6, (REGSIZE * A6)(a0)
	REG_S	a7, (REGSIZE * A7)(a0)
	REG_S	s2, (REGSIZE * S2)(a0)
	REG_S	s3, (REGSIZE * S3)(a0)
	REG_S	s4, (REGSIZE * S4)(a0)
	REG_S	s5, (REGSIZE * S5)(a0)
	REG_S	s6, (REGSIZE * S6)(a0)
	REG_S	s7, (REGSIZE * S7)(a0)
	REG_S	s8, (REGSIZE * S8)(a0)
	REG_S	s9, (REGSIZE * S9)(a0)
	REG_S	s10, (REGSIZE * S10)(a0)
	REG_S	s11, (REGSIZE * S11)(a0)
	REG_S	t3, (REGSIZE * T3)(a0)
	REG_S	t4, (REGSIZE * T4)(a0)
	REG_S	t5, (REGSIZE * T5)(a0)
	REG_S	t6, (REGSIZE * T6)(a0)
	csrrw	t0, mscratch, x0
	REG_S	t0, (REGSIZE * A0)(a0)

	// Saves the zone's PC
	csrr	t0, mepc
	REG_S	t0, (REGSIZE * PC)(a0)

	// Loads the kernel gp and sp
	.option push
	.option norelax
	la	gp, __global_pointer$
	.option pop
	la	sp, _sp

	// Gets arguments mcause, mtval
	csrr	a0, mcause
	csrr	a1, mtval
	tail	handle_trap

asm_restore_context:
	mv	s0, a0
	mv	s1, a1
	mv	s2, a2

	// Loads pmpaddr
	REG_L	a0, (REGSIZE * 0)(s2)
	REG_L	a1, (REGSIZE * 1)(s2)
	REG_L	a2, (REGSIZE * 2)(s2)
	REG_L	a3, (REGSIZE * 3)(s2)
	REG_L	a4, (REGSIZE * 4)(s2)
	REG_L	a5, (REGSIZE * 5)(s2)
	REG_L	a6, (REGSIZE * 6)(s2)
	REG_L	a7, (REGSIZE * 7)(s2)
	csrw	pmpaddr0, a0
	csrw	pmpaddr1, a1
	csrw	pmpaddr2, a2
	csrw	pmpaddr3, a3
	csrw	pmpaddr4, a4
	csrw	pmpaddr5, a5
	csrw	pmpaddr6, a6
	csrw	pmpaddr7, a7

	// Loads pmpcfg
	REG_L	a0, 0(s1)
	csrw	pmpcfg0, a0
#if __riscv_xlen == 32
	REG_L	a1, REGSIZE(s1)
	csrw	pmpcfg1, a1
#endif

	// Saves pointer to regs[32]
	csrw	mscratch, s0

	// Loads pc from regs
	REG_L	t0, (REGSIZE * PC)(s0)
	csrw	mepc, t0

	// Loads x1-x31 from regs
	REG_L	ra, (REGSIZE * RA)(s0)
	REG_L	sp, (REGSIZE * SP)(s0)
	REG_L	gp, (REGSIZE * GP)(s0)
	REG_L	tp, (REGSIZE * TP)(s0)
	REG_L	t0, (REGSIZE * T0)(s0)
	REG_L	t1, (REGSIZE * T1)(s0)
	REG_L	t2, (REGSIZE * T2)(s0)
	REG_L	s1, (REGSIZE * S1)(s0)
	REG_L	a0, (REGSIZE * A0)(s0)
	REG_L	a1, (REGSIZE * A1)(s0)
	REG_L	a2, (REGSIZE * A2)(s0)
	REG_L	a3, (REGSIZE * A3)(s0)
	REG_L	a4, (REGSIZE * A4)(s0)
	REG_L	a5, (REGSIZE * A5)(s0)
	REG_L	a6, (REGSIZE * A6)(s0)
	REG_L	a7, (REGSIZE * A7)(s0)
	REG_L	s2, (REGSIZE * S2)(s0)
	REG_L	s3, (REGSIZE * S3)(s0)
	REG_L	s4, (REGSIZE * S4)(s0)
	REG_L	s5, (REGSIZE * S5)(s0)
	REG_L	s6, (REGSIZE * S6)(s0)
	REG_L	s7, (REGSIZE * S7)(s0)
	REG_L	s8, (REGSIZE * S8)(s0)
	REG_L	s9, (REGSIZE * S9)(s0)
	REG_L	s10,(REGSIZE * S10)(s0)
	REG_L	s11,(REGSIZE * S11)(s0)
	REG_L	t3, (REGSIZE * T3)(s0)
	REG_L	t4, (REGSIZE * T4)(s0)
	REG_L	t5, (REGSIZE * T5)(s0)
	REG_L	t6, (REGSIZE * T6)(s0)
	REG_L	s0, (REGSIZE * S0)(s0)
	
.RESTORE_ZONE:
	// Goes to user mode
	mret

hang:	
	j	hang
