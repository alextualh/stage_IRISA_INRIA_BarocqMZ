#include "kernel.h"
#include "config.h"
#include "csr.h"
#include "sched.h"

kernel_t kernel = { .zones = { ZONE_CONFIG },
		    .intrp_handlers = { INTRP_CONFIG } };

extern void asm_restore_context(uintptr_t[32], uint32_t[2], uintptr_t[8])
	__attribute__((noreturn));

extern void asm_trap_entry(void) __attribute__((noreturn));

void restore_context(void)
{
	asm_restore_context(CURRENT->regs, CURRENT->pmpcfg, CURRENT->pmpaddr);
}

void main(void)
{
	csr_write(mtvec, asm_trap_entry);
	csr_write(mstatus, 0);

	for (int i = 0; i < N_ZONES; i++) {
		ZONES[i].id = i;
	}

	sched_init();

	restore_context();
}