#include "trap.h"
#include "config.h"
#include "sched.h"
#include "timer.h"
#include "kernel.h"

void handle_trap(uintptr_t mcause, uintptr_t mtval)
{
	if ((int32_t)mcause < 0) {
		switch (MCAUSE_INTRP(mcause)) {
		case INTRP_MACHINE_TIMER:
			break;
		default:
			handle_user_intrp(mcause, mtval);
			break;
		}
	} else {
		switch (mcause) {
		case EXCPT_ILLEGAL_INSTR:
			handle_illegal_instruction(mcause, mtval);
			break;
		case EXCPT_ECALL_UMODE:
			handle_ecall(mcause, mtval);
			break;
		case EXCPT_ECALL_SMODE:
		case EXCPT_ECALL_MMODE:
			break;
		default:
			handle_user_excpt(mcause, mtval);
		}
	}
	sched();
}

void handle_user_intrp(uintptr_t mcause, uintptr_t mtval)
{
	intrp_handler_t intrp_handler = INTRP_HANDLERS[MCAUSE_INTRP(mcause)];
	zone_t *zone = intrp_handler.zone;
	uintptr_t handler = intrp_handler.handler;

	if (handler) {
		zone->uepc = zone->regs[PC];
		zone->ucause = mcause;
		zone->utval = mtval;
		zone->regs[PC] = handler;

		zone->ustatus = USTATUS_PIE;
		KERNEL.mie &= ~(zone->uie);

		KERNEL.intrp_deleg = 1;

		if (zone != CURRENT) {
			sched_intrp(zone);
		}
	}
}

void handle_user_excpt(uintptr_t mcause, uintptr_t mtval)
{
	zone_t *zone = CURRENT;
	uintptr_t handler = zone->excpt_handlers[mcause];

	if (handler) {
		zone->uepc = zone->regs[PC];
		zone->ucause = mcause;
		zone->utval = mtval;
		zone->regs[PC] = handler;

		if (zone->ustatus & USTATUS_IE) {
			zone->ustatus = USTATUS_PIE;
			KERNEL.mie &= ~(zone->uie);
		}
	}
}

void static inline handle_uret(void)
{
	zone_t *zone = CURRENT;

	zone->regs[PC] = zone->uepc;
	zone->ucause = 0;
	zone->uepc = 0;
	zone->utval = 0;

	if (zone->ustatus & USTATUS_PIE) {
		zone->ustatus |= USTATUS_IE;
		KERNEL.mie |= zone->uie;
	}

	if (KERNEL.intrp_deleg) {
		KERNEL.deadline = read_mtime();
		sched_yield();
	}
}

void handle_illegal_instruction(uintptr_t mcause, uintptr_t mtval)
{
	if (mtval == OP_URET) {
		handle_uret();
	}
}