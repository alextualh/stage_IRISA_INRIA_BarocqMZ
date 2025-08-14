#include "kernel.h"
#include "regs.h"
#include "csr.h"

#define N_ECALLS 4

typedef enum ecall {
	ECL_EXCPT_VECT = 0,
	ECL_INTRP_VECT,
	ECL_CSRS_MIE,
	ECL_CSRC_MIE
} ecall_t;

typedef void (*handler_t)(zone_t *caller);

static void ecall_excpt_vect(zone_t *caller)
{
	uintptr_t code = caller->regs[A0];
	uintptr_t handler = caller->regs[A1];

	if (code < N_EXCEPTIONS) {
		caller->excpt_handlers[code] = handler;
	}
}

static void ecall_intrp_vect(zone_t *caller)
{
	uintptr_t code = caller->regs[A0];
	uintptr_t handler = caller->regs[A1];

	if (code < N_INTERRUPTS && INTRP_HANDLERS[code].zone == caller) {
		INTRP_HANDLERS[code].handler = handler;
		caller->uie |= (1 << code);

		if (caller->ustatus & USTATUS_IE) {
			KERNEL.mie |= (1 << code);
		}
	}
}

static void ecall_csrs_mie(zone_t *caller)
{
	caller->ustatus |= USTATUS_IE;
	KERNEL.mie |= caller->uie;
}

static void ecall_csrc_mie(zone_t *caller)
{
	caller->ustatus &= ~USTATUS_IE;
	KERNEL.mie &= ~(caller->uie);
}

static const handler_t ecall_handlers[] = { [ECL_EXCPT_VECT] = ecall_excpt_vect,
					    [ECL_INTRP_VECT] = ecall_intrp_vect,
					    [ECL_CSRS_MIE] = ecall_csrs_mie,
					    [ECL_CSRC_MIE] = ecall_csrc_mie };

void handle_ecall(uintptr_t mcause, uintptr_t mtval)
{
	zone_t *caller = CURRENT;
	caller->regs[PC] += 4;
	ecall_t ecall_id = caller->regs[A7];

	if (ecall_id < N_ECALLS) {
		(ecall_handlers[ecall_id])(caller);
	}
}