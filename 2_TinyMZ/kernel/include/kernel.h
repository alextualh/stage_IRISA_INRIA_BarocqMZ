#ifndef KERNEL_H
#define KERNEL_H

#include <stdint.h>
#include "config.h"

#define USTATUS_IE  1
#define USTATUS_PIE (1 << 4)

typedef struct zone {
	uintptr_t id;
	uintptr_t regs[32];
	uint32_t pmpcfg[2];
	uintptr_t pmpaddr[8];
	uintptr_t ustatus;
	uintptr_t uepc;
	uintptr_t uie;
	uintptr_t ucause;
	uintptr_t utval;
	uintptr_t excpt_handlers[N_EXCEPTIONS];
} zone_t;

typedef struct intrp_handler {
	zone_t *zone;
	uintptr_t handler;
} intrp_handler_t;

typedef struct kernel {
	zone_t *current;
	uint64_t deadline;
	zone_t *next;
	uint64_t next_quantum;
	uintptr_t mie;
	uintptr_t intrp_deleg;
	zone_t zones[N_ZONES];
	intrp_handler_t intrp_handlers[N_INTERRUPTS];
} kernel_t;

extern kernel_t kernel;

#define KERNEL	       kernel
#define ZONES	       (kernel.zones)
#define CURRENT	       (kernel.current)
#define INTRP_HANDLERS (kernel.intrp_handlers)

extern void asm_trap_entry(void) __attribute__((noreturn));

void restore_context(void);

#endif // KERNEL_H