#include "altio.h"
#include "libhexfive.h"

void intrp_0x3_handler(void) __attribute__((interrupt("user")));
void intrp_0x3_handler(void)
{
	alt_puts("In the machine software interrupt handler (zone 2)\n");
	asm volatile("wfi");
}

void main(void)
{
	ECALL_CSRS_MIE();
	ECALL_INTRP_VECT(0x3, intrp_0x3_handler);

	do {
		alt_puts("I'm the zone 2!\n");
		asm volatile("wfi");
	} while (1);
}