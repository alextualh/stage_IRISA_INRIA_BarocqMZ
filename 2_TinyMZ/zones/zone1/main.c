#include "altio.h"
#include "uart.h"
#include "libhexfive.h"

int raise_load_access_fault(void)
{
	int a = 0;
	return *((int *)&a + 0x1000);
}

void excpt_0x5_handler(void) __attribute__((interrupt("user")));
void excpt_0x5_handler(void)
{
	alt_puts("In the load access fault handler of zone 1\n");
	// raise_store_access_fault();
	// asm volatile("wfi");
}

void intrp_0x3_handler(void) __attribute__((interrupt("user")));
void intrp_0x3_handler(void)
{
	alt_puts("In the machine software interrupt handler (zone 2)\n");
	asm volatile("wfi");
}

void main(void)
{
	uart_init();

	ECALL_CSRS_MIE();

	ECALL_EXCPT_VECT(0x5, excpt_0x5_handler);
	ECALL_INTRP_VECT(0x3, intrp_0x3_handler);

	// raise_load_access_fault();

	do {
		alt_puts("I'm the zone 1!\n");
		asm volatile("wfi");
	} while (1);
}