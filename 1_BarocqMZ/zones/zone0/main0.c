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
	alt_puts("In the load access fault handler of zone 0\n");
	// raise_store_access_fault();
	// asm volatile("wfi");
}

void intrp_0x3_handler(void) __attribute__((interrupt("user")));
void intrp_0x3_handler(void)
{
	alt_puts("In the machine software interrupt handler (zone ?)\n");
	asm volatile("wfi");
}



// Print for test
static void print_hex32(uint32_t v) {
    const char hex[] = "0123456789ABCDEF";
    for (int shift = 28; shift >= 0; shift -= 4) {
        uint8_t nibble = (v >> shift) & 0xF;
        alt_putc(hex[nibble]);
    }
}

uint32_t vec32[4] = { 0xdeadbeaf, 0xdeadbeaf, 0xdeadbeaf, 0xdeadbeaf };
uint64_t vec64[2] = { 0, 0 };

uint32_t vec32_to_receive[4] = { 0x00000000, 0x00000000, 0x00000000, 0x00000000 };

void main(void)
{
	uart_init();

	ECALL_CSRS_MIE();

	ECALL_EXCPT_VECT(0x5, excpt_0x5_handler);
	ECALL_INTRP_VECT(0x3, intrp_0x3_handler);

	// raise_load_access_fault();

	int res;
	do {
		alt_puts("\nI'm the zone 0!\n");

		// Communication tests :
		// Send
		res = ECALL_SEND(1, vec32);
		if(res == 1){
			alt_puts("0:Sended:\n");
            for (int i = 0; i < 4; i++) {
                print_hex32(vec32[i]);
                alt_putc(' ');
            }
            alt_putc('\n');
		}
		// Received
		res = ECALL_RECV(1, vec32_to_receive);
		if(res == 1){
			alt_puts("0:RECV9:\n");
            for (int i = 0; i < 4; i++) {
				alt_putc('%');
                print_hex32(vec32_to_receive[i]);
                alt_putc(' ');
            }
            alt_putc('\n');
		}
		
		asm volatile("wfi");
	} while (1);
}