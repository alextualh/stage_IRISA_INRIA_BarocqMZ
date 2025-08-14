#include "altio.h"
#include "libhexfive.h"

void intrp_0x3_handler(void) __attribute__((interrupt("user")));
void intrp_0x3_handler(void)
{
	alt_puts("In the machine software interrupt handler (zone 2)\n");
	asm volatile("wfi");
}

static void print_hex32(uint32_t v) {
    const char hex[] = "0123456789ABCDEF";
    for (int shift = 28; shift >= 0; shift -= 4) {
        uint8_t nibble = (v >> shift) & 0xF;
        alt_putc(hex[nibble]);
    }
}

uint32_t vec32_answer[4] = { 0, 0, 0, 0 };
uint64_t vec64_answer[2] = { 0, 0 };

uint32_t vec32_to_send[4] = { 944, 945, 946, 947 };

void main(void)
{
	ECALL_CSRS_MIE();
	ECALL_INTRP_VECT(0x3, intrp_0x3_handler);
	
	int res ;
	do {
		alt_puts("\nI'm the zone 1!\n");
		
		// Communication tests :
		// Received
		res = ECALL_RECV(0, vec32_answer);
        alt_puts("1:sended 944...47:\n");
		if(res == 1){
			alt_puts("1:Received:\n");
            for (int i = 0; i < 4; i++) {
                print_hex32(vec32_answer[i]);
                alt_putc(' ');
            }
            alt_putc('\n');
		}
		// Send
		res = ECALL_SEND(0, vec32_to_send);
		
		asm volatile("wfi");
	} while (1);
}