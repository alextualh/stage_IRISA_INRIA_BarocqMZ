#include "altio.h"
#include "uart.h"

void alt_putc(char c)
{
	uart_putc(c);
}

void alt_puts(const char *s)
{
	while (*s != '\0') {
		alt_putc(*s++);
	}
}