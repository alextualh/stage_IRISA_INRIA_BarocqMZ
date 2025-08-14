#include "uart.h"
#include "platform.h"

void uart_init(void) //replace uart by Uart to respect br module name
{
	UART_REG(UART_TXCTRL) = 0x1UL;
	UART_REG(UART_RXCTRL) = 0x1UL;
}

void uart_putc(char c) //replace uart by Uart to respect br module name
{
	while (UART_REG(UART_TXFIFO) < 0)
		;

	UART_REG(UART_TXFIFO) = c;
}