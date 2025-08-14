#ifndef PLATFORM_H
#define PLATFORM_H

#include <stdint.h>

// #ifdef RTC_FREQ
// #warning "RTC_FREQ is already defined ========= Alex"
// #endif

#define BR_RTC_FREQ 32768

// #define ALEX_RTC_FREQ 32768

//ALEX : !!! : name changed to RV_ as a prefix (risc V) against weird cc error
// car il faut pas compiler avec cc mais avec riscv64-unknown-elf-gcc

// ----------------------------------------
// RTC (CLINT)
// ----------------------------------------

#define CLINT_BASE 0x02000000

#define CLINT_MSIP     0x0000
#define CLINT_MTIMECMP 0x4000
#define CLINT_MTIME    0xBFF8

// ----------------------------------------
// UART0
// ----------------------------------------

#define UART_BASE 0x10013000

#define UART_TXFIFO 0x00
#define UART_RXFIFO 0x04
#define UART_TXCTRL 0x08
#define UART_RXCTRL 0x0C
#define UART_IE	    0x10
#define UART_IP	    0x14
#define UART_DIV    0x18

// ----------------------------------------
// C helper functions
// ----------------------------------------

#define _REG64(base, offset) (*(volatile uint64_t *)((base) + (offset)))
#define _REG32(base, offset) (*(volatile uint32_t *)((base) + (offset)))

#define CLINT_REG(offset) _REG64(CLINT_BASE, (offset))
#define UART_REG(offset)  _REG32(UART_BASE, (offset))

#endif // PLATFORM_H