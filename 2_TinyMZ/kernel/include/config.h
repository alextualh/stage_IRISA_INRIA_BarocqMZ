#ifndef CONFIG_H
#define CONFIG_H

#include "config_macros.h"

#define N_ZONES 2

#define N_EXCEPTIONS 12

#define N_INTERRUPTS 12

#define QUANTUM 2000

#define ZONE_CONFIG                                                  \
	ZONE_3(0x20040000, NAPOT(RX, 0x20040000, 17),                \
	       NAPOT(RW, 0x80001000, 12), NAPOT(RW, 0x10013000, 12)) \
	ZONE_3(0x20060000, NAPOT(RX, 0x20060000, 17),                \
	       NAPOT(RW, 0x80002000, 12), NAPOT(RW, 0x10013000, 12))

#define INTRP_CONFIG INTRP_DELEG(1, 3)

#endif // CONFIG_H