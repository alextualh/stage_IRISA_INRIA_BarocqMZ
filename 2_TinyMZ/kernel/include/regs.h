#ifndef REGS_H
#define REGS_H

#if __riscv_xlen == 32
#define REG_S	sw
#define REG_L	lw
#define REGSIZE 4
#else
#define REG_S	sd
#define REG_L	ld
#define REGSIZE 8
#endif

#define PC  0
#define RA  1
#define SP  2
#define GP  3
#define TP  4
#define T0  5
#define T1  6
#define T2  7
#define FP  8
#define S0  8
#define S1  9
#define A0  10
#define A1  11
#define A2  12
#define A3  13
#define A4  14
#define A5  15
#define A6  16
#define A7  17
#define S2  18
#define S3  19
#define S4  20
#define S5  21
#define S6  22
#define S7  23
#define S8  24
#define S9  25
#define S10 26
#define S11 27
#define T3  28
#define T4  29
#define T5  30
#define T6  31

#endif // REGS_H