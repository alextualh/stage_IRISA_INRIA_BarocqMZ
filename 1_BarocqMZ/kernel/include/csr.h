#ifndef CSR_H
#define CSR_H

#define csr_write(reg, val) \
	__asm__ volatile("csrw " #reg ", %0" ::"r"(val) : "memory")
// injection assembleur inline 

#define csr_read(reg, val) __asm__ volatile("csrr %0, " #reg : "=r"(val))

#define csr_set(reg, in) \
	__asm__ volatile("csrs " #reg ", %0" ::"r"(in) : "memory")

#endif // CSR_H