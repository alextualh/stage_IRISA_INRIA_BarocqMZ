#include "timer.h"
#include "platform.h"

uint64_t read_mtime(void)
{
#if (__riscv_xlen == 64)
	return CLINT_REG(CLINT_MTIME);

#else
	volatile uint32_t *mtimel =
		(volatile uint32_t *)(CLINT_BASE + CLINT_MTIME);
	volatile uint32_t *mtimeh =
		(volatile uint32_t *)(CLINT_BASE + CLINT_MTIME + 4);

	uint32_t mtimeh_val;
	uint32_t mtimel_val;

	do {
		mtimeh_val = *mtimeh;
		mtimel_val = *mtimel;
	} while (mtimeh_val != *mtimeh);

	return (uint64_t)(((uint64_t)mtimeh_val << 32) | mtimel_val);
#endif
}

void write_mtimecmp(uint64_t val)
{
#if (__riscv_xlen == 64)
	CLINT_REG(CLINT_MTIMECMP) = val;
#else
	volatile uint32_t *mtimecmpl =
		(volatile uint32_t *)(CLINT_BASE + CLINT_MTIMECMP);
	volatile uint32_t *mtimecmph =
		(volatile uint32_t *)(CLINT_BASE + CLINT_MTIMECMP + 4);

	*mtimecmph = 0xFFFFFFFF;
	*mtimecmpl = (uint32_t)(val & 0x0FFFFFFFUL);
	*mtimecmph = (uint32_t)(val >> 32);
#endif
}