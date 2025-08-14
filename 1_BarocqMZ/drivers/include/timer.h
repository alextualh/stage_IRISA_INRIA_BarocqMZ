#ifndef TIMER_H
#define TIMER_H

#include <stdint.h>

uint64_t read_mtime(void);
void write_mtimecmp(uint64_t val);

#endif // TIMER_H