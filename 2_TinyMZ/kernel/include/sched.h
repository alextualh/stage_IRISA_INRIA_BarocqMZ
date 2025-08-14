#ifndef SCHED_H
#define SCHED_H

#include "kernel.h"

void sched_init(void);
void sched_yield(void);
void sched_intrp(zone_t *zone);
void sched(void);

#endif // SCHED_H