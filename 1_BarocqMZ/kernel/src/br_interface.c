// what's depending on it :
#include "br_interface.h"

// this file depends on :
#include "../../_build/br_kernel.h"
#include "../../drivers/include/timer.h"
#include "../include/csr.h"
#include "../include/config.h"
#include "../../bsp/fe310/platform.h"

unsigned long long Br_interface_QUANTUM_CYC = INTERFACE_QUANTUM_CYC;

unsigned int Br_interface_N_ZONES = INTERFACE_N_ZONES;

long long unsigned int Br_interface_read_mtime(struct Br_interface_state* ms){
    return read_mtime();
}

struct Br_interface_state* Br_interface_write_mtimecmp(struct Br_interface_state* ms, long long unsigned int val){
    write_mtimecmp(val);
    return ms;
}

#define CSR_MIE 0x304   // mandatory to write it in hard, value finded in : 
// https://docs.openhwgroup.org/projects/cv32e40s-user-manual/en/latest/control_status_registers.html


extern void asm_trap_entry(void) __attribute__((noreturn));


struct Br_interface_state* Br_interface_csr_write(struct Br_interface_state* ms, int reg, unsigned int val){
    if(reg == 0x304) //MIE
        csr_write(0x304, val);

    if(reg == 0x305) //mtvec
        csr_write(0x305, val);

    if(reg == 0x300) //mstatus
        csr_write(0x300, val);

    return ms;
}


struct Br_interface_state* Br_interface_csr_write_asm_trap_entry(struct Br_interface_state* ms, int reg)
{ 
    if(reg == 0x304) //MIE
        csr_write(0x304, asm_trap_entry);

    if(reg == 0x305) //mtvec
        csr_write(0x305, asm_trap_entry);

    if(reg == 0x300) //mstatus
        csr_write(0x300, asm_trap_entry);

    return ms;
}


extern void asm_restore_context(uintptr_t[32], uint32_t[2], uintptr_t[8])
	__attribute__((noreturn));


void asm_restore_context_from_Br_kernel(struct Br_interface_kernel* ks){
    struct Br_interface_zone* zone = ks->zones[ks->current];
    unsigned int * regs = (unsigned int *)zone->regs;
    unsigned long int* pmpcfg = (unsigned long int*)zone->pmpcfg;
    unsigned int * pmpaddr = (unsigned int *)zone->pmpaddr;

    unsigned int* new_regs    = &(regs[0]);
    unsigned int* new_pmpaddr = &(pmpaddr[0]);

    asm_restore_context(new_regs, pmpcfg, new_pmpaddr);
    return;
}


unsigned int Br_interface_MCAUSE_INTRP(unsigned int mcause){
    return ((mcause << 1) >> 1);
}


unsigned int Br_interface_binary_or(unsigned int a, unsigned int b){
    return a | b;
}