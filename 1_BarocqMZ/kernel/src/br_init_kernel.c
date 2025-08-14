#include "../../_build/br_kernel.h"
#include "../include/config.h"
#include "br_interface.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

// initialisation of kernel and zones data, casted from tinyMZ initialisation (based on macro)
// and provide the 2 entry functions called by assembly code and that are calling barocq functions


//============================== Types as in TinyMZ ===============================

typedef struct zone {
	uintptr_t id;
    unsigned long regs[32];

	uint32_t pmpcfg[2];
	uintptr_t pmpaddr[8];
	uintptr_t ustatus;
	uintptr_t uepc;
	uintptr_t uie;
	uintptr_t ucause;
	uintptr_t utval;
	uintptr_t excpt_handlers[N_EXCEPTIONS];
    uintptr_t intrp_deleg;
} zone_t;
typedef struct intrp_handler {
	zone_t *zone;
	uintptr_t handler;
} intrp_handler_t;
struct inbox {
    // if full != 0, then we have a message, else no message.
    u32 full;
    // 128-bit message
    u32 msgs[4];
};
typedef struct kernel {
	zone_t *current;
	uint64_t deadline;
	zone_t *next;
	uint64_t next_quantum;
	uintptr_t mie;
	zone_t zones[N_ZONES];
	intrp_handler_t intrp_handlers[N_INTERRUPTS];
    struct inbox inboxes[N_ZONES*N_ZONES]; // linearised matrix
} kernel_t;


//=========================== MEMORY INITIALIZATION ===================================

// same defn as in TinyMZ : going to be casted in a "Br_interface_kernel" later in this file
kernel_t kernel = { .zones = { ZONE_CONFIG }, 
            .intrp_handlers = { INTRP_CONFIG } };

struct Br_interface_kernel br_kernel = { 
    .current = 0,
    .deadline = 0,
    .next = 1,
    .next_quantum = 0,
    .mie = 0,
    .zones = NULL,
    .intrp_handlers = NULL,
    .inboxes = NULL,
};

// allocating the array that are going to be filled and assignated as field of "Br_interface_kernel"
struct Br_interface_zone br_zones[N_ZONES];
struct Br_interface_zone *br_zones_ptrs[N_ZONES];
struct Br_interface_intrp_handler br_handlers[N_INTERRUPTS];
struct Br_interface_intrp_handler *br_handlers_ptrs[N_INTERRUPTS];
struct Br_interface_inbox br_inboxes[N_ZONES*N_ZONES];
struct Br_interface_inbox *br_inboxes_ptrs[N_ZONES*N_ZONES];

// type convertion FUNCTIONS, defined below, used in init_kernel_entry()
void convert_zones_to_interface(zone_t zones[], struct Br_interface_zone br_zones[]);
void convert_intrp_handlers_to_interface(intrp_handler_t handlers[], struct Br_interface_intrp_handler br_handlers[], size_t num_handlers);
void convert_kernel_to_interface(kernel_t *kernel, struct Br_interface_kernel *br_kernel, struct Br_interface_zone br_zones[], struct Br_interface_zone *br_zones_ptrs[], 
        struct Br_interface_intrp_handler br_handlers[], struct Br_interface_intrp_handler *br_handlers_ptrs[],
        struct Br_interface_inbox br_inboxes[], struct Br_interface_inbox *br_inboxes_ptrs[]
    );


//================================================ MAIN =================================================   
    // MAIN functions "int_kernel_entry" and "handle_trap", called by assembly code :

// Assembly goes here firtly
void init_kernel_entry() {
    // Initialization of zone ids
    for (int i = 0; i < N_ZONES; i++) {
		kernel.zones[i].id = i;
	}

    // Construction cast
    convert_kernel_to_interface(&kernel, &br_kernel, br_zones, br_zones_ptrs, br_handlers, br_handlers_ptrs, br_inboxes, br_inboxes_ptrs);

    Br_kernel_main(&br_kernel);
    asm_restore_context_from_Br_kernel(&br_kernel);
}

// here we've switched to ZONE X
    // ...zone x doing its job...
// when ZONE end, trap.S call this :

// Assembly goes there from trap.S after end of a zone and save of its registers
void handle_trap(uintptr_t mcause, uintptr_t mtval){
    // Using the Barocq function as a lib service : 
    Br_trap_handle_trap(mcause, mtval, &br_kernel); 

    // But handle_trap calls sched(ks) but sched is now without asm restore (not as in tinyMZ)
    asm_restore_context_from_Br_kernel(&br_kernel);
}


//================================================ CAST =================================================  
    // CAST functions x3 :

void convert_zones_to_interface(zone_t zones[], struct Br_interface_zone br_zones[]) {
    for (size_t i = 0; i < N_ZONES; i++) {
        br_zones[i].id = (unsigned int) zones[i].id;
        br_zones[i].regs = (unsigned int *) zones[i].regs;   
        br_zones[i].pmpcfg = (unsigned int *)zones[i].pmpcfg;
        br_zones[i].pmpaddr = (unsigned int *) zones[i].pmpaddr;
        br_zones[i].ustatus = (unsigned int) zones[i].ustatus;
        br_zones[i].uepc = (unsigned int) zones[i].uepc;
        br_zones[i].uie = (unsigned int) zones[i].uie;
        br_zones[i].ucause = (unsigned int) zones[i].ucause;
        br_zones[i].utval = (unsigned int) zones[i].utval;
        br_zones[i].excpt_handlers = (unsigned int *) zones[i].excpt_handlers;
        br_zones[i].intrp_deleg = (unsigned int) 0;
    }
}

void convert_intrp_handlers_to_interface(intrp_handler_t handlers[], struct Br_interface_intrp_handler br_handlers[], size_t num_handlers) {
    for (size_t i = 0; i < num_handlers; i++) {
        br_handlers[i].zone = 0;
        //don't access current and next cuz the ptr havn't been init so read in 0x0 then seg fault
        br_handlers[i].handler = (unsigned int)(handlers[i].handler);
    }
}

// main CAST function : for a given kernel and br_kernel it copies kernel in br_kernel and cast everything as needed
// takes the already reserved memory area (br_[thing]) declared before where we want (e.g. global in .data)
void convert_kernel_to_interface(kernel_t *kernel, struct Br_interface_kernel *br_kernel, 
        struct Br_interface_zone br_zones[], struct Br_interface_zone *br_zones_ptrs[], 
        struct Br_interface_intrp_handler br_handlers[], struct Br_interface_intrp_handler *br_handlers_ptrs[],
        struct Br_interface_inbox br_inboxes[], struct Br_interface_inbox *br_inboxes_ptrs[])
{
    //simple fields
    br_kernel->deadline = kernel->deadline;
    br_kernel->next_quantum = kernel->next_quantum;
    br_kernel->mie = kernel->mie;

    // convert zones -> br_zones
    convert_zones_to_interface(kernel->zones, br_zones);
    for (size_t i = 0; i < N_ZONES; i++) {
        br_zones_ptrs[i] = &br_zones[i];
    }
    br_kernel->zones = br_zones_ptrs;

    // convert intrp_handlers -> br_intrp_handlers
    convert_intrp_handlers_to_interface(kernel->intrp_handlers, br_handlers, N_INTERRUPTS);
    for (size_t i = 0; i < N_INTERRUPTS; i++) {
        br_handlers_ptrs[i] = &br_handlers[i];
    }
    br_kernel->intrp_handlers = br_handlers_ptrs;

    // convert inboxes
    size_t index;
    for (size_t i = 0; i < N_ZONES; i++) {
        for (size_t j = 0; j < N_ZONES; j++) {
            index = i * N_ZONES + j;
            br_inboxes[index].full = (unsigned int)kernel->inboxes[index].full;
            br_inboxes[index].msgs = (unsigned int *)kernel->inboxes[index].msgs;

            br_inboxes_ptrs[index] = &br_inboxes[index];
        }
    }
    br_kernel->inboxes = br_inboxes_ptrs;
}