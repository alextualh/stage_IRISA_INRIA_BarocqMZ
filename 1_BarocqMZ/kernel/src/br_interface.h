#ifndef BR_INTERFACE_H
#define BR_INTERFACE_H
// INTERFACE File : objects declared and use in barocq but defined in C : 

// Constants :
#define INTERFACE_QUANTUM_CYC ((BR_RTC_FREQ) * (QUANTUM) / 1000)
#define INTERFACE_N_ZONES 2
extern unsigned long long Br_interface_QUANTUM_CYC; /* time value, stay 64 bits */
extern unsigned int Br_interface_N_ZONES;


// Types :
struct Br_interface_state{
    int dummy_data;
};


// Functions declared and used in BAROCQ but implemented in C :
struct Br_interface_kernel; // mandatory to use it below in functions

void asm_restore_context_from_Br_kernel(struct Br_interface_kernel* ks);

long long unsigned int Br_interface_read_mtime(struct Br_interface_state* ms);

struct Br_interface_state* Br_interface_write_mtimecmp(struct Br_interface_state*, long long unsigned int val);

unsigned int Br_interface_MCAUSE_INTRP(unsigned int mcause);

unsigned int Br_interface_binary_or(unsigned int a, unsigned int b);

#endif