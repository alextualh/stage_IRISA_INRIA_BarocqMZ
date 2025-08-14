#include "sched.h"
#include "csr.h"
#include "platform.h"
#include "timer.h"

#define QUANTUM_CYC (RTC_FREQ * QUANTUM / 1000)

#define MIE_TIMER (1 << 7)

void sched_init(void)
{
	KERNEL.intrp_deleg = 0;

	CURRENT = &ZONES[0];
	KERNEL.deadline = read_mtime() + QUANTUM_CYC;

	uintptr_t nextId = (CURRENT->id + 1) % N_ZONES;
	KERNEL.next = &ZONES[nextId];
	KERNEL.next_quantum = QUANTUM_CYC;

	KERNEL.mie = MIE_TIMER;

	write_mtimecmp(KERNEL.deadline);
	csr_write(mie, KERNEL.mie);
}

void sched_yield(void)
{
	KERNEL.intrp_deleg = 0; // REMOVED IN BAROCQ MZ, put in handle_uret AND in the zone struct as field

	CURRENT = KERNEL.next;
	KERNEL.deadline += KERNEL.next_quantum;

	uintptr_t nextId = (CURRENT->id + 1) % N_ZONES;
	KERNEL.next = &ZONES[nextId];
	KERNEL.next_quantum = QUANTUM_CYC;

	write_mtimecmp(KERNEL.deadline);
}

void sched_intrp(zone_t *zone)
{
	KERNEL.next = CURRENT;
	CURRENT = zone;

	uint64_t now = read_mtime();

	if (now < KERNEL.deadline) {
		KERNEL.next_quantum = KERNEL.deadline - now;
	} else {
		KERNEL.next_quantum = 0;
	}

	KERNEL.deadline = read_mtime() + QUANTUM_CYC;

	write_mtimecmp(KERNEL.deadline);
}

void sched(void)
{
	if (KERNEL.deadline <= read_mtime()) {
		sched_yield();
		// intrp_deleg <- 0 in sched_yield(), in EVERY cases
	}

	if (KERNEL.intrp_deleg) {
	//absurde car intrp_deleg forcément 0 car set à 0 dans sched_yield si on est entré dedans
	//il faut mettre à 0 pour la prochaine zone mais le laisser à 1 pour la current qu'on reprendra plus tard
	// et même si jamais la nouvelle était à 1 pourquoi pas recharger comme ça ?
	
		csr_write(mie, MIE_TIMER); //si intrp en cours d'exec dans zone : On limite les interruptions autorisées uniquement au timer (MIE_TIMER).
		// Cela protège le handler utilisateur de toute autre interruption concurrente, tout en permettant l’interruption de quantum (le timer) pour éventuellement reprendre la main.

	} else {
		csr_write(mie, KERNEL.mie);
		//→ Sinon, si aucune interruption n’est en cours de traitement par une zone :
		//On restaure la configuration complète des interruptions depuis le noyau (KERNEL.mie), ce qui peut réactiver les interruptions externes, logicielles, etc., en fonction des besoins du système.

	}

	restore_context();
}