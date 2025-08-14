.section .text.libhexfive

.global ECALL_EXCPT_VECT
.global ECALL_INTRP_VECT
.global ECALL_CSRC_MIE
.global ECALL_CSRS_MIE

ECALL_EXCPT_VECT:
	li	a7, 0
	ecall
	ret

ECALL_INTRP_VECT:
	li	a7, 1
	ecall
	ret
	
ECALL_CSRS_MIE:
	li	a7, 2
	ecall
	ret

ECALL_CSRC_MIE:
	li	a7, 3
	ecall
	ret
