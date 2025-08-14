.section .text.libhexfive

.global ECALL_EXCPT_VECT
.global ECALL_INTRP_VECT
.global ECALL_CSRC_MIE
.global ECALL_CSRS_MIE
.global ECALL_SEND
.global ECALL_RECV

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

        
# __________ added for COMMUNICATION __________

ECALL_SEND:
#if __riscv_xlen == 32 
        lw      a4, 12(a1)      # load msg[3]
        lw      a3, 8(a1)       # load msg[2]
        lw      a2, 4(a1)       # load msg[1]
        lw      a1, 0(a1)       # load msg[0]
#else
        ld      a2, 8(a1)       # load msg[2-3]
        ld      a1, 0(a1)       # load msg[0-1]
#endif
        li      a7, 4           # sys call #2
        ecall
        ret

ECALL_RECV:
        mv      a6, a1          # save pointer
        li      a7, 5           # sys call #3
        ecall
        beqz    a0, 1f          # status == 0, go to ret
#if __riscv_xlen == 32 
        sw      a4, 12(a6)      # save msg[3]
        sw      a3, 8(a6)       # save msg[2]
        sw      a2, 4(a6)       # save msg[1]
        sw      a1, 0(a6)       # save msg[0]
#else
        sd      a2, 8(a6)       # save msg[2-3]
        sd      a1, 0(a6)       # save msg[0-1]
#endif
1:      ret
