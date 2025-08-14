.section .text.init
.global _start
_start:
.option push
.option norelax
	// Global pointer initialization
	la	gp, __global_pointer$
.option pop
	la	sp, _sp

	la	t0, hang
	csrw	mtvec, t0

	// Copies the data section from flash to ram
	la	t0, _data_source_start
	la	t1, _data_target_start
	la	t2, _data_target_end

	beq	t0, t1, 2f
1:	lw	a0, 0(t0)
	sw	a0, 0(t1)
	addi	t0, t0, 4
	addi	t1, t1, 4
	bltu	t1, t2, 1b

2:	// Zero bss segment (i.e. uninitialized data)
	la	t1, _bss_start
	la	t2, _bss_end
	beq	t1, t2, 2f

1: 	sw	x0, 0(t1)
	addi	t1, t1, 4
	bltu	t1, t2, 1b

2:

.KERNEL_START:
	.extern init_kernel_entry	// defined in br_init_kernel.c	
	call init_kernel_entry

 hang:
	j	hang

