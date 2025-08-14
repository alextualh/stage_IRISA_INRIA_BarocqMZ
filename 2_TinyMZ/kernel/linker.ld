OUTPUT_ARCH("riscv")

ENTRY(_start)

MEMORY
{
	itim (rx!rai) : ORIGIN = itim, LENGTH = 8K
	flash (rxai!w) : ORIGIN = flash, LENGTH = 128K
	ram (wxa!ri) : ORIGIN = dtim, LENGTH = 4K
}

SECTIONS
{
        __stack_size = DEFINED(__stack_size) ? __stack_size : 1K;
	PROVIDE(__stack_size = __stack_size);

        .text : {
                *(.text.init)
                *(.text .text.*)
                . = ALIGN(8);
        } >flash AT>flash

        .data : ALIGN(8) {
                *(.data .data.*)
                *(.sdata .sdata.*)
                . = ALIGN(8);
        } >ram AT>flash

	PROVIDE(__global_pointer$ = ADDR(.data));
	PROVIDE(_data_source_start = LOADADDR(.data));
	PROVIDE(_data_target_start = ADDR(.data));
	PROVIDE(_data_target_end = ADDR(.data) + SIZEOF(.data));

	.bss : ALIGN(8) {
		*(.bss .bss.*)
		*(.sbss)
		. = ALIGN(8);
	} >ram AT>ram

        PROVIDE(_bss_start = ADDR(.bss));
	PROVIDE(_bss_end = ADDR(.bss) + SIZEOF(.bss));
	PROVIDE(_heap_start = _bss_end);

        .stack ORIGIN(ram) + LENGTH(ram) - __stack_size : {
		. += __stack_size;
	} >ram AT>ram

        PROVIDE(_sp = ADDR(.stack) + SIZEOF(.stack));
}