target remote :1236
set confirm off
add-symbol-file ../_build/zone1.elf
add-symbol-file ../_build/zone2.elf
b .KERNEL_START
b .ZONE1_START
b .ZONE2_START
b .RESTORE_ZONE
b asm_restore_context
b asm_trap_entry
b handle_trap
layout src