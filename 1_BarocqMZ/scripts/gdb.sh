#!/bin/bash

# cd "$(dirname "$0")"
# gdb -q ../_build/kernel.elf -x "debug.gdb"*

cd "$(dirname "$0")"
riscv64-unknown-elf-gdb -q ../_build/kernel.elf -x "debug.gdb"
# gdb -q ../_build/kernel.elf -x "debug.gdb"