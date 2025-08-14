cd "$(dirname "$0")"

gdb -q ../_build/kernel.elf -x "debug.gdb"