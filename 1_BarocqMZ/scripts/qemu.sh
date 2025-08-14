#!/bin/bash

cd "$(dirname "$0")"

YELLOW='\033[0;33m'
NOCOLOR='\033[0m'

print_info () {
	printf ${YELLOW}"$1${NOCOLOR}\n"
}

DEBUG=""

if [ "$1" == "debug" ]
then
	print_info "QEMU started in debug mode"
	DEBUG="-gdb tcp::1236 -S"
else
	print_info "QEMU started"
fi

print_info "Press C-a+x to quit"

qemu-system-riscv32 \
	-nographic \
	-bios none \
	-machine sifive_e,revb=true \
	-device loader,file=../_build/kernel.elf \
	-device loader,file=../_build/zone0.elf \
	-device loader,file=../_build/zone1.elf \
	$DEBUG