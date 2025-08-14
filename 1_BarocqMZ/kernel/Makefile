ROOT = ..

include $(ROOT)/Makefile.config

include $(BSP_DIR)/settings.mk

TARGET_ELF = $(BUILD_DIR)/kernel.elf
TARGET_DUMP = $(DUMP_DIR)/kernel.dump

SOURCE_DIRS = $(BSP_DIR) $(DRIVERS_DIR)/src src
INCLUDES = $(BSP_DIR) $(DRIVERS_DIR)/include include

HEADERS = $(wildcard $(addsuffix /*.h, $(INCLUDES)))
SOURCES_BAROCQ = src/br_interface.br src/br_sched.br src/br_ecall.br src/br_trap.br src/br_kernel.br
# cannot be a wildcard because the order is important

LINKER_SCRIPTS = linker.ld
# $(BSP_DIR)/memory.ld --> no need because already included in linker.ld


#________________________________________________________#________________________________________________________
#________________________________________________________#________________________________________________________
COMP = ccomp

ifeq ($(COMP),gcc)
	CC = riscv64-unknown-elf-gcc
	CFLAGS = -march=$(ARCH) -mabi=$(ABI) -mcmodel=$(CMODEL)
	CFLAGS += -g
	CFLAGS += $(addprefix -I , $(INCLUDES)) -nostdlib
	CFLAGS += $(addprefix -T, $(LINKER_SCRIPTS))
	CLFAGS += -Wall

else ifeq ($(COMP),ccomp)
  CC      = ccomp
  CFLAGS  = $(addprefix -I ,$(INCLUDES)) -nostdlib -finline-asm -v
  # ccomp don't manage -T for ld64, we give a extern linker
  CFLAGS +=  $(addprefix -T ,$(LINKER_SCRIPTS))
  CFLAGS += -g
#- linker=riscv64-unknown-elf-ld no need already know
endif

all : mk_build_dir gen_barocq $(TARGET_ELF)

gcc:
	$(MAKE) COMP=gcc all 
#________________________________________________________#________________________________________________________
#________________________________________________________#________________________________________________________

mk_build_dir :
	@mkdir -p $(BUILD_DIR)

gen_barocq : $(SOURCES_BAROCQ)
	barocq -o $(BUILD_DIR)/br_kernel.c -target-arch ptr32 -gen-header  -types-impl ../kernel/src/br_interface.h $(SOURCES_BAROCQ)

SOURCES_C_ASM = $(wildcard $(addsuffix /*.c, $(SOURCE_DIRS)) $(addsuffix /*.S, $(SOURCE_DIRS)) $(addsuffix /*.c, $(BUILD_DIR)))

$(TARGET_ELF): $(SOURCES_C_ASM) $(HEADERS) $(LINKER_SCRIPTS)
	$(CC) $(CFLAGS) -o $@ $(SOURCES_C_ASM)

clean :
	rm -f $(BUILD_DIR)/kernel.elf $(BUILD_DIR)/*.o $(BUILD_DIR)/*.c $(BUILD_DIR)/*.h