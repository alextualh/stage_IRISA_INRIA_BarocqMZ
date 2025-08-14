ROOT = ..

include $(ROOT)/Makefile.config

include $(BSP_DIR)/settings.mk

TARGET_ELF = $(BUILD_DIR)/kernel.elf
TARGET_DUMP = $(DUMP_DIR)/kernel.dump

SOURCE_DIRS = $(BSP_DIR) $(DRIVERS_DIR)/src src
INCLUDES = $(BSP_DIR) $(DRIVERS_DIR)/include include

HEADERS = $(wildcard $(addsuffix /*.h, $(INCLUDES)))
SOURCES = $(wildcard $(addsuffix /*.c, $(SOURCE_DIRS)) $(addsuffix /*.S, $(SOURCE_DIRS)))


LINKER_SCRIPTS = $(BSP_DIR)/memory.ld linker.ld

CFLAGS = -march=$(ARCH) -mabi=$(ABI) -mcmodel=$(CMODEL)
CFLAGS += -g

CFLAGS += $(addprefix -I , $(INCLUDES)) -nostdlib
CFLAGS += $(addprefix -T, $(LINKER_SCRIPTS))
CLFAGS += -Wall

$(TARGET_ELF): $(SOURCES) $(HEADERS) $(LINKER_SCRIPTS)
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $@ $(SOURCES)

dump: $(TARGET_DUMP)

$(TARGET_DUMP): $(TARGET_ELF)
	@mkdir -p $(DUMP_DIR)
	$(OBJDUMP) -D $< > $@

clean:
	rm -f $(TARGET_ELF)
	rm -f $(TARGET_DUMP)

.PHONY: clean dump