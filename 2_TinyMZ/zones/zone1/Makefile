ROOT = ../..

include $(ROOT)/Makefile.config

include $(BSP_DIR)/settings.mk

TARGET_ELF = $(BUILD_DIR)/zone1.elf
TARGET_DUMP = $(DUMP_DIR)/zone1.dump

SOURCE_DIRS = $(BSP_DIR) $(HEXFIVE_DIR) $(DRIVERS_DIR)/src $(ALTC_DIR) .
INCLUDES = $(BSP_DIR) $(HEXFIVE_DIR) $(DRIVERS_DIR)/include $(ALTC_DIR)

HEADERS = $(wildcard $(addsuffix /*.h, $(INCLUDES)))
SOURCES = $(wildcard $(addsuffix /*.c, $(SOURCE_DIRS)) $(addsuffix /*.S, $(SOURCE_DIRS)))

LINKER_SCRIPTS = $(BSP_DIR)/memory.ld linker.ld

CFLAGS = -march=$(ARCH) -mabi=$(ABI) -mcmodel=$(CMODEL)
CFLAGS += -g

CFLAGS += $(addprefix -I , $(INCLUDES)) -nostartfiles
CFLAGS += $(addprefix -T, $(LINKER_SCRIPTS))
CFLAGS += -Wall -Wno-main

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