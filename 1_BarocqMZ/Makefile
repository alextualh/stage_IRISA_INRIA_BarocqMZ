
all : kernel_compcert 	zone0 zone1

gcc : kernel_gcc 		zone0 zone1
# if this rule is first line of this file it will be exec in every cases

SOURCE_DIRS = kernel zones/zone0 zones/zone1

kernel_gcc:
	$(MAKE) gcc -C kernel -f Makefile

kernel_compcert:
	$(MAKE) -C kernel -f Makefile

# -C kernel = cd kernel
# -f is an option of $(MAKE) to say the Makefile is name "Makefile" (so useless)

zone0:
	$(MAKE) -C zones/zone0 -f Makefile

zone1:
	$(MAKE) -C zones/zone1 -f Makefile

format:
	clang-format -i $$(find . -type f -name '*.[hc]')

dump: kernel zone0 zone1
	@for dir in $(SOURCE_DIRS) ; do \
		$(MAKE) -C $$dir -f Makefile dump; \
	done

clean:
	@for dir in $(SOURCE_DIRS) ; do \
		$(MAKE) -C $$dir -f Makefile clean ; \
	done

.PHONY: kernel zone0  clean dump