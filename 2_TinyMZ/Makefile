all: kernel zone1 zone2

SOURCE_DIRS=kernel zones/zone1 zones/zone2

kernel:
	$(MAKE) -C kernel -f Makefile

zone1:
	$(MAKE) -C zones/zone1 -f Makefile

zone2:
	$(MAKE) -C zones/zone2 -f Makefile

format:
	clang-format -i $$(find . -type f -name '*.[hc]')

dump: kernel zone1 zone2
	@for dir in $(SOURCE_DIRS) ; do \
		$(MAKE) -C $$dir -f Makefile dump; \
	done

clean:
	@for dir in $(SOURCE_DIRS) ; do \
		$(MAKE) -C $$dir -f Makefile clean ; \
	done

.PHONY: kernel zone1 zone2 clean dump