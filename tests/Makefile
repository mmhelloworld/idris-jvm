INTERACTIVE ?= --interactive
threads ?= $(shell (nproc || sysctl -n hw.ncpu) 2>/dev/null || echo 1)

ifeq ($(OS), windows)
	# This produces D:/../.. style paths
	RUN_TESTS_EXEC_PATH := ${CURDIR}/build/exec/runtests.bat
	RUN_TESTS_EXEC := $(shell cygpath -m ${RUN_TESTS_EXEC_PATH})
else
	RUN_TESTS_EXEC := ./build/exec/runtests
endif

.PHONY: testbin test

test:
	${RUN_TESTS_EXEC} $(IDRIS2) $(INTERACTIVE) --timing --failure-file failures --threads $(threads) --only $(only) --except $(except)

retest:
	${RUN_TESTS_EXEC} $(IDRIS2) $(INTERACTIVE) --timing --failure-file failures --threads $(threads) --only-file failures --only $(only) --except $(except)

testbin:
	${IDRIS2} --build tests.ipkg

clean:
	$(RM) failures
	$(RM) -r build
	$(RM) -r **/**/build
	@find . -type f -name 'output' -exec rm -rf {} \;
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
