EMACS ?= emacs
BATCH = $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" -L .

SRCS = herdr-sanctum-rpc.el \
       herdr-sanctum-git.el \
       herdr-sanctum-theme.el \
       herdr-sanctum-ui.el \
       herdr-sanctum-terminal.el \
       herdr-sanctum-sidebar.el \
       herdr-sanctum-drawer.el \
       herdr-sanctum.el \
       herdr-sanctum-ime.el \
       herdr-sanctum-tab-routing.el

TEST_HELPER := test/herdr-sanctum-test-helper.el
TESTS := $(sort $(wildcard test/*-test.el))
TEST_LOADS := $(foreach f,$(TEST_HELPER) $(TESTS),-l $(basename $(notdir $(f))))

.PHONY: all compile test clean

all: compile test

compile:
	$(BATCH) -L test --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(SRCS) $(TEST_HELPER) $(TESTS)

test:
	$(BATCH) -L test -l ert $(TEST_LOADS) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
