EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .

SRCS = herdr-sanctum-rpc.el \
       herdr-sanctum-git.el \
       herdr-sanctum-theme.el \
       herdr-sanctum-terminal.el \
       herdr-sanctum-sidebar.el \
       herdr-sanctum-drawer.el \
       herdr-sanctum.el \
       herdr-sanctum-ime.el \
       herdr-sanctum-tab-routing.el

.PHONY: all compile test clean

all: compile test

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(SRCS)

test:
	$(BATCH) -L test -l ert -l herdr-sanctum-test -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
