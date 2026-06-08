export EMACS ?= emacs
export EASK := $(shell command -v eask 2>/dev/null)

.PHONY: lint compile test install-deps clean

lint:
	$(EASK) lint checkdoc && $(EASK) lint package && $(EASK) lint declare && $(EASK) lint indent && $(EASK) lint elisp-lint

compile:
	$(EASK) compile

test: compile
	$(EASK) test ert test/*-test.el

install-deps:
	$(EASK) install-deps --dev

clean:
	$(EASK) clean elc
	rm -f dimmer.el-autoloads.el *~
