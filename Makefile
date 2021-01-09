export EMACS ?= emacs
export BATCH = --batch -q -l .emacs/init.el

ELS = $(wildcard *.el)
LINT_ELS = $(filter-out dimmer.el-autoloads.el,$(ELS))
OBJECTS = $(ELS:.el=.elc)

.PHONY: version lint clean cleanelpa

.elpa:
	mkdir -p .emacs/elpa/gnupg && \
	chmod 700 .emacs/elpa/gnupg && \
	echo "disable-ipv6" > .emacs/elpa/gnupg/dirmngr.conf && \
	for i in {1..3}; do \
	gpg --keyserver keyserver.ubuntu.com \
	    --homedir .emacs/elpa/gnupg \
	    --recv-keys 066DAFCB81E42C40 \
	    && break || sleep 15; \
	done
	$(EMACS) $(BATCH)
	touch .elpa

version: .elpa
	$(EMACS) $(BATCH) --version

# TODO: re-enable package-lint after handling "frame-focus-state" error
lint: .elpa
	$(EMACS) $(BATCH) -f elisp-lint-files-batch --no-package-lint $(LINT_ELS)

clean:
	rm -f $(OBJECTS) dimmer.el-autoloads.el *~

cleanall: clean
	rm -rf .emacs/elpa .emacs/quelpa .emacs/.emacs-custom.el .elpa
