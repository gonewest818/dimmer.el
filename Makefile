export EMACS ?= emacs
export BATCH = --batch -q -l .emacs/init.el

ELS = $(wildcard *.el)
LINT_ELS = $(filter-out dimmer.el-autoloads.el,$(ELS))
OBJECTS = $(ELS:.el=.elc)

.PHONY: version lint clean cleanelpa

.elpa:
	$(EMACS) $(BATCH)
	touch .elpa

version: .elpa
	$(EMACS) $(BATCH) --version

lint: .elpa
	$(EMACS) $(BATCH) -f elisp-lint-files-batch $(LINT_ELS)

clean:
	rm -f $(OBJECTS) dimmer.el-autoloads.el *~

cleanall: clean
	rm -rf .emacs/elpa .emacs/quelpa .emacs/.emacs-custom.el .elpa
