CASK = cask

.PHONY: version bytecomp checkdoc

version:
	$(CASK) emacs --version

ci-bytecomp: version
	$(CASK) emacs -Q --batch -l test/ci-bytecomp.el

ci-checkdoc: version
	$(CASK) emacs -Q --batch --eval '(checkdoc-file "dimmer.el")' 2>&1 \
		 | grep -E "dimmer.el:[0-9]+" && exit 1 || exit 0
