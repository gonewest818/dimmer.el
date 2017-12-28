;; test-bytecomp.el

;; This batch script is for use in CI.
;; Compile and exit on error or warning.

(progn
  (setq byte-compile-error-on-warn t)
  (unless (byte-compile-file "dimmer.el")
    (kill-emacs 1)))
