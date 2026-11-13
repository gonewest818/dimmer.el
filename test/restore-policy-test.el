;;; restore-policy-test.el --- Tests for dimmer restore policy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Neil Okamoto

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dimmer)

(defmacro dimmer-test-with-temp-buffer (&rest body)
  `(with-temp-buffer
     (setq-local face-remapping-alist nil)
     (setq-local dimmer-buffer-face-remaps nil)
     (setq-local dimmer-buffer-tainted nil)
     ,@body))

(defmacro dimmer-test-with-temp-window (&rest body)
  "temporarily set the selected window to a temp buffer."
  (let ((temp-window (make-symbol "temp-window"))
        (old-buffer (make-symbol "old-buffer")))
    `(dimmer-test-with-temp-buffer
      (let* ((,temp-window (selected-window))
             (,old-buffer (window-buffer ,temp-window)))
        (set-window-buffer ,temp-window (current-buffer))
        (unwind-protect
            (progn ,@body)
          (set-window-buffer ,temp-window ,old-buffer))))))

(ert-deftest dimmer-restore-buffer/marks-tainted-on-remap-error ()
  (dimmer-test-with-temp-buffer
   (let ((calls nil)
         (dimmer-buffer-face-remaps '(cookie-1 cookie-2)))
     (cl-letf (((symbol-function 'face-remap-remove-relative)
                (lambda (cookie)
                  (push cookie calls)
                  (when (eq cookie 'cookie-2)
                    (error "boom")))))
       (dimmer-restore-buffer (current-buffer)))
     (should (equal calls '(cookie-2 cookie-1)))
     (should dimmer-buffer-tainted)
     (should (null dimmer-buffer-face-remaps)))))

(ert-deftest dimmer-restore-buffer/clears-face-remaps-on-success ()
  (dimmer-test-with-temp-buffer
   (let ((calls nil)
         (dimmer-buffer-face-remaps '(cookie-1 cookie-2)))
     (cl-letf (((symbol-function 'face-remap-remove-relative)
                (lambda (cookie)
                  (push cookie calls))))
       (dimmer-restore-buffer (current-buffer)))
     (should (equal calls '(cookie-2 cookie-1)))
     (should-not dimmer-buffer-tainted)
     (should (null dimmer-buffer-face-remaps)))))

(ert-deftest dimmer-filtered-window-p/skips-tainted-when-disabled ()
  (dimmer-test-with-temp-window
   (let ((dimmer-reprocess-tainted-buffers nil)
         (dimmer-buffer-tainted t))
     (should (dimmer-filter-window-p (selected-window))))))

(ert-deftest dimmer-filtered-window-p/keeps-tainted-when-enabled ()
  (dimmer-test-with-temp-window
   (let ((dimmer-reprocess-tainted-buffers t)
         (dimmer-buffer-tainted t))
     (should-not (dimmer-filter-window-p (selected-window))))))

;;; restore-policy-test.el ends here
