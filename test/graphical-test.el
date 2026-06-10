;;; graphical-test.el --- Graphical display tests for dimmer  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; These tests require a graphical display and are skipped in batch/CI mode.

;;; Code:

(require 'ert)
(require 'dimmer)

;;; dimmer-face-color

(ert-deftest dimmer-face-color/empty-plist-when-no-color ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (let ((result (dimmer-face-color 'dimmer-test-face 0.3)))
      (should (listp result))
      (should (not (plist-get result :foreground)))
      (should (not (plist-get result :background))))))

(ert-deftest dimmer-face-color/foreground-set ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (set-face-foreground 'dimmer-test-face "#ffffff")
    (let ((result (dimmer-face-color 'dimmer-test-face 0.3)))
      (should (listp result))
      (should (stringp (plist-get result :foreground)))
      (should (string-prefix-p "#" (plist-get result :foreground))))))

(ert-deftest dimmer-face-color/background-set ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :background)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (set-face-background 'dimmer-test-face "#000000")
    (let ((result (dimmer-face-color 'dimmer-test-face 0.3)))
      (should (listp result))
      (should (stringp (plist-get result :background)))
      (should (string-prefix-p "#" (plist-get result :background))))))

(ert-deftest dimmer-face-color/both-modes ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :both)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (set-face-foreground 'dimmer-test-face "#ffffff")
    (set-face-background 'dimmer-test-face "#000000")
    (let ((result (dimmer-face-color 'dimmer-test-face 0.3)))
      (should (listp result))
      (should (plist-get result :foreground))
      (should (plist-get result :background)))))

(ert-deftest dimmer-face-color/reset-foreground-no-error ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (set-face-foreground 'dimmer-test-face 'unspecified)
    (should (listp (dimmer-face-color 'dimmer-test-face 0.3)))))

(ert-deftest dimmer-face-color/reset-background-no-error ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :background)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (set-face-background 'dimmer-test-face 'unspecified)
    (should (listp (dimmer-face-color 'dimmer-test-face 0.3)))))

(ert-deftest dimmer-face-color/reset-handling-no-error ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-face)
    (set-face-foreground 'dimmer-test-face 'unspecified)
    (should (equal (dimmer-face-color 'dimmer-test-face 0.3) '()))))

;;; graphical-test.el ends here