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
    (set-face-attribute 'dimmer-test-face nil
                        :foreground 'unspecified
                        :background 'unspecified)
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

;;; extended face attribute dimming

(ert-deftest dimmer-face-color/dims-box-underline-overline-strikethrough-distant ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-extended-face)
    (set-face-foreground 'dimmer-test-extended-face "#ffffff")
    (set-face-attribute 'dimmer-test-extended-face nil
                        :box '(:color "#ff0000" :line-width 2 :style released-button)
                        :underline '(:color "#00ff00" :style wave)
                        :overline "#0000ff"
                        :strike-through "#ff00ff"
                        :distant-foreground "#888888")
    (let ((result (dimmer-face-color 'dimmer-test-extended-face 0.3)))
      (dolist (attr '(:box :underline))
        (let ((plist-val (plist-get result attr)))
          (should (listp plist-val))
          (should (stringp (plist-get plist-val :color)))
          (should (string-prefix-p "#" (plist-get plist-val :color)))))
      (dolist (attr '(:overline :strike-through :distant-foreground))
        (let ((str (plist-get result attr)))
          (should (stringp str))
          (should (string-prefix-p "#" str))))
      (should (not (equal (plist-get result :foreground) "#ffffff"))))))

(ert-deftest dimmer-face-color/box-t-no-explicit-color ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-box-t)
    (set-face-attribute 'dimmer-test-box-t nil :box t)
    (let ((result (dimmer-face-color 'dimmer-test-box-t 0.3)))
      (should (not (plist-get result :box))))))

(ert-deftest dimmer-face-color/underline-plist-no-color-key ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-underline-no-color)
    (set-face-attribute 'dimmer-test-underline-no-color nil
                        :underline '(:style wave))
    (let ((result (dimmer-face-color 'dimmer-test-underline-no-color 0.3)))
      (should (not (plist-get result :underline))))))

;;; desaturate mode

(ert-deftest dimmer-face-color/desaturate-returns-hex ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :desaturate)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-desaturate)
    (set-face-foreground 'dimmer-test-desaturate "#ff0000")
    (set-face-background 'dimmer-test-desaturate "#0000ff")
    (let ((result (dimmer-face-color 'dimmer-test-desaturate 0.3)))
      (should (stringp (plist-get result :foreground)))
      (should (stringp (plist-get result :background)))
      (should (string-prefix-p "#" (plist-get result :foreground)))
      (should (string-prefix-p "#" (plist-get result :background))))))

(ert-deftest dimmer-face-color/desaturate-all-attributes ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :desaturate)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-desaturate-all)
    (set-face-foreground 'dimmer-test-desaturate-all "#ffffff")
    (set-face-attribute 'dimmer-test-desaturate-all nil
                        :box '(:color "#ff0000" :line-width 1)
                        :underline '(:color "#00ff00" :style wave))
    (let ((result (dimmer-face-color 'dimmer-test-desaturate-all 0.5)))
      (should (stringp (plist-get result :foreground)))
      (should (listp (plist-get result :box)))
      (should (stringp (plist-get (plist-get result :box) :color)))
      (should (listp (plist-get result :underline)))
      (should (stringp (plist-get (plist-get result :underline) :color))))))

;;; hueshift mode

(ert-deftest dimmer-face-color/hueshift-returns-hex ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :hueshift)
        (dimmer-hue-target 0.5)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-hueshift)
    (set-face-foreground 'dimmer-test-hueshift "#ff0000")
    (set-face-background 'dimmer-test-hueshift "#0000ff")
    (let ((result (dimmer-face-color 'dimmer-test-hueshift 0.3)))
      (should (stringp (plist-get result :foreground)))
      (should (stringp (plist-get result :background)))
      (should (string-prefix-p "#" (plist-get result :foreground)))
      (should (string-prefix-p "#" (plist-get result :background))))))

(ert-deftest dimmer-face-color/hueshift-all-attributes ()
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :hueshift)
        (dimmer-hue-target 0.5)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-hueshift-all)
    (set-face-foreground 'dimmer-test-hueshift-all "#ffffff")
    (set-face-attribute 'dimmer-test-hueshift-all nil
                        :box '(:color "#ff0000" :line-width 1)
                        :underline '(:color "#00ff00" :style wave))
    (let ((result (dimmer-face-color 'dimmer-test-hueshift-all 0.5)))
      (should (stringp (plist-get result :foreground)))
      (should (listp (plist-get result :box)))
      (should (stringp (plist-get (plist-get result :box) :color)))
      (should (listp (plist-get result :underline)))
      (should (stringp (plist-get (plist-get result :underline) :color))))))

;;; symbolic foreground-color underline

(ert-deftest dimmer-face-color/underline-foreground-color-no-error ()
  "A symbolic :color must not be passed to `color-defined-p' (see #89)."
  (skip-unless (display-graphic-p))
  (dolist (mode '(:foreground :both :desaturate :hueshift))
    (let ((dimmer-adjustment-mode mode)
          (dimmer-hue-target 0.5)
          (dimmer-use-colorspace :rgb))
      (make-face 'dimmer-test-underline-fg-symbol)
      (set-face-foreground 'dimmer-test-underline-fg-symbol "#ffffff")
      (set-face-attribute 'dimmer-test-underline-fg-symbol nil
                          :underline '(:color foreground-color :style double-line))
      (let ((result (dimmer-face-color 'dimmer-test-underline-fg-symbol 0.3)))
        (should (listp result))
        (should (not (plist-get result :underline))))
      (should (equal (face-attribute 'dimmer-test-underline-fg-symbol
                                     :underline nil t)
                     '(:color foreground-color :style double-line))))))

(ert-deftest dimmer-face-color/underline-explicit-color-still-dims ()
  "An explicit string color is still dimmed and keeps its :style."
  (skip-unless (display-graphic-p))
  (let ((dimmer-adjustment-mode :foreground)
        (dimmer-use-colorspace :rgb))
    (make-face 'dimmer-test-underline-explicit)
    (set-face-foreground 'dimmer-test-underline-explicit "#ffffff")
    (set-face-attribute 'dimmer-test-underline-explicit nil
                        :underline '(:color "#00ff00" :style double-line))
    (let ((result (dimmer-face-color 'dimmer-test-underline-explicit 0.3)))
      (should (listp (plist-get result :underline)))
      (should (stringp (plist-get (plist-get result :underline) :color)))
      (should (string-prefix-p "#" (plist-get (plist-get result :underline) :color)))
      (should-not (equal (plist-get (plist-get result :underline) :color)
                         "#00ff00"))
      (should (eq (plist-get (plist-get result :underline) :style) 'double-line)))))

;;; graphical-test.el ends here
