;;; dimmer.el --- visually highlight the selected buffer

;; Copyright (C) 2017-2018 Neil Okamoto

;; Filename: dimmer.el
;; Author: Neil Okamoto
;; Version: 0.3.1-SNAPSHOT
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/gonewest818/dimmer.el
;; Keywords: faces, editing
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module provides a minor mode that indicates which buffer is
;; currently active by dimming the faces in the other buffers.  It
;; does this nondestructively, and computes the dimmed faces
;; dynamically such that your overall color scheme is shown in a muted
;; form without requiring you to define what is a "dim" version of
;; every face.
;;
;; The `default` background color is the target for all dimming
;; calculations.  If your default background is "white" then faces
;; will be made brighter when "dimmed".  If your default background is
;; a dark blue, then faces will be shifted "darker" and "more blue"
;; when buffers are dimmed.
;;
;; Usage:
;;
;;      (require 'dimmer) ; unless installed as a package
;;      (dimmer-mode)
;;
;; Customization:
;;
;; `dimmer-fraction` controls the degree to which buffers are dimmed.
;; Range is 0.0 - 1.0, and default is 0.20.  Increase value if you
;; like the other buffers to be more dim.
;;
;; `dimmer-exclusion-regexp` can be used to specify buffers that
;; should never be dimmed.  If the buffer name matches this regexp
;; then `dimmer.el` will not dim that buffer.
;;
;; `dimmer-use-colorspace` allows you to specify what color space the
;; dimming calculation is performed in.  In the majority of cases you
;; won't need to touch this setting.  See the docstring below for more
;; information.
;;
;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'seq)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration

(defgroup dimmer nil
  "Highlight current-buffer by dimming faces on the others."
  :prefix "dimmer-"
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/gonewest818/dimmer.el"))

(defcustom dimmer-fraction 0.20
  "Control the degree to which buffers are dimmed (0.0 - 1.0)."
  :type '(float)
  :group 'dimmer)
(define-obsolete-variable-alias 'dimmer-percent 'dimmer-fraction)

(defcustom dimmer-exclusion-regexp nil
  "Regular expression describing buffer names that are never dimmed."
  :type '(choice (const nil) (regexp))
  :group 'dimmer)

(defcustom dimmer-use-colorspace :cielab
  "Colorspace in which dimming calculations are performed.
Choices are :cielab (default), :hsl, or :rgb.

CIELAB is the default, and in most cases should serve perfectly
well.  As a colorspace it attempts to be uniform to the human
eye, meaning the degree of dimming should be roughly the same for
all your foreground colors.

Bottom line: If CIELAB is working for you, then you don't need to
experiment with the other choices.

However, interpolating in CIELAB introduces one wrinkle, in that
mathematically it's possible to generate a color that isn't
representable on your RGB display (colors having one or more RGB
channel values < 0.0 or > 1.0).  When dimmer finds an
\"impossible\" RGB value like that it simply clamps that value to
fit in the range 0.0 - 1.0.  Clamping like this can lead to some
colors looking \"wrong\".  If you think the dimmed values look
wrong, then try HSV or RGB instead."
  :type '(radio (const :tag "Interpolate in CIELAB 1976" :cielab)
                (const :tag "Interpolate in HSL" :hsl)
                (const :tag "Interpolate in RGB" :rgb))
  :group 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation

(defvar-local dimmer-buffer-face-remaps nil
  "Per-buffer face remappings needed for later clean up.")

(defconst dimmer-dimmed-faces (make-hash-table :test 'equal)
  "Cache of face names with their computed dimmed values.")

(defconst dimmer-last-buffer nil
  "Identity of the last buffer to be made current.")

(defconst dimmer-debug-messages nil
  "Enable debugging output to *Messages* buffer.")

(defun dimmer-lerp (frac v0 v1)
  "Use FRAC to compute a linear interpolation of V0 and V1."
  (+ (* v0 (- 1.0 frac))
     (* v1 frac)))

(defun dimmer-lerp-in-rgb (fg bg frac)
  "Compute linear interpolation of FG and BG in RGB space.
FRAC controls the interpolation."
  (apply 'color-rgb-to-hex
         (cl-mapcar (apply-partially 'dimmer-lerp frac) fg bg)))

(defun dimmer-lerp-in-hsl (fg bg frac)
  "Compute linear interpolation of FG and BG in HSL space.
FRAC controls the interpolation."
  (apply 'color-rgb-to-hex
         (apply 'color-hsl-to-rgb
                (cl-mapcar (apply-partially 'dimmer-lerp frac)
                           (apply 'color-rgb-to-hsl fg)
                           (apply 'color-rgb-to-hsl bg)))))

(defun dimmer-lerp-in-cielab (fg bg frac)
  "Compute linear interpolation of FG and BG in CIELAB space.
FRAC controls the interpolation."
  (apply 'color-rgb-to-hex
         (mapcar 'color-clamp
                 (apply 'color-lab-to-srgb
                        (cl-mapcar (apply-partially 'dimmer-lerp frac)
                                   (apply 'color-srgb-to-lab fg)
                                   (apply 'color-srgb-to-lab bg))))))

(defun dimmer-compute-rgb (fg bg frac colorspace)
  "Compute a \"dimmed\" color via linear interpolation.

Blends the foreground FG and the background BG using FRAC to
control the interpolation. When FRAC is 0.0, the result is equal
to FG.  When FRAC is 1.0, the result is equal to BG.

Any other value for FRAC means the result's hue, saturation, and
value will be adjusted linearly so that the color sits somewhere
between FG and BG.

The interpolation is performed in a COLORSPACE which is specified
with a symbol, :rgb, :hsv, or :cielab."
  (pcase colorspace
    (:rgb    (dimmer-lerp-in-rgb fg bg frac))
    (:hsv    (dimmer-lerp-in-hsl fg bg frac))
    (:cielab (dimmer-lerp-in-cielab fg bg frac))
    (_       (dimmer-lerp-in-cielab fg bg frac))))

(defun dimmer-face-color (f frac)
  "Compute a dimmed version of the foreground color of face F.
FRAC is the amount of dimming where 0.0 is no change and 1.0 is
maximum change."
  (let ((fg (face-foreground f))
        (bg (face-background 'default)))
    (when (and fg (color-defined-p fg)
               bg (color-defined-p bg))
      (let ((key (format "%s-%s-%f-%s" fg bg frac dimmer-use-colorspace)))
        (or (gethash key dimmer-dimmed-faces)
            (let ((rgb (dimmer-compute-rgb (color-name-to-rgb fg)
                                           (color-name-to-rgb bg)
                                           frac
                                           dimmer-use-colorspace)))
              (when rgb
                (puthash key rgb dimmer-dimmed-faces)
                rgb)))))))

(defun dimmer-dim-buffer (buf frac)
  "Dim all the faces defined in the buffer BUF.
FRAC controls the dimming as defined in ‘dimmer-face-color’."
  (with-current-buffer buf
    (unless dimmer-buffer-face-remaps
      (dolist (f (face-list))
        (let ((c (dimmer-face-color f frac)))
          (when c  ; e.g. "(when-let* ((c (...)))" in Emacs 26
            (push (face-remap-add-relative f :foreground c)
                  dimmer-buffer-face-remaps)))))))

(defun dimmer-restore-buffer (buf)
  "Restore the un-dimmed faces in the buffer BUF."
  (with-current-buffer buf
    (when dimmer-buffer-face-remaps
      (mapc 'face-remap-remove-relative dimmer-buffer-face-remaps)
      (setq dimmer-buffer-face-remaps nil))))

(defun dimmer-filtered-buffer-list ()
  "Get filtered subset of all visible buffers in all frames."
  (let (buffers)
    (walk-windows
     (lambda (win)
       (let* ((buf (window-buffer win))
              (name (buffer-name buf)))
         (unless (and dimmer-exclusion-regexp
                      (string-match-p dimmer-exclusion-regexp name))
           (push buf buffers))))
     nil
     t)
    buffers))

(defun dimmer-process-all ()
  "Process all buffers and dim or un-dim each."
  (let ((selected (current-buffer)))
    (setq dimmer-last-buffer selected)
    (dolist (buf (dimmer-filtered-buffer-list))
      (if (eq buf selected)
          (dimmer-restore-buffer buf)
        (dimmer-dim-buffer buf dimmer-fraction)))))

(defun dimmer-dim-all ()
  "Dim all buffers."
  (dimmer--dbg "dimmer-dim-all")
  (mapc (lambda (buf)
          (dimmer-dim-buffer buf dimmer-fraction))
        (buffer-list)))

(defun dimmer-restore-all ()
  "Un-dim all buffers."
  (mapc 'dimmer-restore-buffer (buffer-list)))

(defun dimmer-command-hook ()
  "Process all buffers if current buffer has changed."
  (dimmer--dbg "dimmer-command-hook")
  (unless (eq (window-buffer) dimmer-last-buffer)
    (dimmer-process-all)))

(defun dimmer-config-change-hook ()
  "Process all buffers if window configuration has changed."
  (dimmer--dbg "dimmer-config-change-hook")
  (dimmer-process-all))

;;;###autoload
(define-minor-mode dimmer-mode
  "visually highlight the selected buffer"
  nil
  :lighter ""
  :global t
  :require 'dimmer
  (if dimmer-mode
      (progn
        (add-hook 'focus-in-hook 'dimmer-config-change-hook)
        (add-hook 'focus-out-hook 'dimmer-dim-all)
        (add-hook 'post-command-hook 'dimmer-command-hook)
        (add-hook 'window-configuration-change-hook 'dimmer-config-change-hook))
    (remove-hook 'focus-in-hook 'dimmer-config-change-hook)
    (remove-hook 'focus-out-hook 'dimmer-dim-all)
    (remove-hook 'post-command-hook 'dimmer-command-hook)
    (remove-hook 'window-configuration-change-hook 'dimmer-config-change-hook)
    (dimmer-restore-all)))

;;;###autoload
(define-obsolete-function-alias 'dimmer-activate 'dimmer-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; debugging - call from *scratch*, ielm, or eshell

(defun dimmer--debug-face-remapping-alist (name &optional clear)
  "Display 'face-remapping-alist' for buffer NAME (or clear if CLEAR)."
  (with-current-buffer name
    (if clear
        (setq face-remapping-alist nil)
      face-remapping-alist)))

(defun dimmer--debug-buffer-face-remaps (name &optional clear)
  "Display 'dimmer-buffer-face-remaps' for buffer NAME (or clear if CLEAR)."
  (with-current-buffer name
    (if clear
        (setq dimmer-buffer-face-remaps nil)
      dimmer-buffer-face-remaps)))

(defun dimmer--debug-reset (name)
  "Clear 'face-remapping-alist' and 'dimmer-buffer-face-remaps' for NAME."
  (dimmer--debug-face-remapping-alist name t)
  (dimmer--debug-buffer-face-remaps name t)
  (redraw-display))

(defun dimmer--dbg (label)
  "Print a debug state with the given LABEL."
  (if dimmer-debug-messages
      (let ((inhibit-message t))
        (message "%s: cb '%s' wb '%s' last '%s' %s"
                 label
                 (current-buffer)
                 (window-buffer)
                 dimmer-last-buffer
                 (if (not (eq (current-buffer) (window-buffer)))
                     "**"
                   "")))))

(provide 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dimmer.el ends here
