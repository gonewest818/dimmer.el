;;; dimmer.el --- highlights the current-window by dimming the others
;; 
;; Filename: dimmer.el
;; Author: Neil Okamoto
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/gonewest818/dimmer.el
;; Keywords: faces, editing
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This module provides a subtle visual indication which window is
;; currently active by dimming the faces on the others. It does this
;; nondestructively, and computes the dimmed faces dynamically such
;; that your overall color scheme is shown in a muted form without
;; requiring you to define the "dim" versions of every face.
;;
;; The percentage of dimming is user configrable. In addition, for
;; users of "light" themes there is a dimmer-invert flag that adjusts
;; the faces brighter (toward white, rather than toward black).
;;
;; Unlike the 'hiwin' module which has a similar goal, this module
;; does *not* change the color of the background in any way. It only
;; adjusts foregrounds. In the underlying implementation we do not
;; use overlays, and therefore we avoid some of the visual problems
;; the hiwin module exhibits when highlighting interactive shells
;; and/or repls.
;;
;; To use
;;
;;     (require 'dimmer)
;;     (dimmer-activate)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:



;; (setq face-remapping-alist nil)

;; (with-selected-window (get-buffer-window "*eshell*")
;;   (setq face-remapping-alist nil)
;;   (redraw-display))


;; ======================================================================

(defvar dimmer-face-remaps (make-hash-table))

(defvar dimmer-last-buffer nil)

(defvar dimmer-percent 0.80)

(defvar dimmer-invert nil)

(defun dimmer-face-color (f pct &optional invert)
  (when-let ((fg (face-foreground f)))
    (if invert
        (apply 'color-rgb-to-hex
               (mapcar (lambda (x) (- 1 (* (- 1 x) pct)))
                       (color-name-to-rgb fg)))
        (apply 'color-rgb-to-hex
               (mapcar (lambda (x) (* x pct))
                       (color-name-to-rgb fg))))))

;; (dimmer-face-color 'default 0.1 t)
;; (dimmer-face-color 'hiwin-face 0.5)

(defun dimmer-dim-window (name pct &optional invert)
  (let ((win (get-buffer-window name))
        (cookies nil))
    (unless (gethash win dimmer-face-remaps)
      (with-selected-window win
        (puthash win
                 (dolist (f (face-list) cookies)
                   (when-let ((c (dimmer-face-color f pct invert)))
                     (setq cookies
                           (cons (face-remap-add-relative f :foreground c) cookies))))
                 dimmer-face-remaps)))))

(defun dimmer-restore-window (name)
  (let ((win (get-buffer-window name)))
    (when-let ((cookies (gethash win dimmer-face-remaps)))
      (with-selected-window win
        (dolist (c cookies)
          (face-remap-remove-relative c)))
      (remhash win dimmer-face-remaps))))

;; (dimmer-dim-window "hiwin.el" 0.70)
;; (gethash (get-buffer-window "*eshell*") dimmer-face-remaps)
;; (dimmer-restore-window "hiwin.el")

;;;###autoload
(defun dimmer-activate ()
  (interactive)
  (add-hook 'post-command-hook 'dimmer-command-hook))

;;;###autoload
(defun dimmer-deactivate ()
  (interactive)
  (remove-hook 'post-command-hook 'dimmer-command-hook)
  (dimmer-delete-all))

(defun fih ()
  (message (concat "focus in: " (buffer-name (window-buffer (selected-window))))))

(defun foh ()
  (message (concat "focus out: " (buffer-name (window-buffer (selected-window))))))

;; (add-hook 'focus-in-hook 'fih)
;; (add-hook 'focus-out-hook 'foh)

(defun dimmer-command-hook ()
  (unless (eq (current-buffer) dimmer-last-buffer)
    (let ((current (current-buffer))
          (previous dimmer-last-buffer))
      (setq dimmer-last-buffer current)
      (when current
        (dimmer-restore-window (buffer-name current)))
      (when previous
        (dimmer-dim-window (buffer-name previous) dimmer-percent dimmer-invert)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dimmer.el ends here
