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
;; The percentage of dimming is user configurable. In addition, for
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


(defconst dimmer-face-remaps (make-hash-table)
  "Internal hashmap of per-buffer face remappings needed to clean up later.")

(defconst dimmer-last-buffer nil
  "Identity of the last buffer to be made current.")

(defcustom dimmer-percent 0.80
  "Controls the degree to which unselected buffers are dimmed."
  :type '(float)
  :group 'dimmer)

(defcustom dimmer-invert nil
  "Inverts the dimming for dark-on-light themes."
  :type '(boolean)
  :group 'dimmer)

(defun dimmer-face-color (f pct &optional invert)
  (when-let ((fg (face-foreground f)))
    (if invert
        (apply 'color-rgb-to-hex
               (mapcar (lambda (x) (- 1 (* (- 1 x) pct)))
                       (color-name-to-rgb fg)))
        (apply 'color-rgb-to-hex
               (mapcar (lambda (x) (* x pct))
                       (color-name-to-rgb fg))))))

(defun dimmer-dim-buffer (buf pct &optional invert)
  (let ((cookies nil))
    (unless (gethash buf dimmer-face-remaps)
      ;; skip dimming if already dimmed
      (with-current-buffer buf
        (puthash buf
                 (dolist (f (face-list) cookies)
                   (when-let ((c (dimmer-face-color f pct invert)))
                     (setq cookies
                           (cons (face-remap-add-relative f :foreground c) cookies))))
                 dimmer-face-remaps)))))

(defun dimmer-restore-buffer (buf)
  (when-let ((cookies (gethash buf dimmer-face-remaps)))
    ;; when-let skips restoring if not dimmed
    (with-current-buffer buf
      (dolist (c cookies)
        (face-remap-remove-relative c)))
    (remhash buf dimmer-face-remaps)))

(defun dimmer-process-all ()
  (let ((selected (current-buffer)))
    (setq dimmer-last-buffer selected)
    (dolist (buf (buffer-list))
      (if (eq buf selected)
          (dimmer-restore-buffer buf)
        (dimmer-dim-buffer buf dimmer-percent dimmer-invert)))))

(defun dimmer-restore-all ()
  (dolist (buf (buffer-list))
    (dimmer-restore-buffer buf)))

;; (dimmer-dim-window "hiwin.el" 0.70)
;; (gethash (get-buffer-window "*eshell*") dimmer-face-remaps)
;; (dimmer-restore-window "hiwin.el")
;; (dimmer-restore-all)

(defun dimmer-command-hook ()
  (unless (eq (current-buffer) dimmer-last-buffer)
    (dimmer-process-all)))

(defun dimmer-config-change-hook ()
  (dimmer-restore-all)
  (dimmer-process-all))

;;;###autoload
(defun dimmer-activate ()
  (interactive)
  (add-hook 'post-command-hook 'dimmer-command-hook)
  (add-hook 'window-configuration-change-hook 'dimmer-config-change-hook))

;;;###autoload
(defun dimmer-deactivate ()
  (interactive)
  (remove-hook 'post-command-hook 'dimmer-command-hook)
  (remove-hook 'window-configuration-change-hook 'dimmer-config-change-hook)
  (dimmer-restore-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dimmer.el ends here
