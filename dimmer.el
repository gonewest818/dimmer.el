;;; dimmer.el --- visually highlight the selected buffer
;; 
;; Filename: dimmer.el
;; Author: Neil Okamoto
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/gonewest818/dimmer.el
;; Keywords: faces, editing
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; This module provides a subtle visual indication which window is
;; currently active by dimming the faces on the others.  It does this
;; nondestructively, and computes the dimmed faces dynamically such
;; that your overall color scheme is shown in a muted form without
;; requiring you to define the "dim" versions of every face.
;;
;; The percentage of dimming is user configurable.  In addition, for
;; users of "light" themes there is a dimmer-invert flag that adjusts
;; the faces brighter (toward white, rather than toward black).
;;
;; Unlike the 'hiwin' module which has a similar goal, this module
;; does *not* change the color of the background in any way.  It only
;; adjusts foregrounds.  In the underlying implementation we do not
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

;; to-do
;;  - 'dimmer-face-remaps' could stored in buffer local variables
;;  - allow user-configurable filters to exempt buffers from dimming?


(defconst dimmer-face-remaps (make-hash-table :test 'equal)
  "Internal hashmap of per-buffer face remappings for eventual clean up.")

(defconst dimmer-dimmed-faces (make-hash-table :test 'equal)
  "Cache of computed faces with dimmed foreground values.")

(defconst dimmer-last-buffer nil
  "Identity of the last buffer to be made current.")

(defcustom dimmer-percent 0.20
  "Control the degree to which unselected buffers dim (range: 0.0 - 1.0)."
  :type '(float)
  :group 'dimmer)

(defcustom dimmer-invert nil
  "Invert the dimming for dark-on-light themes."
  :type '(boolean)
  :group 'dimmer)


(defun dimmer-compute-rgb (c pct invert)
  "Computes color C dimmed by percentage PCT. When INVERT is true,
the dimmed value is brighter rather than darker."
  (if invert
      (apply 'color-rgb-to-hex
             (mapcar (lambda (x) (- 1.0 (* (- 1.0 x)
                                           (- 1.0 pct))))
                     (color-name-to-rgb c)))
    (apply 'color-rgb-to-hex
           (mapcar (lambda (x) (* x (- 1.0 pct)))
                   (color-name-to-rgb c)))))

(defun dimmer-face-color (f pct invert)
  "Compute a dimmed version of the foreground color of face F.
PCT is the amount of dimming where 0.0 is no change and 1.0 is
maximum change.  When INVERT is not nil, invert the scaling
for dark-on-light themes."
  (let ((key (concat (symbol-name f) "-"
                     (number-to-string pct)
                     (if invert "-t" "-nil"))))
    (or (gethash key dimmer-dimmed-faces)
        (when-let ((fg (face-foreground f)))
          (let* ((rgb (dimmer-compute-rgb fg pct invert)))
            (puthash key rgb dimmer-dimmed-faces)
            rgb)))))

(defun dimmer-dim-buffer (buf pct invert)
  "Dim all the faces defined in the buffer BUF.
PCT and INVERT controls the dimming as defined
in ‘dimmer-face-color’."
  (let ((cookies nil))
    (unless (gethash buf dimmer-face-remaps)
      (with-current-buffer buf
        (puthash buf
                 (dolist (f (face-list) cookies)
                   (when-let ((c (dimmer-face-color f pct invert)))
                     (setq cookies
                           (cons (face-remap-add-relative f :foreground c) cookies))))
                 dimmer-face-remaps)))))

(defun dimmer-restore-buffer (buf)
  "Restore the un-dimmed faces in the buffer BUF."
  (when-let ((cookies (gethash buf dimmer-face-remaps)))
    (with-current-buffer buf
      (dolist (c cookies)
        (face-remap-remove-relative c)))
    (remhash buf dimmer-face-remaps)))

(defun dimmer-filtered-buffer-list ()
  "Get filtered subset of all buffers."
  (seq-filter
   (lambda (b)
     (not (eq ?\s ; space
              (elt (buffer-name b) 0))))
   (buffer-list)))

(defun dimmer-process-all ()
  "Process all buffers and dim or un-dim each."
  (let ((selected (current-buffer)))
    (setq dimmer-last-buffer selected)
    (dolist (buf (dimmer-filtered-buffer-list))
      (if (eq buf selected)
          (dimmer-restore-buffer buf)
        (dimmer-dim-buffer buf dimmer-percent dimmer-invert)))))

(defun dimmer-restore-all ()
  "Un-dim all buffers."
  (dolist (buf (dimmer-filtered-buffer-list))
    (dimmer-restore-buffer buf)))

(defun dimmer-command-hook ()
  "Process all buffers if current buffer has changed."
  (unless (eq (current-buffer) dimmer-last-buffer)
    (dimmer-process-all)))

(defun dimmer-config-change-hook ()
  "Process all buffers if window configuration has changed."
  ;(dimmer-restore-all)
  (dimmer-process-all))

(defun dimmer-kill-buffer-hook ()
  "Delete any saved state associated with the buffer."
  (remhash (current-buffer) dimmer-face-remaps))

;;;###autoload
(defun dimmer-activate ()
  "Activate the dimmer."
  (interactive)
  (add-hook 'post-command-hook 'dimmer-command-hook)
  (add-hook 'kill-buffer-hook 'dimmer-kill-buffer-hook)
  (add-hook 'window-configuration-change-hook 'dimmer-config-change-hook))

;;;###autoload
(defun dimmer-deactivate ()
  "Deactivate the dimmer and restore all buffers to normal faces."
  (interactive)
  (remove-hook 'post-command-hook 'dimmer-command-hook)
  (remove-hook 'kill-buffer-hook 'dimmer-kill-buffer-hook)
  (remove-hook 'window-configuration-change-hook 'dimmer-config-change-hook)
  (dimmer-restore-all))


;;; debugging - call from *scratch*, ielm, or eshell

(defun dimmer-debug-remaps (name &optional clear)
  "Display 'face-remapping-alist' for buffer NAME (or clear if CLEAR)."
  (with-current-buffer name
    (if clear
        (setq face-remapping-alist nil)
      face-remapping-alist)))

(defun dimmer-debug-hash (&optional name clear)
  "Display 'dimmer-face-remaps' for buffer NAME (or clear if CLEAR)."
  (if name
      (if clear
          (remhash (get-buffer name) dimmer-face-remaps)
        (gethash (get-buffer name) dimmer-face-remaps))
    dimmer-face-remaps))

(defun dimmer-debug-reset (name)
  "Clear 'face-remapping-alist' and 'dimmer-face-remaps' for NAME."
  (dimmer-debug-hash name t)
  (dimmer-debug-remaps name t)
  (redraw-display))


(provide 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dimmer.el ends here
