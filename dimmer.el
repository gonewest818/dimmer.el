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

(require 'subr-x)
(require 'face-remap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration

(defcustom dimmer-percent 0.20
  "Control the degree to which buffers are dimmed (0.0 - 1.0)."
  :type '(float)
  :group 'dimmer)

(defcustom dimmer-invert nil
  "Invert the dimming for dark-on-light themes."
  :type '(boolean)
  :group 'dimmer)

(defcustom dimmer-exclusion-regexp nil
  "Regular expression describing buffer names that are never dimmed."
  :type '(regexp)
  :group 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compatibility needed as long as we support emacs-version < 26

(eval-and-compile

  (unless (fboundp 'if-let*)
    (defmacro if-let* (bindings then &rest else)
      "Process BINDINGS and if all values are non-nil eval THEN, else ELSE.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in THEN, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
      (declare (indent 2)
               (debug ([&or (&rest (symbolp form)) (symbolp form)] form body)))
      (when (and (<= (length bindings) 2)
                 (not (listp (car bindings))))
        ;; Adjust the single binding case
        (setq bindings (list bindings)))
      `(let* ,(internal--build-bindings bindings)
         (if ,(car (internal--listify (car (last bindings))))
             ,then
           ,@else))))

  (unless (fboundp 'when-let*)
    (defmacro when-let* (bindings &rest body)
      "Process BINDINGS and if all values are non-nil eval BODY.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in BODY, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
      (declare (indent 1) (debug if-let*))
      `(if-let* ,bindings ,(macroexp-progn body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation

(defvar-local dimmer-buffer-face-remaps nil
  "Per-buffer face remappings needed for later clean up.")

(defconst dimmer-dimmed-faces (make-hash-table :test 'equal)
  "Cache of face names with their computed dimmed values.")

(defconst dimmer-last-buffer nil
  "Identity of the last buffer to be made current.")

(defun dimmer-compute-rgb (c pct invert)
  "Computes the color C when dimmed by percentage PCT.
When INVERT is true, make the value brighter rather than darker."
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
        (when-let* ((fg (face-foreground f)))
          (let* ((rgb (dimmer-compute-rgb fg pct invert)))
            (puthash key rgb dimmer-dimmed-faces)
            rgb)))))

(defun dimmer-dim-buffer (buf pct invert)
  "Dim all the faces defined in the buffer BUF.
PCT and INVERT controls the dimming as defined
in ‘dimmer-face-color’."
  (with-current-buffer buf
    (unless dimmer-buffer-face-remaps
      (dolist (f (face-list))
        (when-let* ((c (dimmer-face-color f pct invert)))
          (setq dimmer-buffer-face-remaps
                (cons (face-remap-add-relative f :foreground c)
                      dimmer-buffer-face-remaps)))))))

(defun dimmer-restore-buffer (buf)
  "Restore the un-dimmed faces in the buffer BUF."
  (with-current-buffer buf
    (when dimmer-buffer-face-remaps
      (dolist (c dimmer-buffer-face-remaps)
        (face-remap-remove-relative c))
      (setq dimmer-buffer-face-remaps nil))))

(defun dimmer-filtered-buffer-list ()
  "Get filtered subset of all buffers."
  (seq-filter
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (not (or (eq ?\s (elt name 0)) ; leading space
                (and dimmer-exclusion-regexp
                     (string-match-p dimmer-exclusion-regexp name))))))
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
  (dimmer-process-all))

;;;###autoload
(defun dimmer-activate ()
  "Activate the dimmer."
  (interactive)
  (add-hook 'post-command-hook 'dimmer-command-hook)
  (add-hook 'window-configuration-change-hook 'dimmer-config-change-hook))

;;;###autoload
(defun dimmer-deactivate ()
  "Deactivate the dimmer and restore all buffers to normal faces."
  (interactive)
  (remove-hook 'post-command-hook 'dimmer-command-hook)
  (remove-hook 'window-configuration-change-hook 'dimmer-config-change-hook)
  (dimmer-restore-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; debugging - call from *scratch*, ielm, or eshell

(defun dimmer--debug-remaps (name &optional clear)
  "Display 'face-remapping-alist' for buffer NAME (or clear if CLEAR)."
  (with-current-buffer name
    (if clear
        (setq face-remapping-alist nil)
      face-remapping-alist)))

(defun dimmer--debug-hash (name &optional clear)
  "Display 'dimmer-buffer-face-remaps' for buffer NAME (or clear if CLEAR)."
  (with-current-buffer name
    (if clear
        (setq dimmer-buffer-face-remaps nil)
      dimmer-buffer-face-remaps)))

(defun dimmer--debug-reset (name)
  "Clear 'face-remapping-alist' and 'dimmer-buffer-face-remaps' for NAME."
  (dimmer--debug-hash name t)
  (dimmer--debug-remaps name t)
  (redraw-display))


(provide 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dimmer.el ends here
