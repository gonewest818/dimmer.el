;;; dimmer.el --- visually highlight the selected buffer
;; 
;; Filename: dimmer.el
;; Author: Neil Okamoto
;; Version: 0.2.0-SNAPSHOT
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
;; The percentage of dimming is user configurable.
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
(require 'color)
(require 'face-remap)
(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration

(defgroup dimmer nil
  "Highlight current-buffer by dimming faces on the others."
  :prefix "dimmer-"
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/gonewest818/dimmer.el"))

(defcustom dimmer-percent 0.20
  "Control the degree to which buffers are dimmed (0.0 - 1.0)."
  :type '(float)
  :group 'dimmer)

(defcustom dimmer-exclusion-regexp nil
  "Regular expression describing buffer names that are never dimmed."
  :type '(regexp)
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

(defun dimmer-invert-p ()
  "Determine if the dimmed faces should be brighter instead of darker."
  (let* ((fg (face-foreground 'default))
         (fgm (apply 'max (color-name-to-rgb fg)))
         (bg (face-background 'default))
         (bgm (apply 'max (color-name-to-rgb bg))))
    (> bgm fgm)))

(defun dimmer-compute-rgb (c pct invert)
  "Computes the color C when dimmed by percentage PCT.
When INVERT is true, make the value brighter rather than darker."
  (apply 'color-rgb-to-hex
         (mapcar
          (if invert
              (lambda (x) (- 1.0 (* (- 1.0 x) (- 1.0 pct))))
            (lambda (x) (* x (- 1.0 pct))))
          (color-name-to-rgb c))))

(defun dimmer-face-color (f pct invert)
  "Compute a dimmed version of the foreground color of face F.
PCT is the amount of dimming where 0.0 is no change and 1.0 is
maximum change.  When INVERT is not nil, invert the scaling
for dark-on-light themes."
  (let ((key (concat (symbol-name f) "-"
                     (number-to-string pct)
                     (if invert "-t" "-nil"))))
    (or (gethash key dimmer-dimmed-faces)
        (let ((fg (face-foreground f)))
          (when fg  ; e.g. "(when-let* ((fg (...)))" in Emacs 26+
            (let ((rgb (dimmer-compute-rgb fg pct invert)))
              (puthash key rgb dimmer-dimmed-faces)
              rgb))))))

(defun dimmer-dim-buffer (buf pct invert)
  "Dim all the faces defined in the buffer BUF.
PCT and INVERT controls the dimming as defined
in ‘dimmer-face-color’."
  (with-current-buffer buf
    (unless dimmer-buffer-face-remaps
      (dolist (f (face-list))
        (let ((c (dimmer-face-color f pct invert)))
          (when c  ; e.g. "(when-let* ((c (...)))" in Emacs 26
            (setq dimmer-buffer-face-remaps
                  (cons (face-remap-add-relative f :foreground c)
                        dimmer-buffer-face-remaps))))))))

(defun dimmer-restore-buffer (buf)
  "Restore the un-dimmed faces in the buffer BUF."
  (with-current-buffer buf
    (when dimmer-buffer-face-remaps
      (dolist (c dimmer-buffer-face-remaps)
        (face-remap-remove-relative c))
      (setq dimmer-buffer-face-remaps nil))))

(defun dimmer-filtered-buffer-list ()
  "Get filtered subset of all visible buffers in the current frame."
  (let (buffers)
    (walk-windows
     (lambda (win)
       (let* ((buf (window-buffer win))
              (name (buffer-name buf)))
         (unless (and dimmer-exclusion-regexp
                      (string-match-p dimmer-exclusion-regexp name))
           (push buf buffers)))))
    buffers))

(defun dimmer-process-all ()
  "Process all buffers and dim or un-dim each."
  (let ((selected (current-buffer)))
    (setq dimmer-last-buffer selected)
    (dolist (buf (dimmer-filtered-buffer-list))
      (if (eq buf selected)
          (dimmer-restore-buffer buf)
        (dimmer-dim-buffer buf dimmer-percent (dimmer-invert-p))))))

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
        (add-hook 'post-command-hook 'dimmer-command-hook)
        (add-hook 'window-configuration-change-hook 'dimmer-config-change-hook))
    (remove-hook 'post-command-hook 'dimmer-command-hook)
    (remove-hook 'window-configuration-change-hook 'dimmer-config-change-hook)
    (dimmer-restore-all)))

;;;###autoload
(defun dimmer-activate ()
  "Activate the dimmer."
  (interactive)
  (dimmer-mode 1))

;;;###autoload
(defun dimmer-deactivate ()
  "Deactivate the dimmer and restore all buffers to normal faces."
  (interactive)
  (dimmer-mode 0))

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
