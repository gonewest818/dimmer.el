;;; dimmer.el --- Visually highlight the selected buffer

;; Copyright (C) 2017-2021 Neil Okamoto

;; Filename: dimmer.el
;; Author: Neil Okamoto
;; Version: 0.4.2
;; Package-Requires: ((emacs "25.1"))
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
;; `dimmer.el' can be configured to adjust foreground colors (default),
;; background colors, or both.
;;
;; Usage:
;;
;;      (require 'dimmer)
;;      (dimmer-configure-which-key)
;;      (dimmer-configure-helm)
;;      (dimmer-mode t)
;;
;; Configuration:
;;
;; By default dimmer excludes the minibuffer and echo areas from
;; consideration, so that most packages that use the minibuffer for
;; interaction will behave as users expect.
;;
;; `dimmer-configure-company-box' is a convenience function for users
;; of company-box.  It prevents dimming the buffer you are editing when
;; a company-box popup is displayed.
;;
;; `dimmer-configure-helm' is a convenience function for helm users to
;; ensure helm buffers are not dimmed.
;;
;; `dimmer-configure-gnus' is a convenience function for gnus users to
;; ensure article buffers are not dimmed.
;;
;; `dimmer-configure-hydra' is a convenience function for hydra users to
;; ensure  "*LV*" buffers are not dimmed.
;;
;; `dimmer-configure-magit' is a convenience function for magit users to
;; ensure transients are not dimmed.
;;
;; `dimmer-configure-org' is a convenience function for org users to
;; ensure org-mode buffers are not dimmed.
;;
;; `dimmer-configure-posframe' is a convenience function for posframe
;; users to ensure posframe buffers are not dimmed.
;;
;; `dimmer-configure-which-key' is a convenience function for which-key
;; users to ensure which-key popups are not dimmed.
;;
;; Please submit pull requests with configurations for other packages!
;;
;; Customization:
;;
;; `dimmer-adjustment-mode' controls what aspect of the color scheme is adjusted
;; when dimming.  Choices are :foreground (default), :background, or :both.
;;
;; `dimmer-fraction' controls the degree to which buffers are dimmed.
;; Range is 0.0 - 1.0, and default is 0.20.  Increase value if you
;; like the other buffers to be more dim.
;;
;; `dimmer-buffer-exclusion-regexps' can be used to specify buffers that
;; should never be dimmed.  If the buffer name matches any regexp in
;; this list then `dimmer.el' will not dim that buffer.
;;
;; `dimmer-buffer-exclusion-predicates' can be used to specify buffers that
;; should never be dimmed.  If any predicate function in this list
;; returns true for the buffer then `dimmer.el' will not dim that buffer.
;;
;; `dimmer-prevent-dimming-predicates' can be used to prevent dimmer from
;; altering the dimmed buffer list.  This can be used to detect cases
;; where a package pops up a window temporarily, and we don't want the
;; dimming to change.  If any function in this list returns a non-nil
;; value, dimming state will not be changed.
;;
;; `dimmer-watch-frame-focus-events' controls whether dimmer will dim all
;; buffers when Emacs no longer has focus in the windowing system.  This
;; is enabled by default.  Some users may prefer to set this to nil, and
;; have the dimmed / not dimmed buffers stay as-is even when Emacs
;; doesn't have focus.
;;
;; `dimmer-use-colorspace' allows you to specify what color space the
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
;;; customization

(defgroup dimmer nil
  "Highlight the current buffer by dimming the colors on the others."
  :prefix "dimmer-"
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/gonewest818/dimmer.el"))

(define-obsolete-variable-alias 'dimmer-percent 'dimmer-fraction "0.2.2")
(defcustom dimmer-fraction 0.20
  "Control the degree to which buffers are dimmed (0.0 - 1.0)."
  :type '(float)
  :group 'dimmer)

(defcustom dimmer-adjustment-mode :foreground
  "Control what aspect of the color scheme is adjusted when dimming.
Choices are :foreground (default), :background, or :both."
  :type '(radio (const :tag "Foreground colors are dimmed" :foreground)
                (const :tag "Background colors are dimmed" :background)
                (const :tag "Foreground and background are dimmed" :both))
  :group 'dimmer)

(make-obsolete-variable
 'dimmer-exclusion-regexp
 "`dimmer-exclusion-regexp` is obsolete and has no effect in this session.
The variable has been superseded by `dimmer-buffer-exclusion-regexps`.
See documentation for details."
 "v0.4.0")

(define-obsolete-variable-alias
  'dimmer-exclusion-regexp-list 'dimmer-buffer-exclusion-regexps "0.4.2")
(defcustom dimmer-buffer-exclusion-regexps '("^ \\*Minibuf-[0-9]+\\*$"
                                             "^ \\*Echo.*\\*$")
  "List of regular expressions describing buffer names that are never dimmed."
  :type '(repeat (choice regexp))
  :group 'dimmer)

(defcustom dimmer-buffer-exclusion-predicates '()
  "List of predicate functions indicating buffers that are never dimmed.

Functions in the list are called while visiting each available
buffer.  If the predicate function returns a truthy value, then
the buffer is not dimmed."
  :type '(repeat (choice function))
  :group 'dimmer)

(define-obsolete-variable-alias
  'dimmer-exclusion-predicates 'dimmer-prevent-dimming-predicates "0.4.0")
(defcustom dimmer-prevent-dimming-predicates '(window-minibuffer-p)
  "List of functions which prevent dimmer from altering dimmed buffer set.

Functions in this list are called in turn with no arguments.  If any function
returns a non-nil value, no buffers will be added to or removed from the set
of dimmed buffers."
  :type '(repeat (choice function))
  :group 'dimmer)

(defcustom dimmer-watch-frame-focus-events t
  "Should windows be dimmed when all Emacs frame(s) lose focus?

Restart Emacs after changing this configuration.
When configuring dimmer in your init scripts, please be sure to
change this setting before calling the function `dimmer-mode'."
  :type '(boolean)
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
wrong, then try HSL or RGB instead."
  :type '(radio (const :tag "Interpolate in CIELAB 1976" :cielab)
                (const :tag "Interpolate in HSL" :hsl)
                (const :tag "Interpolate in RGB" :rgb))
  :group 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration

(defun dimmer-company-box-p ()
  "Return non-nil if current buffer is a company box buffer."
  (string-prefix-p " *company-box-" (buffer-name)))

;;;###autoload
(defun dimmer-configure-company-box ()
  "Convenience setting for company-box users.
This predicate prevents dimming the buffer you are editing when
company-box pops up a list of completion."
  (add-to-list
   'dimmer-prevent-dimming-predicates #'dimmer-company-box-p))

;;;###autoload
(defun dimmer-configure-helm ()
  "Convenience settings for helm users."
  (with-no-warnings
    (add-to-list
     'dimmer-exclusion-regexp-list "^\\*[h|H]elm.*\\*$")
    (add-to-list
     'dimmer-prevent-dimming-predicates #'helm--alive-p)))

;;;###autoload
(defun dimmer-configure-gnus ()
  "Convenience settings for gnus users."
  (add-to-list
   'dimmer-exclusion-regexp-list "^\\*Article .*\\*$"))

;;;###autoload
(defun dimmer-configure-hydra ()
  "Convenience settings for hydra users."
  (add-to-list
   'dimmer-exclusion-regexp-list "^ \\*LV\\*$"))

;;;###autoload
(defun dimmer-configure-magit ()
  "Convenience settings for magit users."
  (add-to-list
   'dimmer-exclusion-regexp-list "^ \\*transient\\*$"))

;;;###autoload
(defun dimmer-configure-org ()
  "Convenience settings for org users."
  (add-to-list 'dimmer-exclusion-regexp-list "^\\*Org Select\\*$")
  (add-to-list 'dimmer-exclusion-regexp-list "^ \\*Agenda Commands\\*$"))

;;;###autoload
(defun dimmer-configure-posframe ()
  "Convenience settings for packages depending on posframe.

Note, packages that use posframe aren't required to be consistent
about how they name their buffers, but many of them tend to
include the words \"posframe\" and \"buffer\" in the buffer's
name.  Examples include:

  - \" *ivy-posframe-buffer*\"
  - \" *company-posframe-buffer*\"
  - \" *flycheck-posframe-buffer*\"
  - \" *ddskk-posframe-buffer*\"

If this setting doesn't work for you, you still have the option
of adding another regular expression to catch more things, or
in some cases you can customize the other package and ensure it
uses a buffer name that fits this pattern."
  (add-to-list
   'dimmer-exclusion-regexp-list "^ \\*.*posframe.*buffer.*\\*$"))

;;;###autoload
(defun dimmer-configure-which-key ()
  "Convenience settings for which-key-users."
  (with-no-warnings
    (add-to-list
     'dimmer-exclusion-regexp-list "^ \\*which-key\\*$")
    (add-to-list
     'dimmer-prevent-dimming-predicates #'which-key--popup-showing-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation

(defvar dimmer-last-buffer nil
  "Identity of the last buffer to be made current.")

(defvar dimmer-debug-messages 0
  "Control debugging output to *Messages* buffer.
Set 0 to disable all output, 1 for basic output, or a larger
integer for more verbosity.")

(defvar-local dimmer-buffer-face-remaps nil
  "Per-buffer face remappings needed for later clean up.")
;; don't allow major mode change to kill the local variable
(put 'dimmer-buffer-face-remaps 'permanent-local t)

(defconst dimmer-dimmed-faces (make-hash-table :test 'equal)
  "Cache of face names with their computed dimmed values.")

(defun dimmer-lerp (frac c0 c1)
  "Use FRAC to compute a linear interpolation of C0 and C1."
  (+ (* c0 (- 1.0 frac))
     (* c1 frac)))

(defun dimmer-lerp-in-rgb (c0 c1 frac)
  "Compute linear interpolation of C0 and C1 in RGB space.
FRAC controls the interpolation."
  (apply 'color-rgb-to-hex
         (cl-mapcar (apply-partially 'dimmer-lerp frac) c0 c1)))

(defun dimmer-lerp-in-hsl (c0 c1 frac)
  "Compute linear interpolation of C0 and C1 in HSL space.
FRAC controls the interpolation."
  ;; Implementation note: We must handle this case carefully to ensure the
  ;; hue is interpolated over the "shortest" arc around the color wheel.
  (apply 'color-rgb-to-hex
         (apply 'color-hsl-to-rgb
                (cl-destructuring-bind (h0 s0 l0)
                    (apply 'color-rgb-to-hsl c0)
                  (cl-destructuring-bind (h1 s1 l1)
                      (apply 'color-rgb-to-hsl c1)
                    (if (> (abs (- h1 h0)) 0.5)
                        ;; shortest arc "wraps around"
                        (list (mod (dimmer-lerp (- 1.0 frac) h1 (+ 1.0 h0)) 1.0)
                              (dimmer-lerp frac s0 s1)
                              (dimmer-lerp frac l0 l1))
                      ;; shortest arc is the natural one
                      (list (dimmer-lerp frac h0 h1)
                            (dimmer-lerp frac s0 s1)
                            (dimmer-lerp frac l0 l1))))))))

(defun dimmer-lerp-in-cielab (c0 c1 frac)
  "Compute linear interpolation of C0 and C1 in CIELAB space.
FRAC controls the interpolation."
  (apply 'color-rgb-to-hex
         (cl-mapcar 'color-clamp
                    (apply 'color-lab-to-srgb
                           (cl-mapcar (apply-partially 'dimmer-lerp frac)
                                      (apply 'color-srgb-to-lab c0)
                                      (apply 'color-srgb-to-lab c1))))))

(defun dimmer-compute-rgb (c0 c1 frac colorspace)
  "Compute a \"dimmed\" color via linear interpolation.

Blends the two colors, C0 and C1, using FRAC to control the
interpolation. When FRAC is 0.0, the result is equal to C0.  When
FRAC is 1.0, the result is equal to C1.

Any other value for FRAC means the result's hue, saturation, and
value will be adjusted linearly so that the color sits somewhere
between C0 and C1.

The interpolation is performed in a COLORSPACE which is specified
with a symbol, :rgb, :hsl, or :cielab."
  (pcase colorspace
    (:rgb    (dimmer-lerp-in-rgb c0 c1 frac))
    (:hsl    (dimmer-lerp-in-hsl c0 c1 frac))
    (:cielab (dimmer-lerp-in-cielab c0 c1 frac))
    (_       (dimmer-lerp-in-cielab c0 c1 frac))))

(defun dimmer-cached-compute-rgb (c0 c1 frac colorspace)
  "Lookup a \"dimmed\" color value from cache, else compute a value.
This is essentially a memoization of `dimmer-compute-rgb` via a hash
using the arguments C0, C1, FRAC, and COLORSPACE as the key."
  (let ((key (format "%s-%s-%f-%s" c0 c1 frac dimmer-use-colorspace)))
    (or (gethash key dimmer-dimmed-faces)
        (let ((rgb (dimmer-compute-rgb (color-name-to-rgb c0)
                                       (color-name-to-rgb c1)
                                       frac
                                       dimmer-use-colorspace)))
          (when rgb
            (puthash key rgb dimmer-dimmed-faces)
            rgb)))))

(defun dimmer-face-color (f frac)
  "Compute a dimmed version of the foreground color of face F.
If `dimmer-adjust-background-color` is true, adjust the
background color as well.  FRAC is the amount of dimming where
0.0 is no change and 1.0 is maximum change.  Returns a plist
containing the new foreground (and if needed, new background)
suitable for use with `face-remap-add-relative`."
  (let ((fg (face-foreground f))
        (bg (face-background f))
        (def-fg (face-foreground 'default))
        (def-bg (face-background 'default))
        ;; when mode is :both, the perceptual effect is "doubled"
        (my-frac (if (eq dimmer-adjustment-mode :both)
                     (/ frac 2.0)
                   frac))
        (result '()))
    ;; We shift the desired components of F by FRAC amount toward the `default`
    ;; color, thereby dimming or desaturating the overall appearance:
    ;;   * When the `dimmer-adjustment-mode` is `:foreground` we move the
    ;;     foreground component toward the `default` background.
    ;;   * When the `dimmer-adjustment-mode` is :background we mofe the
    ;;     background component of F toward the `default` foreground.`
    (when (and (or (eq dimmer-adjustment-mode :foreground)
                   (eq dimmer-adjustment-mode :both))
               fg (color-defined-p fg)
               def-bg (color-defined-p def-bg))
      (setq result
            (plist-put result :foreground
                       (dimmer-cached-compute-rgb fg
                                                  def-bg
                                                  my-frac
                                                  dimmer-use-colorspace))))
    (when (and (or (eq dimmer-adjustment-mode :background)
                   (eq dimmer-adjustment-mode :both))
               bg (color-defined-p bg)
               def-fg (color-defined-p def-fg))
      (setq result
            (plist-put result :background
                       (dimmer-cached-compute-rgb bg
                                                  def-fg
                                                  my-frac
                                                  dimmer-use-colorspace))))
    result))

(defun dimmer-filtered-face-list ()
  "Return a filtered version of `face-list`.
Filtering is needed to exclude faces that shouldn't be dimmed."
  ;; `fringe` is problematic because it is shared for all windows,
  ;; so for now we just leave it alone.
  (remove 'fringe (face-list)))

(defun dimmer-dim-buffer (buf frac)
  "Dim all the faces defined in the buffer BUF.
FRAC controls the dimming as defined in ‘dimmer-face-color’."
  (with-current-buffer buf
    (dimmer--dbg 1 "dimmer-dim-buffer: BEFORE '%s' (%s)" buf
                 (alist-get 'default face-remapping-alist))
    (dimmer--dbg 2 "dimmer-buffer-face-remaps: %s"
                 (alist-get 'default dimmer-buffer-face-remaps))
    (unless dimmer-buffer-face-remaps
      (dolist (f (dimmer-filtered-face-list))
        (let ((c (dimmer-face-color f frac)))
          (when c  ; e.g. "(when-let* ((c (...)))" in Emacs 26
            (push (face-remap-add-relative f c) dimmer-buffer-face-remaps)))))
    (dimmer--dbg 2 "dimmer-buffer-face-remaps: %s"
                 (alist-get 'default dimmer-buffer-face-remaps))
    (dimmer--dbg 2 "dimmer-dim-buffer: AFTER '%s' (%s)" buf
                 (alist-get 'default face-remapping-alist))))

(defun dimmer-restore-buffer (buf)
  "Restore the un-dimmed faces in the buffer BUF."
  (with-current-buffer buf
    (dimmer--dbg 1 "dimmer-restore-buffer: BEFORE '%s' (%s)" buf
                 (alist-get 'default face-remapping-alist))
    (dimmer--dbg 2 "dimmer-buffer-face-remaps: %s"
                 (alist-get 'default dimmer-buffer-face-remaps))
    (when dimmer-buffer-face-remaps
      (mapc 'face-remap-remove-relative dimmer-buffer-face-remaps)
      (setq dimmer-buffer-face-remaps nil))
    (dimmer--dbg 2 "dimmer-buffer-face-remaps: %s"
                 (alist-get 'default dimmer-buffer-face-remaps))
    (dimmer--dbg 2 "dimmer-restore-buffer: AFTER '%s' (%s)" buf
                 (alist-get 'default face-remapping-alist))))

(defun dimmer-visible-buffer-list ()
  "Get all visible buffers in all frames."
  (let (buffers)
    (walk-windows
     (lambda (win)
       (let ((buf (window-buffer win)))
         (unless (member buf buffers)   ; ensure items are unique
           (push buf buffers))))
     nil
     t)
    (dimmer--dbg 3 "dimmer-visible-buffer-list: %s" buffers)
    buffers))

(defun dimmer-filtered-buffer-list (&optional buffer-list)
  "Get filtered subset of all visible buffers in all frames.
If BUFFER-LIST is provided by the caller, then filter that list."
  (let ((buffers
         (seq-filter
          (lambda (buf)
            ;; This filter function REMOVES any buffer if:
            ;;    (a) one of the dimmer-buffer-exclusion-regexps matches
            ;; OR (b) one of the dimmer-buffer-exclusion-predicates is true
            (let ((name (buffer-name buf)))
              (not (or (cl-some (lambda (rxp) (string-match-p rxp name))
                                dimmer-buffer-exclusion-regexps)
                       (cl-some (lambda (f) (funcall f buf))
                                dimmer-buffer-exclusion-predicates)))))
          (or buffer-list (dimmer-visible-buffer-list)))))
    (dimmer--dbg 3 "dimmer-filtered-buffer-list: %s" buffers)
    buffers))

(defun dimmer-process-all (&optional force)
  "Process all buffers and dim or un-dim each.

When FORCE is true some special logic applies.  Namely, we must
process all buffers regardless of the various dimming predicates.
While performing this scan, any buffer that would have been
excluded due to the predicates before should be un-dimmed now."
  (dimmer--dbg-buffers 1 "dimmer-process-all")
  (let* ((selected (current-buffer))
         (ignore   (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                            dimmer-prevent-dimming-predicates))
         (visbufs  (dimmer-visible-buffer-list))
         (filtbufs (dimmer-filtered-buffer-list visbufs)))
    (dimmer--dbg 1 "dimmer-process-all: force %s" force)
    (setq dimmer-last-buffer selected)
    (when (or force (not ignore))
      (dolist (buf (if force visbufs filtbufs))
        (dimmer--dbg 2 "dimmer-process-all: buf %s" buf)
        (if (or (eq buf selected)
                (and force (not (memq buf filtbufs))))
            (dimmer-restore-buffer buf)
          (dimmer-dim-buffer buf dimmer-fraction))))))

(defun dimmer-dim-all ()
  "Dim all buffers."
  (dimmer--dbg-buffers 1 "dimmer-dim-all")
  (mapc (lambda (buf)
          (dimmer-dim-buffer buf dimmer-fraction))
        (dimmer-visible-buffer-list)))

(defun dimmer-restore-all ()
  "Un-dim all buffers."
  (dimmer--dbg-buffers 1 "dimmer-restore-all")
  (mapc 'dimmer-restore-buffer (buffer-list)))

(defun dimmer-command-handler ()
  "Process all buffers if current buffer has changed."
  (dimmer--dbg-buffers 1 "dimmer-command-handler")
  (unless (eq (window-buffer) dimmer-last-buffer)
    (dimmer-process-all)))

(defun dimmer-config-change-handler ()
  "Process all buffers if window configuration has changed."
  (dimmer--dbg-buffers 1 "dimmer-config-change-handler")
  (dimmer-process-all t))

(defun dimmer-after-focus-change-handler ()
  "Handle cases where a frame may have gained or last focus.
Walk the `frame-list` and check the state of each one.  If none
of the frames has focus then dim them all.  If any frame has
focus then dim the others.  Used in Emacs >= 27.0 only."
  (dimmer--dbg-buffers 1 "dimmer-after-focus-change-handler")
  (let ((focus-out t))
    (with-no-warnings
      (dolist (f (frame-list) focus-out)
        (setq focus-out (and focus-out (not (frame-focus-state f))))))
    (if focus-out
        (dimmer-dim-all)
      (dimmer-process-all t))))

(defun dimmer-manage-frame-focus-hooks (install)
  "Manage the frame focus in/out hooks for dimmer.

When INSTALL is t, install the appropriate hooks to catch focus
events.  Otherwise remove the hooks.  This function has no effect
when `dimmer-watch-frame-focus-events` is nil."
  (when dimmer-watch-frame-focus-events
    (if (boundp 'after-focus-change-function)
        ;; emacs-version >= 27.0
        (if install
            (add-function :before
                          after-focus-change-function
                          #'dimmer-after-focus-change-handler)
          (remove-function after-focus-change-function
                           #'dimmer-after-focus-change-handler))
      ;; else emacs-version < 27.0
      (if install
          (with-no-warnings
            (add-hook 'focus-in-hook #'dimmer-config-change-handler)
            (add-hook 'focus-out-hook #'dimmer-dim-all))
        (with-no-warnings
          (remove-hook 'focus-in-hook #'dimmer-config-change-handler)
          (remove-hook 'focus-out-hook #'dimmer-dim-all))))))

;;;###autoload
(define-minor-mode dimmer-mode
  "Visually highlight the selected buffer."
  :init-value nil
  :lighter ""
  :global t
  :group 'dimmer
  (if dimmer-mode
      (progn
        (dimmer-manage-frame-focus-hooks t)
        (add-hook 'post-command-hook #'dimmer-command-handler)
        (add-hook 'window-configuration-change-hook
                  #'dimmer-config-change-handler))
    (dimmer-manage-frame-focus-hooks nil)
    (remove-hook 'post-command-hook #'dimmer-command-handler)
    (remove-hook 'window-configuration-change-hook
                 #'dimmer-config-change-handler)
    (dimmer-restore-all)))

;;;###autoload
(define-obsolete-function-alias 'dimmer-activate 'dimmer-mode "0.2.0")

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

(defun dimmer--dbg (v fmt &rest args)
  "Print debug message at verbosity V, filling format string FMT with ARGS."
  (when (>= dimmer-debug-messages v)
    (apply #'message fmt args)))

(defun dimmer--dbg-buffers (v label)
  "Print debug buffer state at verbosity V and the given LABEL."
  (when (>= dimmer-debug-messages v)
    (let ((inhibit-message t)
          (cb (current-buffer))
          (wb (window-buffer)))
      (message "%s: cb '%s' <== lb '%s' %s" label cb dimmer-last-buffer
               (if (not (eq cb wb))
                   (format "wb '%s' **" wb)
                 "")))))

(provide 'dimmer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dimmer.el ends here
