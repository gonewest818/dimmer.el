;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run with:
;;    /usr/local/bin/emacs -q -l gallery.el --eval "(go :background 0.4 'solarized-dark)"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set user-emacs-directory to the directory where this file lives
;; do not read or write anything in $HOME/.emacs.d

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set custom-file to write into a separate place

(setq custom-file (concat user-emacs-directory ".custom.el"))
(when (file-readable-p custom-file) (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure melpa (and other repos if needed)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bootstrap `use-package'

(setq package-pinned-packages
      '((bind-key           . "melpa")
        (diminish           . "melpa")
        (use-package        . "melpa")))

(dolist (p (mapcar 'car package-pinned-packages))
  (unless (package-installed-p p)
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install and configure packages

;; https://emacsthemes.com/popular/index.html

(use-package zenburn-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  ;(load-theme 'solarized-dark t)
  ;(load-theme 'solarized-light t)
  )

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init
  ;(load-theme 'spacemacs-light)
  ;(load-theme 'spacemacs-dark)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  ;(load-theme sanityinc-tomorrow-day)
  ;(load-theme sanityinc-tomorrow-night)
  ;(load-theme sanityinc-tomorrow-blue)
  ;(load-theme sanityinc-tomorrow-bright)
  ;(load-theme sanityinc-tomorrow-eighties)
  )

(use-package dimmer
  :ensure t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-gallery (file &optional line)
  (with-current-buffer (find-file file)
    (setq buffer-file-name nil)
    (rename-buffer "dimmed")
    (delete-other-windows)
    (and line (goto-line line))
    (split-window-right))
  (with-current-buffer (find-file file)
    (setq buffer-file-name nil)
    (rename-buffer "default")
    (and line (goto-line line))))

(defun go-dimmer (mode frac)
  (setq dimmer-adjustment-mode mode)
  (setq dimmer-fraction frac)
  (dimmer-mode 1))

;; https://emacsthemes.com/popular/index.html
(defun go-theme (theme)
  (cond ((eq theme 'zenburn))
        ((eq theme 'solarized-dark)
         (load-theme 'solarized-dark t))
        ((eq theme 'solarized-light)
         (load-theme 'solarized-light t))
        ((eq theme 'spacemacs-dark)
         (load-theme 'spacemacs-dark t))
        ((eq theme 'spacemacs-light)
         (load-theme 'spacemacs-light t))
        ((eq theme 'tomorrow-day)
         (load-theme 'sanityinc-tomorrow-day t))
        ((eq theme 'tomorrow-night)
         (load-theme 'sanityinc-tomorrow-night t))
        ((eq theme 'tomorrow-blue)
         (load-theme 'sanityinc-tomorrow-blue t))
        ((eq theme 'tomorrow-bright)
         (load-theme 'sanityinc-tomorrow-bright t))
        ((eq theme 'tomorrow-eighties)
         (load-theme 'sanityinc-tomorrow-eighties t))))

(defun go (mode frac theme)
  (go-dimmer mode frac)
  (go-theme theme)
  (go-gallery "../dimmer.el" 310)
  (setq grab-file (format "example-%s-%s-%s.png"
                          theme
                          (substring (symbol-name mode) 1)
                          frac)))

(defun grab ()
  (interactive)
  (shell-command (format "screencapture -R0,80,1280,673 %s" (concat user-emacs-directory grab-file)))
  (kill-emacs))

(global-set-key (kbd "<f3>") #'grab)
