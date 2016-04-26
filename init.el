

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment 'Japanese)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq completion-ignore-case t)
(when (fboundp 'global-auto-revert-mode)
  (global-auto-revert-mode 1))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.System.MAC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq 'darwin system-type)
  (prefer-coding-system 'utf-8)
  (setq-default locale-coding-system 'utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.System.Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq 'windows-nt system-type)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-buffer-file-coding-system 'utf-8-unix)
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (setq file-name-coding-system 'shift_jis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables '(tab-width 4))
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)
(global-linum-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map [?¥] [?\\])
(global-set-key (kbd "C-x C-b") 'bs-show)
(when (fboundp 'winner-mode)
  (winner-mode t)
  (global-set-key (kbd "C-x 4") 'winner-undo)
  (global-set-key (kbd "C-x 5") 'winner-redo))
(when window-system
  (global-set-key (kbd "<C-return>") 'toggle-frame-maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Keymap.MAC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (set-frame-parameter nil 'alpha 95))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.GUI.MAC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and window-system (eq 'darwin system-type))
  (setenv "LANG" "ja_JP.UTF-8")
  (setenv "PATH" "/bin:/usr/bin:/usr/local/bin")
  (setq exec-path '("/bin" "/usr/bin" "/usr/local/bin"))
  (setq line-spacing 2)
  (setq mac-allow-anti-aliasing t)
  (set-face-attribute 'default nil
                      :family "Courier New"
                      :height 150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.GUI.Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and window-system (eq 'windows-nt system-type))
  (set-face-attribute 'default nil
                      :family "Courier New"
                      :height 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Network.SB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (member (system-name) '("rio.local"))
  (setq url-proxy-services
        '(("http" . "10.221.237.10:8080")
          ("https" . "10.221.237.10:8080"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (require 'package nil t)
    (progn (add-to-list 'package-archives
                        '("melpa" . "http://melpa.org/packages/") t)
           (add-to-list 'package-archives
                        '("melpa-stable" . "http://stable.melpa.org/packages/") t)
           (add-to-list 'package-archives
                        '("marmalade" . "http://marmalade-repo.org/packages/") t)
           (package-initialize)
           (unless (package-installed-p 'use-package)
             (package-refresh-contents)
             (package-install 'use-package))
           (require 'use-package))
  (defmacro use-package (&rest args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl
  :ensure t)

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode t)
  (ido-everywhere t))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  (("M-x" . smex)))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  :bind
  (("C-x o" . switch-window)))

(use-package smooth-scroll
  :ensure t
  :config
  (setq smooth-scroll/vscroll-step-size 4)
  (setq smooth-scroll/hscroll-step-size 4)
  (smooth-scroll-mode t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (global-set-key (kbd "C-i") 'company-indent-or-complete-common))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package atom-dark-theme
  :ensure t
  :config
  (load-theme 'atom-dark t))

(use-package pastelmac-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'pastelmac t))

(use-package autumn-light-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'autumn-light t))

(use-package paper-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'paper t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.EWW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eww
  :config
  (setq eww-search-prefix "https://www.google.co.jp/search?q="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.CL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook 'paredit-mode))

(setq inferior-lisp-program
      (or (executable-find "sbcl")
          (executable-find "ccl")
          (executable-find "ccl64")
          (executable-find "dx86cl")
          (executable-find "dx86cl64")))

(use-package slime-company
  :ensure t
  :if inferior-lisp-program)

(use-package slime
  :ensure t
  :if inferior-lisp-program
  :config
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-repl slime-banner slime-fancy slime-company))
  (define-key slime-mode-map (kbd "C-c C-l")
    (lambda ()
      (interactive)
      (call-interactively 'save-buffer)
      (call-interactively 'slime-load-file)))
  (define-key slime-repl-mode-map (kbd "C-c C-r") 'slime-restart-inferior-lisp)
  (define-key slime-repl-mode-map (kbd "C-c C-q") 'slime-repl-quit)
  (add-hook 'lisp-mode-hook 'slime-mode)
  :mode
  (("\\.lisp$" . lisp-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package cider
  :ensure t
  :if (executable-find "lein")
  :config
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-buffer-name-show-port t)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-use-pretty-printing t)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (define-key cider-mode-map (kbd "C-c C-b") 'cider-interrupt)
  (define-key cider-repl-mode-map (kbd "C-c C-b") 'cider-interrupt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :if (executable-find "python")
  :config
  (setq python-indent-guess-indent-offset nil)
  (define-key python-mode-map (kbd "C-c C-l")
    (lambda ()
      (interactive)
      (call-interactively 'save-buffer)
      (call-interactively 'python-shell-send-file))))
