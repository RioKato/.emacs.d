

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
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-x C-b") 'bs-show)
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
  (setq mac-allow-anti-aliasing t)
  (set-face-attribute 'default nil
                      :family "Anonymous Pro"
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

(use-package sr-speedbar
  :ensure t
  :config
  (sr-speedbar-close)
  (setq sr-speedbar-delete-windows t)
  (setq speedbar-use-images nil)
  (setq sr-speedbar-width 25)
  (setq sr-speedbar-max-width 25)
  (setq sr-speedbar-auto-refresh t)
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-right-side nil)
  (sr-speedbar-open)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
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

(use-package eldoc
  :config
  (setq eldoc-documentation-function
        (lambda ()
          (when (eql last-command-event 32)
            (let (eldoc-documentation-function)
              (eldoc-print-current-symbol-info))))))

(defgroup showmatch nil "vimlike showmatch"
  :group 'emacs)

(defcustom showmatch-idle-delay 0.4
  "showmatch idele delay time"
  :type 'number
  :group 'showmatch)

(defmacro define-key-showmatch (map chr)
  `(define-key ,map (kbd ,chr)
     (lambda ()
       (interactive)
       (insert ,chr)
       (save-excursion
         (ignore-errors
           (backward-sexp))
         (sit-for showmatch-idle-delay)))))

(define-minor-mode showmatch-minor-mode
  "vimlike showmatch"
  :lighter "showmatch"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key-showmatch map ")")
    (define-key-showmatch map "]")
    (define-key-showmatch map "}")
    (identity map)))

(defun show-tail-of-buffer (name &optional n)
  (interactive "b\nn")
  (setq n (or n 1))
  (let ((buffer (get-buffer name)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (end-of-buffer)
          (dotimes (i n)
            (ignore-errors
              (previous-line)))
          (beginning-of-line)
          (message (buffer-substring-no-properties (point) (point-max))))))))

(defun align-range (range-type f)
  (cond ((eq :all range-type)
         (save-excursion
           (funcall f (point-min) (point-max))))
        ((eq :defun range-type)
         (save-excursion
           (end-of-defun)
           (let ((end (point)) (case-fold-search t))
             (beginning-of-defun)
             (funcall f (point) end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Lisp.CL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (align-range :all 'indent-region))
                        nil t)))
  (add-hook 'lisp-mode-hook 'show-paren-mode)
  (add-hook 'lisp-mode-hook 'showmatch-minor-mode))

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
  (add-hook 'lisp-mode-hook 'slime-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Lisp.Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (align-range :all 'indent-region)
                          (align-range :all 'clojure-align))
                        nil t)))
  (add-hook 'clojure-mode-hook 'show-paren-mode)
  (add-hook 'clojure-mode-hook 'showmatch-minor-mode))

(use-package inf-clojure
  :ensure t
  :if (executable-find "lein")
  :config
  (define-key clojure-mode-map (kbd "C-c C-d")
    (lambda ()
      (interactive)
      (show-tail-of-buffer "*inf-clojure*" 5)))
  (setq inf-clojure-prompt-read-only t)
  (add-hook 'inf-clojure-mode-hook 'company-mode)
  (add-hook 'inf-clojure-minor-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'inf-clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode))

(use-package javadoc-lookup
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "C-c j") 'javadoc-lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :if (executable-find "python")
  :config
  (setq python-indent-guess-indent-offset nil))

