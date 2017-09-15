
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
(display-time)

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

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)
(global-linum-mode t)
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq exec-path
      '("/bin"
        "/usr/bin"
        "/usr/local/bin"))

(setenv "PATH"
        (cond ((null exec-path) "")
              ((null (cdr exec-path)) (car exec-path))
              (:else
               (let ((path (car exec-path))
                     (temp (cdr exec-path)))
                 (while temp
                   (setq path (format "%s:%s" path (car temp)))
                   (setq temp (cdr temp)))
                 path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-t") 'repeat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Keymap.MAC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (define-key key-translation-map [165] [92]))

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
;; Config.Network.Proxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (member (system-name) '())
;;   (setq url-proxy-services
;;         '(("http" . "10.221.237.10:8080")
;;           ("https" . "10.221.237.10:8080"))))

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package migemo
  :ensure t
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

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
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection)))))

  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (global-set-key (kbd "C-i") 'company-indent-or-complete-common))

(use-package auto-complete
  :disabled t
  :ensure t
  :config
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-auto-show-menu 0.5)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

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
  :disabled t
  :ensure t
  :config
  (load-theme 'atom-dark t))

(use-package pastelmac-theme
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
;; Config.Packages.Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :config
  (setq eldoc-documentation-function
        (lambda ()
          (when (eql last-command-event 32)
            (let (eldoc-documentation-function)
              (eldoc-print-current-symbol-info))))))

(use-package flycheck
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package ac-slime
  :ensure t
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (add-to-list 'ac-modes 'slime-repl-mode))

(use-package slime-company
  :disabled t
  :ensure t)

(use-package slime
  :ensure t
  :if inferior-lisp-program
  :config
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-banner slime-fancy slime-repl)))

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
  :disabled t
  :ensure t
  :if (executable-find "lein")
  :config
  (setq inf-clojure-prompt-read-only t)
  ;; (add-hook 'inf-clojure-mode-hook 'company-mode)
  ;; (add-hook 'inf-clojure-minor-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'inf-clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode))

(use-package javadoc-lookup
  :disabled t
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "C-c j") 'javadoc-lookup))

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scheme
  :config
  (when (executable-find "gosh")
    (setq scheme-program-name "gosh -i"))
  (add-hook 'scheme-mode-hook 'show-paren-mode)
  (add-hook 'scheme-mode-hook 'showmatch-minor-mode)
  (add-hook 'scheme-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (align-range :all 'indent-region)
                          nil t))
              (define-key scheme-mode-map (kbd "C-c C-c")
                (lambda ()
                  (interactive)
                  (align-range :defun 'scheme-send-region)))
              (put 'when 'scheme-indent-function 1)
              (put 'unless 'scheme-indent-function 1)
              (put 'dolist 'scheme-indent-function 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode
  :disabled t
  :ensure t
  :if (executable-find "scala"))

(use-package ensime
  :ensure t
  :if (and (executable-find "scala")
           (executable-find "sbt"))
  :config
  (add-hook 'scala-mode-hook 'ensime-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
  (add-hook 'haskell-mode-hook 'inf-haskell-mode)
  (when (and (package-installed-p 'flycheck)
             (executable-find "hlint"))
    (add-hook 'haskell-mode-hook
              (lambda ()
                (setq flycheck-checker 'haskell-hlint)
                (flycheck-mode 1)))))

(use-package ghc
  :ensure t
  :if (executable-find "ghc-mod"))

(use-package cl-lib
  :ensure t)

(use-package company-ghc
  :disabled t
  :ensure t
  :if (and (package-installed-p 'ghc)
           (package-installed-p 'cl-lib))
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-to-list 'company-backends
                           'company-ghc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :if (executable-find "python")
  :config
  (setq python-indent-guess-indent-offset nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Programming.SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sql
  :ensure t)

(use-package sql-indent
  :ensure t
  :if (package-installed-p 'sql)
  :config
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (when (executable-find "mysql")
    (sql-set-product "mysql")))

