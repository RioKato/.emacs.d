
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

(defun get-network-address (interface-name)
  (let ((interface (network-interface-info interface-name)))
    (when interface
      (let ((ip (nth 0 interface))
            (mask (nth 2 interface))
            (result nil))
        (when (or ip mask)
          (format-network-address
           (apply 'vector
                  (reverse
                   (dotimes (i 4 result)
                     (push (logand (aref ip i)
                                   (aref mask i))
                           result))))))))))

(let ((address (or (get-network-address "en0")
                   (get-network-address "eth0"))))
  (when (and address
             (member address '("rio.local")))
    (setq url-proxy-services
          '(("http" . "10.221.237.10:8080")
            ("https" . "10.221.237.10:8080")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config.Packages.Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (require 'package nil t)
    (progn (add-to-list 'package-archives
                        '("melpa" . "https://melpa.org/packages/") t)
           (add-to-list 'package-archives
                        '("melpa-stable" . "https://stable.melpa.org/packages/") t)
           (add-to-list 'package-archives
                        '("marmalade" . "https://marmalade-repo.org/packages/") t)
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

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)

  (when (package-installed-p 'cl)
    (with-no-warnings
      (defun undo-tree-recover-window-init ()
        (defvar undo-tree-recover-window nil)
        (put 'undo-tree-recover-window (lambda () nil) t)
        (make-variable-buffer-local 'undo-tree-recover-window))

      (lexical-let ((undo-tree-visualize.old (symbol-function 'undo-tree-visualize)))
        (defun undo-tree-visualize ()
          (interactive)
          (let ((window-openp (= 1 (count-if-not 'window-dedicated-p (window-list)))))
            (funcall undo-tree-visualize.old)
            (setq undo-tree-recover-window
                  (if window-openp
                      (lexical-let ((current-window (get-buffer-window)))
                        (lambda ()
                          (delete-window current-window)))
                    (lambda () nil))))))

      (lexical-let ((undo-tree-visualizer-quit.old (symbol-function 'undo-tree-visualizer-quit)))
        (defun undo-tree-visualizer-quit ()
          (interactive)
          (let ((temp undo-tree-recover-window))
            (funcall undo-tree-visualizer-quit.old)
            (funcall temp))))

      (add-hook 'undo-tree-visualizer-mode-hook 'undo-tree-recover-window-init))))

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
;; Config.Packages.Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :config
  (setq eldoc-documentation-function
        (lambda ()
          (when (eql last-command-event 32)
            (let (eldoc-documentation-function)
              (eldoc-print-current-symbol-info))))))

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

(defun show-buffer-backward-search (name regexp &optional repeat)
  (interactive "b\ns\np")
  (let ((buffer (get-buffer name)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (if (re-search-backward regexp nil t repeat)
              (beginning-of-line)
            (goto-char (point-min)))
          (message "%s" (buffer-substring-no-properties (point) (point-max))))))))

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
      (show-buffer-backward-search "*inf-clojure*" "^[a-zA-Z0-9\.-_]*=>" 2)))
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
;; Config.Packages.Programming.Scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and window-system (eq 'darwin system-type))
  (setenv "SCALA_HOME" "/usr/local/java/scala/bin/")
  (setenv "SBT_HOME" "/usr/local/java/sbt/bin/")
  (setenv "PATH" (format "%s:%s:%s"
                         (getenv "PATH")
                         (getenv "SCALA_HOME")
                         (getenv "SBT_HOME")))
  (setq exec-path
        (append exec-path
                (list (getenv "SCALA_HOME")
                      (getenv "SBT_HOME")))))

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
;; Config.Packages.Programming.Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :if (executable-find "python")
  :config
  (setq python-indent-guess-indent-offset nil))

