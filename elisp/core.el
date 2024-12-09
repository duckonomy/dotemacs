(use-package emacs
  :ensure nil
  :config
  (setq show-help-function nil)
  (setq indicate-empty-lines nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; Truncate end symbol (default $)
  (set-display-table-slot standard-display-table 0 ?\ )
  (setq ring-bell-function #'ignore)
  (setq visible-bell nil)
  (setq visible-cursor nil)
  (setq x-stretch-cursor t)
  (setq cursor-in-non-selected-windows nil)
  (setq-default menu-bar-mode nil)
  (setq debugger-stack-frame-as-list t)
  (setq use-dialog-box nil)
  (setopt use-short-answers t)
  (setq words-include-escapes t)
  (setq bidi-display-reordering nil)
  (setq-default redisplay-dont-pause t)
  (setq-default display-line-numbers-width nil)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq-default javascript-indent-level 2)
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default css-indent-offset 2)
  (setq-default typescript-indent-level 2)
  (setq-default visual-fill-column-center-text nil)
  (setq-default visual-fill-column-width fill-column)
  (setq-default history-delete-duplicates t)
  (setq history-length 1000)
  (setq auto-window-vscroll nil)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t)
  (setq y-or-n-p-use-read-key t)
  (setq scroll-margin 10)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 10) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  (setq highlight-nonselected-windows nil)
  (setq max-mini-window-height 0.3)
  (setq resize-mini-windows 'grow-only)
  (setq frame-resize-pixelwise t)
  (setq sentence-end-double-space nil)
  ;; (setq mode-line-default-help-echo nil)
  (setq auto-save-list-file-prefix nil)
  (setq uniquify-buffer-name-style 'forward)
  (setq indicate-buffer-boundaries nil)
  (setq frame-title-format
	'(buffer-file-name "%f"
	 		   (dired-directory dired-directory "%b")))
  (setq-default
   fringe-indicator-alist
   (delq (assq 'continuation fringe-indicator-alist)
	 fringe-indicator-alist))
  (setq-default
   default-frame-alist
   '((horizontal-scroll-bars . nil)
     (internal-border-width . 20)
     (left-fringe . 0)
     (right-fringe . 0)
     (bottom-divider-width . 0)
     (right-divider-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (undecorated . t)
     (vertical-scroll-bars . nil)))
  :init
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(use-package simple
  :ensure nil
  :init
  (column-number-mode 1)
  :hook
  (before-save . delete-trailing-whitespace)
  :bind
  ("C-/" . undo-only)
  :config
  (setq-default indent-tabs-mode nil)
  (setq tab-always-indent 'complete)
  (setq-default kill-do-not-save-duplicates t)
  (setq save-interprogram-paste-before-kill t)
  (setq set-mark-command-repeat-pop t)
  (setq backward-delete-char-untabify-method 'hungry)
  (setq kill-ring-max 3000)
  (setq next-line-add-newlines nil))

(use-package format
  :ensure nil
  :hook
  ((text-mode . (lambda ()
	 	  (setq truncate-lines nil
	 	        global-visual-line-mode t
                        global-visual-wrap-prefix-mode t
	 	        word-wrap t)))
   (prog-mode . (lambda ()
	 	  (setq truncate-lines t
	 	        global-visual-line-mode nil
                        global-visual-wrap-prefix-mode nil
	 	        word-wrap nil)))))

(use-package window
  :ensure nil
  :preface
  (defun prev-window ()
    "Previous window."
    (interactive)
    (other-window -1))
  :bind
  (("C-x O" . prev-window))
  :config
  (setq recenter-positions '(top middle bottom))
  (setq split-width-threshold nil)
  (setq scroll-error-top-bottom t))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width-start t))

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset 4))

(use-package paren
  :ensure nil
  :init
  (show-paren-mode 1)
  :custom
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen t)
  (show-paren-delay 0))

(use-package emacs-news-view-modeslkdfjlsjdf
  :disabled
  :ensure nil
  :config
  ;; NEWS files use single quotes around elisp symbols. I think those
  ;; are the only files I view in outline-mode, but if I find others
  ;; then I might modify the syntax only locally in NEWS files.
  (modify-syntax-entry ?' "\"" emacs-news-view-mode-syntax-table))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

(use-package tool-bar
  :ensure nil
  :init
  (tool-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (setq debug-on-quit nil))

(use-package fringe
  :ensure nil
  :init
  (set-fringe-style -1)
  (fringe-mode '(0 . 0)))

(use-package tooltip
  :ensure nil
  :init
  (tooltip-mode -1)
  :config
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-use-echo-area t))

(use-package minibuffer
  :ensure nil
  :bind
  (:map minibuffer-local-completion-map
        ("<backtab>" . minibuffer-force-complete)
        ("SPC") ("?"))
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (resize-mini-windows t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :config
  (mapc (lambda (x)
	  (add-to-list 'completion-ignored-extensions x))
	'(".aux" ".bbl" ".blg" ".exe"
	  ".log" ".meta" ".out" ".pdf"
	  ".synctex.gz" ".tdo" ".toc"
	  "-pkg.el" "-autoloads.el"
	  "Notes.bib" "auto/"
	  ".o" ".elc" "~" ".bin"
	  ".class" ".exe" ".ps"
	  ".abs" ".mx" ".~jv" ".rbc"
	  ".pyc" ".beam" ".aux" ".out"
	  ".pdf" ".hbc"))
  (defun stealthily (fn &rest args)
    "Apply FN to ARGS while inhibiting modification hooks."
    (let ((inhibit-modification-hooks t))
      (apply fn args)))
  (advice-add 'minibuf-eldef-setup-minibuffer :around #'stealthily))


(use-package frame
  :ensure nil
  :init
  (blink-cursor-mode -1)
  :bind
  ("C-z" . nil)
  :custom
  (window-divider-mode t)
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1))

(use-package jit-lock
  :ensure nil
  :config
  (setq jit-lock-defer-time nil)
  (setq jit-lock-stealth-nice 0.1)
  (setq jit-lock-stealth-time 0.2)
  (setq jit-lock-stealth-verbose nil))

(use-package autorevert
  :ensure nil
  :hook
  (dired-mode . auto-revert-mode))

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file "~/.emacs.d/custom.el"))

(use-package files
  :ensure nil
  :config
  (setq-default make-backup-files nil)
  (setq auto-save-default nil)
  (setq-default cache-long-line-scans t)
  (setq view-read-only t)
  (setq backup-directory-alist
 	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
 	`((".*" ,temporary-file-directory t)))
  (setq backup-by-copying t)
  (setq confirm-kill-emacs nil)
  (setq create-lockfiles nil)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq version-control t))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
	savehist-autosave-interval 60
	savehist-save-minibuffer-history t))

(use-package novice
  :ensure nil
  :config
  (setq disabled-command-function nil))

(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode))

(use-package face-remap
  :ensure nil
  :preface
  (defun set-bigger-spacing ()
    (setq-local default-text-properties '(line-spacing 0.25 line-height 1.2)))
  :hook (((eww-mode Info-mode org-mode) . set-bigger-spacing)
         ((Info-mode-hook org-mode) . 'variable-pitch-mode)))

(use-package delsel
  :hook ((after-init . delete-selection-mode)))

(use-package windmove
  :ensure nil
  :bind
  (("C-x M-j" . windmove-down)
   ("C-x M-k" . windmove-up)
   ("C-x M-h" . windmove-left)
   ("C-x M-l" . windmove-right)))

;; (use-package tab-bar
;;   :ensure nil
;;   :config
;;   (setq tab-bar-close-button-show nil)
;;   (setq tab-bar-new-button-show nil)
;;   (setq tab-bar-position nil)
;;   (setq tab-line-close-button-show nil)
;;   (setq tab-line-new-button-show nil))

(provide 'core)