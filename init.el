;;; Init.el --- Load all modules -*- lexical-binding: t; -*-

;;; Commentary:
;;; Include all the batteries

;;; Code:

;;;; Profiling and Debug
(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; (setq-default debug-on-error (and (not noninteractive) emacs-debug-mode))
;; (setq warning-minimum-level :emergency)



;; Elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.


(set-face-attribute 'default nil
		    :weight 'normal
		    :height 120
		    :slant 'normal
		    :family "Iosevka")
(set-face-attribute 'variable-pitch nil
		    :weight 'normal
		    :height 1.0
		    :slant 'normal
		    :family "Lexend")
(set-face-attribute 'fixed-pitch nil
		    :weight 'normal
		    :height 1.0
		    :slant 'normal
		    :family "Iosevka")
(set-fontset-font t 'hangul
	 	  (font-spec
	 	   :name "Noto Sans CJK KR"
	 	   :weight 'bold))

(defun duckonomy/keys-decode (&optional frame)
  (when frame
    (select-frame frame))

  (define-key input-decode-map (kbd "C-[") (kbd "<C-[>"))
  (define-key input-decode-map (kbd "C-i") (kbd "<C-i>"))
  (define-key input-decode-map (kbd "C-m") (kbd "<C-m>"))
  (define-key input-decode-map [?\C-i] [C-i])
  (define-key input-decode-map [?\C-\S-i] [C-S-i])
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'keyboard-quit)

  (defadvice keyboard-escape-quit
      (around keyboard-escape-quit-dont-close-windows activate)
    (let ((buffer-quit-function (lambda () ())))
      ad-do-it))


  (setq default-input-method "korean-hangul")
  (global-set-key (kbd "<C-[>") 'toggle-input-method)
  )



(if (daemonp)
    (progn
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (with-selected-frame frame
		    (duckonomy/keys-decode))))))
(duckonomy/keys-decode)



(let ((default-directory (locate-user-emacs-file "elisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'core)
(require 'core-packages)
(require 'appearance)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; ;; (add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
;; ;; (load-theme 'my-dark-theme t)
;; ;; (add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

;; (use-package clowns
;;   :load-path "elisp/themes"
;;   ;; :config
;;   ;; (load-theme 'clowns t)
;;   )
;; (require 'clowns)
(require 'search-menu)
(require 'files-folders)
(require 'snippets-and-completion)
(require 'editing)
(require 'bindings-settings)
(require 'lsp-programming)
(require 'version-control)
(require 'shell-terminal)
(require 'org-settings)
(require 'apps)









;;; Packages
;; (require 'package)

;; ;; Add melpa
;; (add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

;; Precompute activation actions to speed up startup.
;; (setq package-quickstart t)
;; (defun my-native-recompile ()
;;   "Prune eln cache and native recompile everything on `package-user-dir'."
;;   (interactive)
;;   (native-compile-prune-cache)
;;   (native-compile-async package-user-dir 'recursively))
;; (my-native-recompile)

;; (setq package-native-compile t)
;; (setq native-comp-jit-compilation t)

;; (require 'use-package)




;; (cl-defun iannn/vc-install (&key (fetcher "github") repo name rev backend)
;;   "Install a package from a remote if it's not already installed.
;; This is a thin wrapper around `package-vc-install' in order to
;; make non-interactive usage more ergonomic.  Takes the following
;; named arguments:

;; - FETCHER the remote where to get the package (e.g., \"gitlab\").
;;   If omitted, this defaults to \"github\".

;; - REPO should be the name of the repository

;; - NAME, REV, and BACKEND are as in `package-vc-install' (which
;;   see)."
;;   (let* ((url (format "https://www.%s.com/%s" fetcher repo))
;;          (iname (when name (intern name)))
;;          (pac-name (or iname (intern (file-name-base repo)))))
;;     (unless (package-installed-p pac-name)
;;       (package-vc-install url iname rev backend))))

;; (use-package diminish :ensure t :defer t)

;; (add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;; (load "iannn-default")
;; (load "iannn-appearance")
;; (load "iannn-menu")
;; (load "iannn-editing")
;; (load "iannn-completion")

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; ;; (add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
;; ;; (load-theme 'my-dark-theme t)
;; ;; (add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

;; ;; (use-package my-dark-theme
;; ;;   :load-path "themes"
;; ;;   ;; :config
;; ;;   (load-theme 'my-dark t)
;; ;;   )

;; (load "iannn-navigation")
;; (load "iannn-snippets")
;; (load "iannn-dired")
;; (load "iannn-eshell")
;; (load "iannn-formatting")
;; (load "iannn-org")
;; (load "iannn-vc")
;; (load "iannn-misc")
;; (load "iannn-mail")
;; (add-to-list 'load-path (expand-file-name "lisp/programming" user-emacs-directory))
;; (load "iannn-lsp")
;; (load "iannn-java")
;; ;; (load "iannn-go")
;; ;; (load "iannn-python")
;; (load "iannn-treesitter")


;; (use-package vim-tab-bar
;;   :ensure t
;;   :commands vim-tab-bar-mode
;;   :hook
;;   (after-init . vim-tab-bar-mode))



;; (use-package yasnippet
;;   :ensure t
;;   :bind
;;   (("<C-i>" . yas-expand))
;;   :init
;;   (yas-global-mode 1))


;; (use-package yasnippet-snippets
;;   :ensure t
;;   :defer t)



;; ;; (use-package yasnippet-capf
;; ;;   :ensure t
;; ;;   :after cape
;; ;;   :config
;; ;;   (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; ;; (use-package elcord
;; ;;   :ensure t
;; ;;   :init
;; ;;   (elcord-mode)
;; ;;   :config
;; ;;   (setq-default elcord-display-buffer-details nil))

;; ;; (add-to-list 'load-path (expand-file-name "lisp/lsp-bridge" user-emacs-directory))
;; ;; (require 'lsp-bridge)
;; ;; (require 'lsp)
;; ;; ;; ;; TODO: move this to somewhere else
;; ;; (use-package lsp-bridge
;; ;;   ;; :ensure t
;; ;;   ;; :init (iannn/vc-install :fetcher "github" :repo "https://github.com/manateelazycat/lsp-bridge")
;; ;;   :hook
;; ;;   ((java-ts-mode . lsp-bridge-mode)
;; ;;    (python-ts-mode . lsp-bridge-mode))
;; ;;   :bind (:map lsp-bridge-mode-map
;; ;;               ("M-." . lsp-bridge-find-def)
;; ;;               ("M-," . lsp-bridge-find-def-return)
;; ;;               ("C-." . lsp-bridge-peek)
;; ;;               ("C-c l a" . lsp-bridge-code-action)
;; ;;               ("C-c l f" . lsp-bridge-code-format)
;; ;;               ("C-c l r" . lsp-bridge-rename)
;; ;;               ("M-n" . lsp-bridge-diagnostic-jump-next)
;; ;;               ("M-p" . lsp-bridge-diagnostic-jump-prev)))

;; (defun set-bigger-spacing ()
;;   (setq-local default-text-properties '(line-spacing 0.25 line-height 1.2)))
;; (add-hook 'org-mode-hook 'set-bigger-spacing)
;; (add-hook 'eww-mode-hook 'set-bigger-spacing)
;; ;; (add-hook 'text-mode-hook 'set-bigger-spacing)
;; (add-hook 'Info-mode-hook 'set-bigger-spacing)

;; (setq-default hangul-im-keymap nil)



;; ;; (use-package niceify-info
;; ;;   :ensure t
;; ;;   :config
;; ;;   (add-hook 'Info-selection-hook
;; ;;             #'niceify-info))

;; ;; (use-package info+
;; ;;   :load-path "~/.emacs.d/lisp/info+/"
;; ;;   :config
;; ;;   (add-hook 'Info-mode-hook 'Info-variable-pitch-text-mode)
;; ;;   (add-hook 'Info-mode-hook 'Info-persist-history-mode))


;; ;; (set-bigger-spacing)
;; (require 'pdf-tools)
;; ;; TODO
;; ;; https://github.com/Qkessler/consult-project-extra
;; ;; TODO for big projects
;; ;; https://github.com/universal-ctags/citre
;; ;; https://github.com/radian-software/apheleia
;; ;; https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
;; ;; TODO Emacs lsp indexing optimization
;; ;; (make indexing work better for emacs-core so that it works like intellij products)
;; ;; Configure
;; ;; https://www.boot.dev/
;; (use-package geiser
;;   :ensure t
;;   )

;; (use-package geiser-guile
;;   :ensure t
;;   :config
;;   (setq geiser-guile-binary "guile3.0")
;;   (setq geiser-repl-binary "guile3.0")
;;   )

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
