(use-package outline
  :ensure nil
  :hook (prog-mode . outline-minor-mode))

(use-package text-mode
  :ensure nil
  :config
  (modify-syntax-entry ?\" "\"" text-mode-syntax-table))

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout nil)
  :hook
  ((emacs-lisp-mode . flymake-mode))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package imenu
  :ensure nil
  :custom (imenu-space-replacement nil))

(use-package recentf
  :ensure nil
  :bind
  (("C-x C-r" . recentf)
   ("C-x M-r"  . find-file-read-only))
  :config
  (setq recentf-max-saved-items 200
	recentf-auto-cleanup 300
	recentf-exclude (list "/\\.git/.*\\'" ; Git contents
	 		      "/elpa/.*\\'"   ; Package files
	 		      ".*\\.gz\\'"
	 		      "TAGS"
	 		      ".*-autoloads\\.el\\'"
	 		      "ido.last"))
  (recentf-mode))

(use-package vc-hooks
  :ensure nil
  :config
  (setq vc-follow-symlinks nil))

(use-package hippie-expand
  :bind
  ("M-?" . hippie-expand))

(use-package project
  :ensure nil
  :config
  (setq project-mode-line t)
  (setq xref-search-program 'ripgrep)
  (setq-default project-vc-ignores '("target/" "bin/" "obj/" "elpa/" "eln-cache/" "etc/" "var/" "share/" "tree-sitter/" "eglot*/"))
  (setq-default project-vc-extra-root-markers '("pom.xml" "*.csproj"))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))

(use-package info
  :ensure nil
  :bind
  (:map Info-mode-map
        ("{" . backward-paragraph)
        ("}" . forward-paragraph))
  :hook ((Info-mode . variable-pitch-mode)))


(use-package doc-view
  :ensure nil
  :config
  (setq doc-view-continuous t))

(use-package proced
  :ensure nil
  :commands proced
  :custom
  ;; (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm)))
  (defun my-format-java-args (args)
	(pcase-let* ((base (proced-format-args args))
				 (`(,exe . ,rest) (split-string base))
				 (exe-prop
                  (if (string= exe "java")
                      (propertize exe 'font-lock-face '((t (:foreground "#f89820"))))
					exe)))
      (mapconcat #'identity (cons exe-prop rest) " ")))
  (setf (alist-get 'args proced-grammar-alist)
		'("Args"               ; name of the column
          my-format-java-args  ; format function
          left                 ; alignment within column
          proced-string-lessp  ; defines the sort method (ascending)
          nil                  ; non-nil reverses sort order
		  (args pid)            ; sort scheme
		  (nil t nil)))         ; refiner for custom refinement logic - see proced-refine
  )

(use-package dired
  :ensure nil
  :hook
  (dired-mode . hl-line-mode)
  :config
  (eval-after-load "dired"
	#'(lambda ()
	 	(put 'dired-find-alternate-file 'disabled nil)
	 	(define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)))
  ;; (setq image-dired-thumb-margin 5)
  ;; (setq image-dired-external-viewer "/usr/bin/qimgv")
  (setq delete-by-moving-to-trash t)
  :custom
  (dired-listing-switches "-al --group-directories-first")
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top))

(use-package image-mode
  :ensure nil
  :config
  (setq image-animate-loop t))

(use-package tramp
  :ensure nil
  :preface
  (defun sudo-edit (&optional arg)
	"Edit currently visited file as ARG root.

	   With a prefix ARG prompt for a file to visit.
	   Will also prompt for a file to visit if current
	   buffer is not visiting a file."
	(interactive "P")
	(if (or arg (not buffer-file-name))
	 	(counsel-find-file (concat "/sudo:root@localhost:"
	 							   (read-file-name "Find file(as root): ")))
	  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
  :config
  (customize-set-variable 'tramp-default-method "ssh")
  :bind
  ("C-x C-a C-f" . sudo-edit))

(use-package eshell
  :ensure nil
  :preface
  (defvar my-ansi-escape-re
    (rx (or ?\233 (and ?\e ?\[))
        (zero-or-more (char (?0 . ?\?)))
        (zero-or-more (char ?\s ?- ?\/))
        (char (?@ . ?~))))

  ;; :preface
  ;; (defun chunyang-eshell-mode-setup ()
  ;; 	(remove-hook 'eshell-output-filter-functions
  ;; 	 			 'eshell-postoutput-scroll-to-bottom))
  ;; (defun rm-comint-postoutput-scroll-to-bottom ()
  ;; 	(remove-hook 'comint-output-filter-functions
  ;; 	 			 'comint-postoutput-scroll-to-bottom))
  ;; (defun duckonomy/eshell-clear-prompt ()
  ;; 	"Clear `eshell' buffer, comint-style."
  ;; 	(interactive)
  ;; 	(let ((input (eshell-get-old-input)))
  ;; 	  (eshell/clear-scrollback)))
  ;; ;; this works best :)
  ;; (defun duckonomy/eshell-clear ()
  ;; 	"Clear `eshell' buffer, comint-style."
  ;; 	(interactive)
  ;; 	(let ((input (eshell-get-old-input)))
  ;; 	  (eshell/clear-scrollback)
  ;; 	  (eshell-emit-prompt)
  ;; 	  (insert input)))
  ;; (defun shortened-path (path max-len)
  ;; 	"Return a modified version of PATH, replacing some components.
  ;; 	 with single characters starting from the left to try and get
  ;; 	 the path down to MAX-LEN"
  ;; 	(let* ((components (split-string (abbreviate-file-name path) "/"))
  ;; 	 	   (len (+ (1- (length components))
  ;; 	 			   (reduce '+ components :key 'length)))
  ;; 	 	   (str ""))
  ;; 	  (while (and (> len max-len)
  ;; 	 			  (cdr components))
  ;; 	 	(setq str (concat str (if (= 0 (length (car components)))
  ;; 	 							  "/"
  ;; 	 							(string (elt (car components) 0) ?/)))
  ;; 	 		  len (- len (1- (length (car components))))
  ;; 	 		  components (cdr components)))
  ;; 	  (concat str (reduce (lambda (a b) (concat a "/" b)) components))))
  ;; (defun eshell-new()
  ;; 	"Open a new instance of eshell."
  ;; 	(interactive)
  ;; 	(eshell 'N))
  :config
  (setq-default eshell-cmpl-cycle-completions nil)
  (setq-default eshell-buffer-maximum-lines 20000)
  (setq-default eshell-history-size 350)
  (setq-default eshell-hist-ignoredups t)
  (setq-default eshell-buffer-shorthand t)
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "dnf")
              (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
              (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
              (add-to-list 'eshell-visual-commands "top")))
  ;; (setq-default eshell-highlight-prompt nil)
  ;; (setq-default eshell-plain-echo-behavior t)
  (setq-default eshell-scroll-to-bottom-on-output nil)
  (setenv "PAGER" "cat")
  (setq
   comint-preoutput-filter-functions
   '((lambda (output)
       (replace-regexp-in-string "\033\[[0-9]+[GK]" "" output))))

  ;; (setq-default comint-move-point-for-output t)
  ;; (setq-default eshell-prompt-function
  ;;       			(lambda ()
  ;;       			  (concat
  ;;       			   (propertize "┌─[" 'face `(:foreground "#61AFEF"))
  ;;       			   ;; (propertize (concat (eshell/pwd)) 'face `(:foreground "#56B6C2"))
  ;;       			   (propertize (shortened-path (eshell/pwd) 40) 'face `(:foreground "#56B6C2"))

  ;;       			   (if (magit-get-current-branch)
  ;;       				   (concat
  ;;       					(propertize "@" 'face `(:foreground "#98C379"))
  ;;       					(propertize (magit-get-current-branch) 'face `(:foreground "#98C379")))
  ;;       				 "")
  ;;       			   (propertize "]──[" 'face `(:foreground "#61AFEF"))
  ;;       			   (propertize (format-time-string "%I:%M %p" (current-time)) 'face `(:foreground "#D56871"))
  ;;       			   (propertize "]\n" 'face `(:foreground "#61AFEF"))
  ;;       			   (propertize "└─>" 'face `(:foreground "#61AFEF"))
  ;;       			   (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "#E5C07B")))))
  :hook
  (
   ;; (eshell-mode . chunyang-eshell-mode-setup)
   ;; (eshell-mode . (lambda()
   ;; 	 				(local-set-key (kbd "C-l") 'duckonomy/eshell-clear)))
   ;; (eshell-mode . (lambda()
   ;; 	 				(local-set-key (kbd "<tab>") 'completion-at-point)))
   ;; (comint-mode . rm-comint-postoutput-scroll-to-bottom)
   (eshell-banner-load . (lambda () (setq eshell-banner-message "")))))

;; (use-package eldoc
;;   :init
;;   (global-eldoc-mode +1)
;;   :config
;;   (setq eldoc-idle-delay 0.4))

(use-package which-key
  :ensure nil
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-separator " → " )
  (setq which-key-prefix-prefix "+" )
  (which-key-mode t))

(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1))

(provide 'core-packages)
