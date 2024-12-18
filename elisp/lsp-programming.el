(use-package morlock
  :ensure t
  :config
  (font-lock-add-keywords 'emacs-lisp-mode morlock-font-lock-keywords))

(use-package nix-mode
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package eglot
  :preface
  (defun company-backend-local-settings ()
    "Set the company backends so that lsp results are first, but
  code results are a backup. This allows for autocompletion
  without semantic evaluation, which can be useful if the lsp is
  busy or does not return results quickly enough."
    (setq-local company-backends
                '((company-capf :separate company-dabbrev-code)
                  company-files)))

  (defun maybe-start-eglot ()
    "Exlude some mode from eglot."
    (let ((disabled-modes '(emacs-lisp-mode dockerfile-ts-mode)))
      (unless (apply 'derived-mode-p disabled-modes)
        (eglot-ensure))))

  (defun duck/go-eglot-start()
    (define-key go-ts-mode-map
                ["RET"] 'newline-and-indent)
    (define-key go-ts-mode-map
                ["M-RET"] 'newline)
    (eglot-ensure))


  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e f f" . eglot-format)
              ("C-c e f b" . eglot-format-buffer)
              ("C-c e a" . eglot-code-actions))
  :hook
  (eglot-managed-mode . flymake-mode)
  (eglot-managed-mode . company-backend-local-settings)
  (python-mode . eglot-ensure)
  (java-mode . eglot-ensure)
  :custom
  (fset #'jsonrpc--log-event #'ignore)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
  (eglot-extend-to-xref t)
  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))

  :config
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))

  ;; (add-to-list 'eglot-server-programs
  ;;              '(astro-ts-mode . ("astro-ls" "--stdio"
  ;;                              :initializationOptions
  ;;                              (:typescript (:tsdk "/home/iannn/.local/share/pnpm/global/5/node_modules/typescript/lib")))))
  (setq eglot-stay-out-of '(company-backends))

  :hook
  ((python-ts-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (nix-mode . eglot-ensure)
   ;; (astro-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package astro-ts-mode
  :ensure t
  :config
  (require 'astro-ts-mode)
  (defun astro-ts-comment-dwim (args)
    (interactive "*P")
    (let ((lang (astro-ts-mode--treesit-language-at-point
                 (if (region-active-p)
                     (save-mark-and-excursion
                       (deactivate-mark)
                       (goto-char (region-beginning))
                       (beginning-of-line)
                       (point))
                   (line-beginning-position)))))
      (cond ((or (eq lang 'tsx) (string-match-p "frontmatter" (if (stringp (treesit-inspect-node-at-point)) (treesit-inspect-node-at-point) "")))
             (setq-local comment-start "//")
             (setq-local comment-end "")
             (comment-dwim args)
             )
            ((eq lang 'css)
             (setq-local comment-start "/*")
             (setq-local comment-end "*/")
             (comment-dwim args)
             )
            ((eq lang 'astro)
             (setq-local comment-start "<!--")
             (setq-local comment-end "--!>")
             (comment-dwim args)
             )
            )))
  (define-key astro-ts-mode-map (kbd "M-;") #'astro-ts-comment-dwim))


(use-package envrc
  :ensure t
  :init
  (envrc-global-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :bind
  (:map go-ts-mode-map
        ("C-c g a" . treesit-beginning-of-defun)
        ("C-c g e" . treesit-end-of-defun)
        ("C-c g i" . prog-indent-sexp)
        ("RET"     . newline-and-indent)
        ("M-RET"   . newline)
        )
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.local/bin")
  :hook
  (go-ts-mode . eglot-ensure))

(use-package godoctor
  :ensure t)

(provide 'lsp-programming)
