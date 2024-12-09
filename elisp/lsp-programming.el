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
  ;; (global-treesit-auto-mode)
)

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
   (go-ts-mode . eglot-ensure)
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

(provide 'lsp-programming)
