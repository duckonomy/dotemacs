(use-package imenu-list
  :ensure t
  :hook
  (imenu-list-after-jump . recenter-top-bottom)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-after-jump-hook nil))

(use-package orderless
  :ensure t
  :config
  (defun prefixes-for-separators (pattern _index _total)
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  :custom
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators)))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 15)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-preselect 'directory)
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  :init
  (vertico-mode)
  ;; bind this to TAB in vertico-map if needed
  (defun my-vertico-insert-unless-tramp ()
    "Insert current candidate in minibuffer, except for tramp."
    (interactive)
    (if (vertico--remote-p (vertico--candidate))
        (minibuffer-complete)
      (vertico-insert)))

  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
	 (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
	 (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
		 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  (defun my/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :bind
  (:map vertico-map
        ("C-<return>" . vertico-really-exit-input)
        ("DEL" . vertico-directory-delete-char)
        ("C-M-d" . consult-dir)
        ("C-M-j" . consult-dir-jump-file)
        ("/" . #'my/vertico-insert)
        ("M-q" . vertico-quick-jump))
  :config
  (defalias 'vertico-really-exit-input #'exit-minibuffer)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
		     (propertize "» " 'face 'vertico-current)
                   "  ")
		 cand))))

;; (use-package vertico-posframe
;;   :ensure t
;;   :init
;;   (vertico-posframe-mode 1))

(use-package prescient
  :ensure t)

(use-package corfu-prescient
  :after prescient
  :ensure t
  :config
  (corfu-prescient-mode 1))

(use-package vertico-prescient
  :ensure t
  :after prescient
  :config
  (vertico-prescient-mode 1)

  (setq prescient-filter-method '(literal initialism prefix regexp)
        prescient-use-char-folding t
        prescient-use-case-folding 'smart
        prescient-sort-full-matches-first t
        prescient-sort-length-enable t)

  (prescient-persist-mode 1))

(use-package vertico-directory
  :after vertico
  :demand
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :ensure t
  :preface

  (defun my/consult-project-ripgrep (&optional dir initial)
    (interactive "P")
    (let (dirs (consult--project-root))
      (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dirs initial)))

  (defun my/consult-project-external-ripgrep (&optional dir initial)
    (interactive "P")
    (let (dirs (project-external-roots (project-current)))
      (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dirs initial)))

  (defun my/consult-fd (&optional dir initial)
    (interactive "P")
    (let ((dirs (append (list (consult--project-root))
                        (project-external-roots (project-current)))))
      (consult-fd dirs initial)))
  :bind
  (("M-g l" . consult-line)
   ("M-g L" . consult-line-multi)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-g o" . consult-outline)
   ("M-g a" . consult-org-agenda)
   ("M-g m" . consult-mark)
   ("M-g K" . consult-keep-lines)
   ("M-g k" . consult-global-mark)
   ("M-X" . consult-mode-command)
   ("C-c b" . consult-buffer)
   ("C-c 4 b" . consult-buffer-other-window)
   ("C-x p g" . my/consult-project-ripgrep)
   ("C-x p G" . my/consult-project-external-ripgrep)
   (:prefix "C-x g"
            :prefix-map file-stuff-map
            ("f" . find-file)
            ("l" . consult-focus-line)
            ("s" . consult-fd)
            ("e" . consult-recent-file)
            ("q" . set-fill-column)
            ("r" . consult-ripgrep)
            ("G" . consult-git-grep)
            ("g" . consult-grep)
            ("i" . consult-info)
            ("d" . consult-dir)
            ("F" . consult-find)))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  (minibuffer-setup . choose-completion-in-region)
  :config
  ;; (setq consult-async-input-debounce 0
  ;;       consult-async-input-throttle 0
  ;;       consult-async-refresh-delay 0)

  (defun choose-completion-in-region ()
    "Use default `completion--in-region' unless we are not completing."
    (when minibuffer-completion-table
      (setq-local completion-in-region-function #'completion--in-region)))
  (advice-add #'register-preview :override #'consult-register-window)
  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring))

(use-package consult-imenu
  :defer t
  :config
  (setf
   (alist-get
    ?k (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config) :types))
   '("Keymaps" font-lock-variable-name-face)))

(use-package consult-yasnippet
  :ensure t
  :bind
  (("C-x g y" . consult-yasnippet)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)
  ("C-:" . embark-act-all)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("C-h M" . embark-bindings-in-keymap)
  ("C-h E" . embark-on-last-message)
  (:map completion-list-mode-map
        ("." . embark-act))
  (:map embark-collect-mode-map
        ("a") ; I don't like my own default :)
        ("." . embark-act)
        ("F" . consult-focus-lines))
  (:map embark-package-map
        ("t" . try))
  (:map embark-identifier-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map))
  (:map embark-expression-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map))
  (:map embark-region-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map)
        ("D" . dictionary-search)
        ("=" . quick-calc))
  (:map embark-email-map
        ("+" . add-email-to-ecomplete)
        ("\\" . remove-email-from-ecomplete))
  (:map embark-encode-map
        ("p" . topaz-paste-region))
  (:map embark-url-map
        ("a" . arXiv-map)
        ("o" . omnivore-add-url))
  (:map embark-identifier-map
        ("D" . dictionary-lookup-definition))
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")

  (embark-confirm-act-all nil)
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; want sentence and paragraph targets in more modes
  (embark-define-thingatpt-target sentence
    text-mode help-mode Info-mode man-common mastodon-mode
    lem-mode emacs-news-view-mode)
  (embark-define-thingatpt-target paragraph
    text-mode help-mode Info-mode man-common mastodon-mode
    lem-mode emacs-news-view-mode)
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (dolist (cmd '(markdown-insert-code
                 markdown-insert-italic
                 markdown-insert-bold
                 cdlatex-math-modify
                 TeX-font))
    (push #'embark--mark-target (alist-get cmd embark-around-action-hooks)))
  (push #'embark--xref-push-marker (alist-get 'find-file embark-pre-action-hooks))
  (add-to-list 'embark-keymap-alist '(ecomplete . embark-email-map))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg))))

(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode)
  :config
  (defun marginalia--file-owner (attrs) ; Only display UID
    "Return file owner given ATTRS."
    (let ((uid (file-attribute-user-id attrs)))
      (when (/= (user-uid) uid)
        (or (user-login-name uid) uid)))))

;; (use-package all-the-icons-completion
;;   :ensure t
;;   :after (marginalia all-the-icons)
;;   :init
;;   (all-the-icons-completion-mode)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; (use-package ibuffer
;;   :general
;;   ([remap list-buffers] 'ibuffer)
;;   )

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode 1)
  :bind
  (([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap query-replace] . anzu-query-replace))
  (:map isearch-mode-map
	([remap isearch-query-replace] . anzu-isearch-query-replace)
	([remap isearch-query-replace] . anzu-isearch-query-replace-regexp)))

(use-package avy
  :ensure t
  :bind
  (("C-'" . avy-goto-char-timer))
  (:map isearch-mode-map
        ("M-q" . avy-isearch)
        ("M-g g" . avy-goto-line))
  :config
  (add-to-list 'avy-dispatch-alist '(?\, . avy-action-goto))
  (defun avy-embark-act (pt)
    "Use Embark to act on the item at PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))
      t))
  (add-to-list 'avy-dispatch-alist '(?\. . avy-embark-act))
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange)))

(provide 'search-menu)
