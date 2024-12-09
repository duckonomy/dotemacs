(use-package expand-region
  :ensure t
  :bind
  ("C-\\" . er/expand-region)
  ("C-|" . er/contract-region))

(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t))

(use-package iedit
  :ensure t
  :preface
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
	(iedit-mode)
      (save-excursion
	(save-restriction
	  (widen)
	  ;; this function determines the scope of `iedit-start'.
	  (if iedit-mode
	      (iedit-done)
	    ;; `current-word' can of course be replaced by other
	    ;; functions.
	    (narrow-to-defun)
	    (iedit-start (current-word) (point-min) (point-max)))))))
  :bind
  ("C-;" . iedit-mode)
  ("C-M-;" . iedit-dwim))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-M-<" . mc/mark-all-like-this))

(use-package embrace
  :ensure t
  :bind
  ("C-z ." . embrace-commander)
  :hook
  (org-mode . embrace-org-mode-hook))

(provide 'editing)
