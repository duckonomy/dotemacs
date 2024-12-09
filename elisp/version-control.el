(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-define-global-key-bindings nil)
  :bind
  ("C-x G s" . magit-status)
  ("C-x G d" . magit-dispatch)
  ("C-x G f" . magit-file-dispatch))

(provide 'version-control)
