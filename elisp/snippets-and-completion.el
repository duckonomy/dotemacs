(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))



(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :preface
  (defun my-emacs-mode-company ()
    (setq-local company-minimum-prefix-length 3
                company-idle-delay 0))

  (defun my-eshell-mode-company ()
    (setq-local company-tooltip-idle-delay 100
                company-frontends
                '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
                  company-preview-frontend
                  company-echo-metadata-frontend)))

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallnback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))
  :hook
  (((eglot-managed-mode comint-mode emacs-lisp-mode) . company-mode)
   (emacs-lisp-mode . my-emacs-mode-company)
   (eshell-mode . my-eshell-mode-company)
   (company-mode . (lambda ()
                     (substitute-key-definition
                      'company-complete-common
                      'company-yasnippet-or-completion
                      company-active-map))))
  :bind
  (("C-c & y" . company-yasnippet))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-limit 20)
  (setq company-echo-delay 0)
  (setq company-require-match nil)
  (setq company-selection-wrap-around nil)
  (setq company-tooltip-align-annotations t)
  (setq company-transformers '(company-sort-by-occurrence))
  (push 'company-files company-backends)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package kind-icon
  :ensure t
  :after company
  :config
  (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
         (formatter (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
    (defun my-company-kind-icon-margin (cand _selected)
      (funcall formatter cand))
    (setq company-format-margin-function #'my-company-kind-icon-margin)))

(provide 'snippets-and-completion)
