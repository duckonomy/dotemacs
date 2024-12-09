(use-package elfeed
  :ensure t)

(use-package webjump
  :bind
  ("C-c w" . webjump))

(use-package ement
  :ensure t)


;; (use-package webjump-extras
;;   :after webjump
;;   :demand t
;;   :bind
;;   ("C-c W" . webjump-reload)
;;   :config
;;   (webjump-reload))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . variable-pitch-mode)))


(use-package notmuch
  :ensure t
  :bind (("C-x m" . notmuch-mua-new-mail))
  :config
  (setq user-full-name "Ian Park")
  (setq user-mail-address "iannnpark@icloud.com")
  (setq mail-user-agent 'message-user-agent)
  (setq mail-specify-envelope-from t)
  (setq sendmail-program "/usr/bin/msmtp"
	mail-specify-envelope-from t
	mail-envelope-from 'header
	message-sendmail-envelope-from 'header))

(provide 'apps)
