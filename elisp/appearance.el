;; (use-package vim-tab-bar
;;   :ensure t
;;   :commands vim-tab-bar-mode
;;   :init
;;   (vim-tab-bar-mode 1))

;; (use-package doom-themes
;;   :ensure t)
(use-package mindre-theme
  :ensure t
  :preface
  (defun my/load-theme (theme)
    (my/disable-themes)
    (load-theme theme :no-confirm))

  (defun my/light-theme ()
    (interactive)
    (my/load-theme 'mindre)))
  ;; :config
  ;; (load-theme 'mindre t))

(use-package no-clown-fiesta-theme
  :ensure (:host github :repo "emacsmirror/no-clown-fiesta-theme")
  :preface
  (defun my/disable-themes ()
    (mapc #'disable-theme custom-enabled-themes))

  (defun my/dark-theme ()
    (interactive)
    (my/load-theme 'no-clown-fiesta))

  (defun my/themes-toggle ()
    "Toggle between the light and dark themes."
    (interactive)
    (pcase (car custom-enabled-themes)
      ('mindre (my/dark-theme))
      ('no-clown-fiesta (my/light-theme))
      (_ (my/dark-theme))))
  :config
  (load-theme 'no-clown-fiesta t)
  )

(use-package minions
  :ensure t
  :config
  (setq minions-prominent-modes '(flymake-mode overwrite-mode))
  (minions-mode 1))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(use-package ligature
  :ensure t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

(provide 'appearance)
