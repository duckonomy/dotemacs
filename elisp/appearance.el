;; (use-package vim-tab-bar
;;   :ensure t
;;   :commands vim-tab-bar-mode
;;   :init
;;   (vim-tab-bar-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t)
  ;; (set-face-attribute 'tab-bar-tab nil :foreground (face-background 'mode-line-active) :background (face-background 'default)  :box `(:color ,(face-background 'default) :line-width (10 . 10)))
  ;; (set-face-attribute 'tab-bar-tab-inactive nil :foreground "#777777" :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (10 . 10)))
  ;; (set-face-attribute 'tab-bar nil :inherit 'fixed-pitch :foreground "#aaaaaa" :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (10 . 10)))

  ;; (set-face-attribute 'mode-line nil :background (face-background 'default)  :box `(:color ,(face-background 'default) :line-width (10 . 10)))
  ;; (set-face-attribute 'mode-line-inactive nil :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (10 . 10)))
  ;; (set-face-attribute 'mode-line-active nil :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (10 . 10)))
  (set-face-attribute 'default nil :background "#191919")
  (set-face-attribute 'tab-bar-tab nil :foreground (face-background 'mode-line-active) :background (face-background 'default)  :box `(:color ,(face-background 'default) :line-width (5 . 5)))
  (set-face-attribute 'tab-bar-tab-inactive nil :foreground "#777777" :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (5 . 5)))
  (set-face-attribute 'tab-bar nil :inherit 'fixed-pitch :foreground "#aaaaaa" :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (5 . 5)))

  (set-face-attribute 'mode-line nil :background (face-background 'default)  :box `(:color ,(face-background 'default) :line-width (5 . 5)))
  (set-face-attribute 'mode-line-inactive nil :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (5 . 5)))
  (set-face-attribute 'mode-line-active nil :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (5 . 5)))
  (set-face-attribute 'default nil :background "#151515")
  )
(use-package mindre-theme
  :ensure t
  :preface
  (defun my/load-theme (theme)
    (my/disable-themes)
    (load-theme theme :no-confirm))

  (defun my/light-theme ()
    (interactive)
    (my/load-theme 'mindre)
    (set-face-attribute 'tab-bar-tab nil
                        :foreground (face-background 'mode-line)
                        :background (face-background 'default)
                        :box `(:color ,(face-background 'default) :line-width (10 . 10)))
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :foreground "#aaaaaa"
                        :background (face-background 'default)
                        :box `(:color ,(face-background 'default) :line-width (10 . 10)))
    (set-face-attribute 'tab-bar nil
                        :foreground "#777777"
                        :background (face-background 'default)
                        :box `(:color ,(face-background 'default) :line-width (10 . 10)))
    )
  )
;; :config
;; (load-theme 'mindre t))

;; (use-package no-clown-fiesta-theme
;;   :ensure (:host github :repo "emacsmirror/no-clown-fiesta-theme")
;;   :preface
;;   (defun my/disable-themes ()
;;     (mapc #'disable-theme custom-enabled-themes))

;;   (defun my/dark-theme ()
;;     (interactive)
;;     (my/load-theme 'no-clown-fiesta)
;;     (set-face-attribute 'tab-bar-tab nil
;;                         :foreground (face-background 'mode-line-active)
;;                         :background (face-background 'default)
;;                         :box `(:color ,(face-background 'default) :line-width (10 . 10)))
;;     (set-face-attribute 'tab-bar-tab-inactive nil :foreground "#777777" :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (10 . 10)))
;;     (set-face-attribute 'tab-bar nil :foreground "#aaaaaa" :background (face-background 'default) :box `(:color ,(face-background 'default) :line-width (10 . 10)))
;;     )

;;   (defun my/themes-toggle ()
;;     "Toggle between the light and dark themes."
;;     (interactive)
;;     (pcase (car custom-enabled-themes)
;;       ('mindre (my/dark-theme))
;;       ('no-clown-fiesta (my/light-theme))
;;       (_ (my/dark-theme))))
;;   :config
;;   (load-theme 'no-clown-fiesta t)


;; (use-package noctilux-theme
;;   :ensure t
;;   :config
;;   (load-theme 'noctilux t)



;; )

(use-package minions
  :ensure t
  :config
  (setq minions-prominent-modes '(flymake-mode overwrite-mode envrc-mode))
  (minions-mode 1))

;; (use-package mood-line
;;   :ensure t
;;   :config
;;   (mood-line-mode))

;; (use-package lambda-line
;;   :straight (:type git :host github :repo "lambda-emacs/lambda-line")
;;   :custom
;;   ;; (lambda-line-icon-time t) ;; requires ClockFace font (see below)
;;   ;; (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
;;   (lambda-line-position 'top) ;; Set position of status-line
;;   (lambda-line-abbrev t) ;; abbreviate major modes
;;   (lambda-line-hspace "  ")  ;; add some cushion
;;   (lambda-line-prefix t) ;; use a prefix symbol
;;   (lambda-line-prefix-padding nil) ;; no extra space for prefix
;;   (lambda-line-status-invert nil)  ;; no invert colors
;;   (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
;;   (lambda-line-gui-mod-symbol " ⬤")
;;   (lambda-line-gui-rw-symbol  " ◯")
;;   (lambda-line-space-top -.10)  ;; padding on top and bottom of line
;;   (lambda-line-space-bottom +.10)
;;   (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
;;   :config
;;   ;; activate lambda-line
;;   (lambda-line-mode)
;;   ;; set divider line in footer
;;   (when (eq lambda-line-position 'top)
;;     (setq-default mode-line-format (list "%_"))
;;     (setq mode-line-format (list "%_"))))


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
