;; (use-package evil
;;   :ensure t
;;   :init
;;   (evil-mode 1)
;;   :hook
;;   (evil-insert-state-entry . evil-ex-nohighlight)
;;   :custom
;;   (evil-want-C-u-scroll t)
;;   (evil-ex-visual-char-range t)
;;   (evil-want-visual-char-semi-exclusive t)
;;   (evil-ex-search-vim-style-regexp t)
;;   (evil-ex-interactive-search-highlight 'selected-window)
;;   (evil-echo-state nil)
;;   (evil-ex-substitute-global t)
;;   (evil-insert-skip-empty-lines t)
;;   (evil-want-fine-undo nil)
;;   :config
;;   (setq evil-magic t)
;;   (setq evil-default-state 'emacs)
;;   (with-eval-after-load 'evil-maps
;;     ;; UNSAFE
;;     (define-key evil-motion-state-map (kbd ":") 'evil-ex)
;;     ;; UNSAFE
;;     (define-key evil-motion-state-map (kbd ";") 'evil-ex)
;;     ;; UNSAFE
;;     (define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-or-subword-1)
;;     ;; UNSAFE
;;     (define-key evil-visual-state-map (kbd "SPC") 'avy-goto-word-or-subword-1))
;;   (evil-set-initial-state 'prog-mode 'normal)
;;   (evil-set-initial-state 'sgml-mode 'normal)
;;   (evil-set-initial-state 'fundamental-mode 'normal)
;;   (evil-set-initial-state 'sws-mode 'normal)
;;   (evil-set-initial-state 'text-mode 'normal)
;;   (evil-set-initial-state 'org-mode 'emacs)
;;   (evil-set-initial-state 'dired-mode 'emacs)
;;   ;; (evil-set-initial-state 'info-mode 'emacs)
;;   ;; (evil-set-initial-state 'man-mode 'emacs)
;;   ;; (evil-set-initial-state 'woman-mode 'emacs)
;;   ;; (evil-set-initial-state 'help-mode 'emacs)
;;   (evil-set-initial-state 'wdired-mode 'emacs)
;;   (evil-set-initial-state 'eshell-mode 'emacs)
;;   (evil-set-initial-state 'conf-mode 'normal)
;;   ;; (evil-set-initial-state 'conf-windows-mode 'normal)
;;   ;; (evil-set-initial-state 'conf-toml-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-unix-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-colon-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-space-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-desktop-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-ppd-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-javaprop-mode 'emacs)
;;   ;; (evil-set-initial-state 'conf-xdefaults-mode 'emacs)
;;   (evil-select-search-module 'evil-search-module 'evil-search))


;; ;; (use-package evil
;; ;;   :ensure t
;; ;;   :bind (("<escape>" . keyboard-escape-quit))
;; ;;   :init
;; ;;   ;; allows for using cgn
;; ;;   ;; (setq evil-search-module 'evil-search)
;; ;;   (setq evil-want-keybinding t)
;; ;;   ;; no vim insert bindings
;; ;;   ;; (setq evil-undo-system 'undo-fu)
;; ;;   :config
;; ;;   (evil-mode 1))

;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   (setq evil-want-integration t)
;;   (evil-collection-init))

;; ;; (use-package evil-textobj-tree-sitter
;; ;;   :ensure t
;; ;;   :after evil)

;; ;; (use-package evil-textobj-tree-sitter
;; ;;   :straight (evil-textobj-tree-sitter :type git
;; ;;                                       :host github
;; ;;                                       :repo "meain/evil-textobj-tree-sitter"
;; ;;                                       :files (:defaults "queries" "treesit-queries")))


;; (use-package evil-commentary
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-commentary-mode)
;;   )


;; (use-package evil-surround
;;   :ensure t
;;   :after evil
;;   :config
;;   (global-evil-surround-mode 1))


;; (use-package evil-snipe
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-snipe-mode +1)
;;   (evil-snipe-override-mode +1))

;; (use-package evil-owl
;;   :ensure t
;;   :after evil
;;   :config
;;   (custom-set-faces
;;    '(evil-owl-group-name ((t (:inherit font-lock-function-name-face))))
;;    '(evil-owl-entry-name ((t (:inherit font-lock-type-face)))))

;;   (setq evil-owl-max-string-length 500)
;;   (add-to-list 'display-buffer-alist
;;                '("*evil-owl*"
;;                  (display-buffer-in-side-window)
;;                  (side . bottom)
;;                  (window-height . 0.3)))
;;   (evil-owl-mode))


(use-package bind-key
  :ensure nil
  :preface
  (defmacro save-column (&rest body)
    `(let ((column (current-column)))
       (unwind-protect
           (progn ,@body)
         (move-to-column column))))
  (put 'save-column 'lisp-indent-function 0)

  (defun move-line-up ()
    (interactive)
    (save-column
      (transpose-lines 1)
      (forward-line -2)))

  (defun move-line-down ()
    (interactive)
    (save-column
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)))
  :bind
  (("C-M-S-j" . move-line-down)
   ("C-M-S-k" . move-line-up))
  :config
  (define-key indent-rigidly-map "j" 'move-line-down)
  (define-key indent-rigidly-map "k" 'move-line-up))

(use-package repeat
  :ensure nil
  :init
  (repeat-mode)

  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (define-key indent-rigidly-map "h" 'indent-rigidly-left)
  (define-key indent-rigidly-map "l" 'indent-rigidly-right)
  (define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
  (define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)


  (defvar isearch-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'isearch-repeat-forward)
      (define-key map (kbd "r") #'isearch-repeat-backward)
      map))

  (dolist (cmd '(isearch-repeat-forward isearch-repeat-backward))
    (put cmd 'repeat-map 'isearch-repeat-map))

  ;; (defvar buffer-navigation-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (define-key map (kbd "n") #'next-line)
  ;;     (define-key map (kbd "p") #'previous-line)
  ;;     (define-key map (kbd "f") #'forward-word)
  ;;     (define-key map (kbd "b") #'backward-word)
  ;;     (define-key map (kbd "d") #'scroll-up-command)
  ;;     (define-key map (kbd "u") #'scroll-down-command)
  ;;     map))

  ;; (dolist (cmd '(next-line previous-line forward-word backward-word scroll-up-command scroll-down-command))
  ;;   (put cmd 'repeat-map 'buffer-navigation-map))


  (defvar windmove-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "h") 'windmove-left)
      (define-key map (kbd "H") 'windmove-swap-states-left)
      (define-key map (kbd "l") 'windmove-right)
      (define-key map (kbd "L") 'windmove-swap-states-right)
      (define-key map (kbd "k") 'windmove-up)
      (define-key map (kbd "K") 'windmove-swap-states-up)
      (define-key map (kbd "j") 'windmove-down)
      (define-key map (kbd "J") 'windmove-swap-states-down)
      (define-key map (kbd "{") 'shrink-window)
      (define-key map (kbd "}") 'enlarge-window)
      (define-key map (kbd "[") 'shrink-window-horizontally)
      (define-key map (kbd "]") 'enlarge-window-horizontally)
      map))

  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'windmove-repeat-map)))
   windmove-repeat-map)

  )



(provide 'bindings-settings)
