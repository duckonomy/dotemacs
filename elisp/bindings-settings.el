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
