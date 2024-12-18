(use-package org
  :ensure nil
  :preface
  (defun iannn/toggle-side-bullet-org-buffer ()
    "Toggle `bullet.org` in a side buffer for quick note taking.  The buffer is opened in side window so it can't be accidentaly removed."
    (interactive)
    (iannn/toggle-side-buffer-with-file "~/Documents/Notes/bullet.org"))

  (defun iannn/buffer-visible-p (buffer)
    "Check if given BUFFER is visible or not.  BUFFER is a string representing the buffer name."
    (or (eq buffer (window-buffer (selected-window))) (get-buffer-window buffer)))

  (defun iannn/display-buffer-in-side-window (buffer)
    "Just like `display-buffer-in-side-window' but only takes a BUFFER and rest of the parameters are for my taste."
    (select-window
     (display-buffer-in-side-window
      buffer
      (list (cons 'side 'right)
            (cons 'slot 0)
            (cons 'window-width 84)
            (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                           (cons 'no-other-window nil)))))))

  (defun iannn/remove-window-with-buffer (the-buffer-name)
    "Remove window containing given THE-BUFFER-NAME."
    (mapc (lambda (window)
            (when (string-equal (buffer-name (window-buffer window)) the-buffer-name)
              (delete-window window)))
          (window-list (selected-frame))))

  (defun iannn/toggle-side-buffer-with-file (file-path)
    "Toggle FILE-PATH in a side buffer. The buffer is opened in side window so it can't be accidentaly removed."
    (interactive)
    (let ((fname (file-name-nondirectory file-path)))
      (if (iannn/buffer-visible-p fname)
          (iannn/remove-window-with-buffer fname)
        (iannn/display-buffer-in-side-window
         (save-window-excursion
           (find-file file-path)
           (current-buffer))))))

  :bind
  (("<C-m> o c" . org-capture)
   ("<C-m> o a" . org-agenda)
   ("<C-m> o l" . org-store-link)
   ("<C-m> o o" . iannn/toggle-side-bullet-org-buffer)
   ("<C-m> o g" . org-clock-goto))
  (:map org-mode-map
        ("C-c H" . org-shiftmetaleft)
        ("C-c J" . org-shiftmetadown)
        ("C-c K" . org-shiftmetaup)
        ("C-c L" . org-shiftmetaright)
        ("C-c h" . org-metaleft)
        ("C-c j" . org-metadown)
        ("C-c k" . org-metaup)
        ("C-c l" . org-metaright)
        ("C-M-<return>" . org-insert-subheading)
        (:prefix "C-c C-l"
                 :prefix-map org-link-map
                 ("k" . org-store-link)
                 ("l" . org-insert-link)))

  :config
  (require 'org-tempo)
  (setq
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-capture-templates
	    (quote (("t" "Todo" entry (file "~/Documents/OrgFiles/task.org")
		         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		        ("r" "Respond" entry (file "~/Documents/OrgFiles/refile.org")
		         "* Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		        ("n" "Note" entry (file "~/Documents/OrgFiles/notes.org")
		         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		        ("j" "Journal" entry (file+datetree "~/Documents/OrgFiles/journal.org")
		         "* %?\n%U\n" :clock-in t :clock-resume t)
		        ("g" "With Jesus" entry (file+datetree "~/Documents/OrgFiles/withjesus.org")
		         "* %?\n%U\n" :clock-in t :clock-resume t)
		        ("d" "Diary" entry (file+datetree "~/Documents/OrgFiles/diary.org")
		         "* %?\n%U\n" :clock-in t :clock-resume t)
		        ("R" "Refile" entry (file+olp+datetree "~/Documents/OrgFiles/refile.org")
		         "* TODO Review %A %^G\n%x\n%U\n" :immediate-finish t)
		        ("m" "Meeting" entry (file "~/Documents/OrgFiles/refile.org")
		         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
		        ("p" "PDF-Notes" entry (file "~/Documents/OrgFiles/notes.org")
                 "* %?\n%(org-capture-pdf-active-region)\n")
		        ("b" "Book" entry (file  "~/Documents/OrgFiles/books.org")
		         "*  %(let* ((url (substring-no-properties (current-kill 0)))
                  (details (org-books-get-details url)))
                (when details (apply #'org-books-format 1 details)))")
		        ("a" "Appointment" entry (file+olp+datetree "~/Documents/OrgFiles/diary.org")
		         "* APPT %^{Description} %^g %?\n Added: %U")
		        ("c" "Contacts" entry (file+headline "~/Documents/OrgFiles/contacts.org" "")
		         "* %^{Name} :CONTACT: %[~/Documents/OrgFiles/contacts.txt]")
		        ("P" "Protocol" entry (concat org-directory ,(file+headline  "notes.org")
					                          "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
		        ("L" "Protocol Link" entry (concat org-directory ,(file+headline  "notes.org")
						                           "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (R . t)
     (dot . t)
     (haskell . t)
     (python . t)
     (shell . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     ;; (ledger . t)         ;this is the important one for this tutorial
     (C . t)
     (sql . t)
     (sqlite . t)))

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :background "#000000")

  (set-face-attribute 'org-block-begin-line nil
                      :inherit 'fixed-pitch
                      :height 0.9
                      :overline "#444444"
                      :background "#000000"
                      :extend t)

  (set-face-attribute 'org-block-end-line nil
                      :inherit 'fixed-pitch
                      :height 0.9
                      :foreground "#444444"
                      :background "#000000"
                      :extend t)

  ;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block-begin-line nil
  ;;                     :inherit 'fixed-pitch
  ;;                     :height 0.9
  ;;                     :extend t)

  ;; (set-face-attribute 'org-block-end-line nil
  ;;                     :inherit 'fixed-pitch
  ;;                     :height 0.9
  ;;                     :extend t)

  (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit 'fixed-pitch :foreground (face-background 'default))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox-statistics-done nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox-statistics-todo nil :inherit 'fixed-pitch)


  (set-face-attribute 'org-code nil :inherit 'fixed-pitch :background "#333333")
  ;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)

  (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch :height 0.9)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch :foreground "#e6e147")
  (set-face-attribute 'org-headline-done nil :foreground "#585c60")
  (set-face-attribute 'org-done nil :foreground "#585c60")
  (set-face-attribute 'org-todo nil :foreground "#ffa3a3" :background "#745050")
  (set-face-attribute 'org-link nil :foreground "#a2b9d9" :underline t)
  (set-face-attribute 'org-document-title nil :height 1.3 :weight 'bold :foreground "#888888")
  ;; (set-face-attribute 'org-document-title nil :height 1.3 :weight 'bold)
  ;; (set-face-attribute 'org-modern-label nil :box (:color "#222222" :line-width (0 . -1)))

  (set-face-attribute 'org-level-1 nil :foreground "#4fb87f" :height 1.1)
  (set-face-attribute 'org-level-2 nil :foreground "#4fb87f" :height 1.1)
  (set-face-attribute 'org-level-3 nil :foreground "#4fb87f" :height 1.0)
  (set-face-attribute 'org-level-4 nil :foreground "#4fb87f" :height 1.0)
  (set-face-attribute 'org-level-5 nil :foreground "#4fb87f" :height 1.0)
  (set-face-attribute 'org-level-6 nil :foreground "#4fb87f" :height 1.0)
  (set-face-attribute 'org-level-7 nil :foreground "#4fb87f" :height 1.0)
  (set-face-attribute 'org-level-8 nil :foreground "#4fb87f" :height 1.0)

  (set-face-attribute 'mode-line nil :background "#2A2A2A" :box '(:color "#2a2a2a" :line-width 5))
  (set-face-attribute 'mode-line-inactive nil :background "#202020" :box '(:color "#202020" :line-width 5))

  (set-face-attribute 'tab-bar nil
                      :background "#2a2a2a"
                      :box '(:color "#2a2a2a" :line-width 1))
  (set-face-attribute 'tab-bar-tab nil
                      :background "#2a2a2a"
                      :box '(:color "#444444" :line-width 1))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background "#202020"
                      :box '(:color "#333333" :line-width 1))
  :custom
  (org-ellipsis "…")
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-startup-indented t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-export-with-smart-quotes t)
  (org-confirm-babel-evaluate nil)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content nil)
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively t)
  (org-entities-user '(("newline" "\\newline" nil "<br>" "\n" "\n" "⏎")))
  (org-preview-latex-image-directory "~/.cache/ltximg/")
  (org-tags-column 0)
  (org-indent-mode t)
  (org-auto-align-tags nil)
  (org-use-speed-commands t)
  (org-cycle-emulate-tab 't)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-agenda-start-on-weekday nil)
  (org-log-into-drawer t)
  (org-indent-indentation-per-level 4)
  (org-agenda-use-time-grid nil))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  (org-mode . variable-pitch-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (defface org-level-default '((t :inherit 'default :height 1.1 :weight bold))
    "Face used for level default headlines."
    :group 'org-faces)

  (advice-add
   'org-get-level-face :override
   (defun org-get-level-face--override (n)
     (let* ((org-l0 (- (match-end 2) (match-beginning 1) 1))
            (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
            (org-f (if org-cycle-level-faces
                       (nth (% (1- org-l) org-n-level-faces) org-level-faces)
                     (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
       (cond
        ((eq n 1)
         ;; face for leading stars
         (if org-hide-leading-stars 'org-hide org-f))
        ((eq n 2)
         ;; face for the last star and space
         org-f)
        ;; face for the heading
        (t 'org-level-default)))))

  (setq org-modern-fold-stars '(("#" . "#")
                                ("#" . "#")
                                ("#" . "#")
                                ("#" . "#")
                                ("#" . "#"))))

(use-package org-roam
  :ensure t
  :init
  (defun my/org-roam-node-has-tag (node tag)
    "Filter function to check if the given NODE has the specified TAG."
    (member tag (org-roam-node-tags node)))

  (defun my/org-roam-node-find-by-tag ()
    "Find and open an Org-roam node based on a specified tag."
    (interactive)
    (let ((tag (read-string "Enter tag: ")))
      (org-roam-node-find nil nil (lambda (node) (my/org-roam-node-has-tag node tag)))))
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Notes/")
  (org-roam-completion-everywhere t)
  :bind (("<C-m> r l" . org-roam-buffer-toggle)
         ("<C-m> r f" . org-roam-node-find)
         ("<C-m> r i" . org-roam-node-insert)
         ("<C-m> r t" . my/org-roam-node-find-by-tag))
  :config
  (setq org-roam-node-display-template (concat "${title:*}" (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-setup))

(use-package ox-extra
  :after org
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-with-hyperref nil)
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil)))

(provide 'org-settings)
