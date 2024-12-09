(use-package window-highlight
  :if (and window-system (>= emacs-major-version 27))
  :demand t
  :ensure nil
  :straight (:host github :repo "dcolascione/emacs-window-highlight")
  :config
  ;; Sometimes on startup, Emacs doesn't realize it's in focus? I think this is
  ;; because of the way macOS starts Emacs (because starting it from the command
  ;; line doesn't exhibit this behavior). Anyway, it doesn't seem too terrible
  ;; to go ahead and set it manually.
  (set-frame-parameter (selected-frame) 'last-focus-update t)
  (window-highlight-mode))

(defun theme-face-attribute (face attribute)
  "Get the ATTRIBUTE of the FACE from the current theme.

See `theme-attribute'."
  (plist-get (face-spec-choose (theme-face face)) attribute))

(defun color-blend (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 with ALPHA.
COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").
Alpha should be a float between 0 and 1.

Stolen from solarized."
  (apply #'color-rgb-to-hex
         (-zip-with (lambda (it other)
                      (+ (* alpha it) (* other (- 1 alpha))))
                    (color-name-to-rgb color1)
                    (color-name-to-rgb color2))))

(defun custom-enabled-themes-reset (&rest _)
  "Remove all current themes before loading a new theme."
  (mapc #'disable-theme custom-enabled-themes))

(defun m-customize-faces (&rest _)
  "Customize faces after a theme is loaded.

This sets things up for `window-highlight' and `mode-line'."
  (let* ((active-bg (or (theme-face-attribute 'default :background)
                        (if (eq (frame-parameter nil 'background-mode) 'light)
                            "#FFF" "#000")))
         (inactive-bg (color-blend active-bg
                                   (theme-face-attribute 'default :foreground)
                                   0.95)))
    (apply #'custom-set-faces
           `((default ((t :background ,inactive-bg)))
             (fringe ((t :background ,inactive-bg)))
             (window-highlight-focused-window ((t :background ,active-bg)))
             (vertical-border ((t :foreground ,inactive-bg)))
             (mode-line-inactive ((t :box nil)))))))

(advice-add #'load-theme :before #'custom-enabled-themes-reset)
(advice-add #'load-theme :after #'m-customize-faces)
