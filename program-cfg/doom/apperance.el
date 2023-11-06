;;; ~/g/dotfiles/config/doom/apperance.el -*- lexical-binding: t; -*-

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(+global-word-wrap-mode 1)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-laserwave)

(add-hook! vterm-mode
           (buffer-face-set (font-spec :family "FiraMono Nerd Font Mono")))

;;; Scrolling
;; Make scrolling happen not quite at the bottom of the screen
(setq scroll-margin 3)

;; Indent to two, damn it
(setq go-tab-width 2)
(setq c-basic-offset 2)
(setq coffe-tab-width 2)
(setq javascript-indent-level 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

;; Allow which-key to use the right side if there's room
(after! which-key
  (which-key-setup-side-window-right-bottom))

(setq treemacs-collapse-dirs 50)

(after! rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 8)
  ;;more rainbow-ey rainbow delimiters. they cycle around the
  ;;color wheel approximately every three colors with a bit of offset. 256 color
  ;;term compatible
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#5fd7ff")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ffaf00")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#d75fff")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#87ff00")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#ff5f00")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#0087ff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ffff00")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#ff87ff"))

;; Prevent show-paren-mode from clobbering the rainbow color
(custom-set-faces! '(show-paren-match :foreground unspecified))
