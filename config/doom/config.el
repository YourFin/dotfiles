;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Patrick Nuckolls"
      user-mail-address "d.junkpobox+git@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Mono" :size 28))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(+global-word-wrap-mode 1)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


(after! hydra
  (defun get-face-with-mouse (event)
    (interactive "@e")
    (let ((clicked-point (posn-point (event-start event))))
      ;; Posn-area will return non-nil if given a non-buffer area
      (if (and clicked-point (not (posn-area (event-start event))))
          (let ((face (or (get-char-property clicked-point 'read-face-name)
                          (get-char-property clicked-point 'face))))
            (if face
                (describe-face face)
              (message "Could not find face at point")))
        (message "Clicked elsewhere"))))

  (defhydra hydra-get-face-with-mouse (nil nil :hint nil)
    "
Right or middle click to see the face at a given point in a buffer"
    ("<mouse-2>" get-face-with-mouse)
    ("<mouse-3>" get-face-with-mouse)))


;;;; TODO
;; (add-to-list 'load-path (expand-file-name "lisp" dotspacemacs-directory))
;; ;;; custom lib
;; (require 'custom-commands)
;; (require 'heretic-evil-clipboard-mode)
;; (require 'apperance)
;; (require 'major-modes)
;; (require 'editing-file)
;; ;;(require 'evil-bindings)
;; (require 'evil-space-binds)
;; (require 'global-bindings)
