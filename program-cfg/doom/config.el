;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Patrick Nuckolls"
      user-mail-address "d.junkpobox+git@gmail.com")

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
(after! evil-collection-help
  (evil-set-initial-state 'help-mode 'motion))
;;; Tell tramp to set its TERM to 'tramp'
(setq tramp-terminal-type "tramp")

(setq org-roam-directory "~/n")
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(setq evil-want-fine-undo t)

;;; Load the other damn files
(load! "./apperance.el")
(load! "./editing.el")
(load! "./space-binds.el")
(load! "./snippets.el")

;;; "Languages". This was called major modes in previous iterations
;;; of my emacs config. That was more confusing than languages, even though
;;; major modes is technically more correct
(load! "./langs/elm.el")
(load! "./langs/git.el")
(load! "./langs/latex.el")
(load! "./langs/haskell.el")
;;(load! "./langs/org.el")

;; TODO: Find somewhere else to put this
(after! hydra
  (defun get-face-with-mouse--main (event)
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
    "Right or middle click to see the face at a given point in a buffer"
    ("<mouse-2>" get-face-with-mouse--main)
    ("<mouse-3>" get-face-with-mouse--main))
  (defun get-face-with-mouse ()
    "Find the the face of something by clicking"
    (interactive)
    (hydra-get-face-with-mouse/body)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default)))
 '(package-selected-packages (quote (dhall-mode vimrc-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
