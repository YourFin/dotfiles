;;; ~/g/dotfiles/config/doom/langs/elm.el -*- lexical-binding: t; -*-

(after! elm
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))
