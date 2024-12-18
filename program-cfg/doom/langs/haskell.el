;;; ~/g/dotfiles/config/doom/langs/haskell.el -*- lexical-binding: t; -*-

(map! :mode 'haskell-error-mode "q" #'evil-delete-window)

(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "stylish-haskell"))
