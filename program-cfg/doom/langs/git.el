;;; ~/g/dotfiles/config/doom/langs/git.el -*- lexical-binding: t; -*-

;; Who the fuck uses macros in git commits

(after! (magit evil with-editor)
  (evil-define-minor-mode-key 'normal 'git-commit-mode
    "q" 'with-editor-finish
    "Q" 'with-editor-cancel)
  )
