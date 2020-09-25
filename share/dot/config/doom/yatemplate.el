;;; ~/g/dotfiles/config/doom/yatemplate.el -*- lexical-binding: t; -*-

(use-package yatemplate
      :defer t
      :init
      (setq-default yatemplate-dir (expand-file-name "yatemplates" doom-private-dir)))
