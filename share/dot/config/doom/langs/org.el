;;; ~/g/dotfiles/config/doom/langs/org.el -*- lexical-binding: t; -*-

(map! :map org-mode-map
      (:localleader
       :nv
       "x t" #'org-toggle-latex-fragment))

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")
