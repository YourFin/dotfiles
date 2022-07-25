;;; snippets.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Patrick Nuckolls
;;
;; Created: July 25, 2022
;; Modified: July 25, 2022
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(after! yasnippet
 (set-file-templates!
  '(purescript-mode :project t))
 )

(provide 'snippets)
;;; templates.el ends here
