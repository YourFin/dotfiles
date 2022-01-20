;;; java.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Patrick Nuckolls
;;
;; Author: Patrick Nuckolls <https://github.com/nuckolp>
;; Maintainer: Patrick Nuckolls <d.junkpobox+git@gmail.com>
;; Created: September 22, 2021
;; Modified: September 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nuckolp/java
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(java-mode "java"))
  (lsp-register-client
   (make-lsp-client
    :major-modes '(java-mode)
    :new-connection (lsp-stdio-connection "/Users/nuckolp/g/java-language-server/dist/language_server_mac.sh")
    :activation-fn (lsp-activate-on "java")
    :server-id 'java-lang-server)))



;;; java.el ends here
