;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).
(doom-load (expand-file-name "~/.config/yf/emacs/features.el"))

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;;(company +childframe) ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy  ; search engine for love and life
       (corfu +orderless)

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; emoji in emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;; ligatures
       hydra
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;pretty-code       ; replace bits of code with pretty symbols
       tabs              ; an tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       ;; workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       undo              ; persistent, smarter undo for your inevitable mistakes
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +everywhere) ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :os
       (:if IS-MAC macos)
       tty

       :tools
       ;;ansible
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (lsp +peek)
       ;;macos             ; MacOS-specific commands
       (magit +forge)            ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       lsp
       tree-sitter

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp)                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       (elixir +lsp)            ; erlang done right
       elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       go                ; the hipster dialect
       (haskell +lsp +tree-sitter)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       (java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        ;;+hugo            ; use Emacs for hugo blogging
        ;;+jupyter        ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        ;;+pomodoro        ; be fruitful with the tomato technique
        +pandoc
        +roam2
        +present)        ; using org-mode for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       purescript ; javascript, but functional
       (:if (featurep 'yf-python) ; beautiful is better than ugly
           (python +tree-sitter +lsp +pyright)
         (python +tree-sitter))
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)         ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scala +lsp)       ; java, but good
       (scheme +guile)    ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       web               ; the tubes
       yaml
       (zig +lsp)        ; c but simpler

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))

;; (use-package-hook! evil
;;   :pre-init
;;   ;;(setq evil-magic nil)
;;   (setq evil-respect-visual-line-mode t) ;; sane j and k behavior
;;   (setq evil-want-C-u-scroll nil)
;;   t)
;; 
;; (use-package-hook! evil
;;   :pre-init
;;   t)
