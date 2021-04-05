;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
       (ivy +childframe +prescient)

       :ui
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
       ligatures
       modeline
       ophints
       (popup +defaults)
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces

       :editor
       (evil +everywhere)
       fold
       format
       multiple-cursors
       parinfer
       snippets
       word-wrap

       :emacs
       (dired +ranger +icons)
       electric
       ibuffer
       (undo +tree)
       vc

       :term
       vterm

       :checkers
       syntax
       (spell +flyspell +aspell +everywhere)

       :tools
       debugger
       direnv
       docker
       (eval +overlay)
       lookup
       (lsp +peek)
       magit
       make
       pdf
       rgb

       :os
       (tty +osc)

       :lang
       data
       emacs-lisp
       json
       javascript
       (markdown +grip)
       nix
       org
       rest
       (rust +lsp)
       sh
       web
       yaml

       :config
       (default +bindings +smartparens))
