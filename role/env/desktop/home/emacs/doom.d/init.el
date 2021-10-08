(doom! :completion
       (company +childframe)
       (ivy +childframe +prescient)

       :ui
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
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
       (parinfer +rust)
       snippets
       word-wrap

       :emacs
       dired
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
       (lookup +dictionary +offline)
       (lsp +peek)
       magit
       make
       rgb

       :os
       (tty +osc)

       :lang
       data
       emacs-lisp
       javascript
       json
       (markdown +grip)
       nix
       org
       (php +lsp)
       rest
       (rust +lsp)
       sh
       web
       yaml

       :config
       (default +bindings +smartparens))
