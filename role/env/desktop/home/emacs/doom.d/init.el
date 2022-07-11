(doom! :completion
       (company +childframe)
       vertico

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
       (dired +dirvish)
       electric
       ibuffer
       (undo +tree)
       vc

       :term
       vterm

       :checkers
       syntax
       (spell +everywhere)

       :tools
       debugger
       direnv
       docker
       (eval +overlay)
       (lookup +dictionary +offline)
       lsp
       magit
       make
       pdf
       rgb
       tree-sitter

       :os
       (tty +osc)

       :lang
       (cc +lsp +tree-sitter)
       data
       (emacs-lisp +tree-sitter)
       (javascript +tree-sitter)
       (json +tree-sitter)
       (markdown +grip)
       (nix +tree-sitter)
       org
       rest
       (rust +lsp +tree-sitter)
       (sh +tree-sitter)
       (web +tree-sitter)
       yaml

       :config
       (default +bindings +smartparens))
