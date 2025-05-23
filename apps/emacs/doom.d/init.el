(doom! :completion
       company
       vertico

       :ui
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
       modeline
       (popup +defaults)
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces

       :editor
       (evil +everywhere)
       fold
       (format +lsp)
       multiple-cursors
       parinfer
       snippets
       word-wrap

       :emacs
       (dired +dirvish)
       electric
       ibuffer
       (undo +tree)
       vc

       :term
       eshell

       :checkers
       syntax
       (spell +aspell +everywhere)

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
       tree-sitter

       :os
       tty

       :lang
       (cc +lsp +tree-sitter)
       (csharp +lsp +tree-sitter)
       data
       (emacs-lisp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (json +tree-sitter)
       (markdown +grip)
       (nix +tree-sitter)
       org
       (php +tree-sitter)
       (python +lsp +tree-sitter)
       rest
       (rust +lsp +tree-sitter)
       (sh +tree-sitter)
       (swift +lsp +tree-sitter)
       (web +tree-sitter)
       yaml

       :config
       (default +bindings +smartparens))
