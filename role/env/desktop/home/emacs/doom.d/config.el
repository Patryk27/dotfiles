;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ace-window
(require 'ace-window)

;; emacs
(setq user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me"
      display-line-numbers-type 'relative)

(map! :leader
      (:prefix ("TAB" . "workspace")
       :desc "Swap left" "(" #'+workspace/swap-left
       :desc "Swap right" ")" #'+workspace/swap-right))

(global-display-fill-column-indicator-mode +1)

;; emacs (internal)
(setq read-process-output-max (* 1024 1024))

;; dired
(map! :leader
      :desc "Dired"
      "j" #'dired-jump)

;; doom
(setq doom-font (font-spec :family "Fira Code" :size 16 :weight 'light)
      doom-theme 'doom-gruvbox
      doom-themes-treemacs-theme "doom-colors")

;; ispell
(setq ispell-dictionary "en")

;; lsp
(setq lsp-completion-provider :capf
      lsp-file-watch-threshold 5000
      lsp-rust-all-features t
      lsp-rust-analyzer-cargo-load-out-dirs-from-check t
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-completion-postfix-enable nil
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil)

(map! :leader
      (:prefix ("c" . "code")
       :desc "Jump to type definition" "v" #'lsp-goto-type-definition))

;; lsp-ui
(setq lsp-ui-doc-show-with-cursor nil
      lsp-ui-doc-show-with-mouse nil)

;; org
(setq org-directory "~/org/")

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

;; treemacs
(setq treemacs-read-string-input 'from-minibuffer)

;; undo-tree
(setq undo-tree-visualizer-timestamps t)
