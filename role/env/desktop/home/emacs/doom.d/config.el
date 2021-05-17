;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; emacs
(setq user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me"
      display-line-numbers-type 'relative
      read-process-output-max (* 1024 1024))

(map! :leader
      (:prefix ("TAB" . "workspace")
       :desc "Swap left" "(" #'+workspace/swap-left
       :desc "Swap right" ")" #'+workspace/swap-right))

(global-display-fill-column-indicator-mode +1)

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
(setq lsp-file-watch-threshold 5000
      lsp-rust-all-features t
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-doc-show-with-mouse nil)

(define-key evil-normal-state-map (kbd "gsd") 'lsp-goto-type-definition)
(define-key evil-normal-state-map (kbd "gsp") 'lsp-rust-find-parent-module)

;; org
(setq org-directory "~/org/")

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

;; rainbow-delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; undo-tree
(setq undo-tree-visualizer-timestamps t)

;; ---------------- ;;
;; CLI improvements ;;

(bracketed-paste-enable)

(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;2C" [S-right])
(define-key input-decode-map "\e[1;2D" [S-left])

(define-key input-decode-map "\e[1;3A" [M-up])
(define-key input-decode-map "\e[1;3B" [M-down])
(define-key input-decode-map "\e[1;3C" [M-right])
(define-key input-decode-map "\e[1;3D" [M-left])

(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])

(define-key input-decode-map "\e[1;6A" [C-S-up])
(define-key input-decode-map "\e[1;6B" [C-S-down])
(define-key input-decode-map "\e[1;6C" [C-S-right])
(define-key input-decode-map "\e[1;6D" [C-S-left])

(defun xterm-title-update ()
  (send-string-to-terminal (concat "\033]1;" (buffer-name) "\007")
                           (if buffer-file-name
                               (send-string-to-terminal (concat "\033]2;" (buffer-file-name) "\007"))
                             (send-string-to-terminal (concat "\033]2;" (buffer-name) "\007")))))

(add-hook 'post-command-hook 'xterm-title-update)
