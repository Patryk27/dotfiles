;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; dired
(map! :leader "j" #'dired-jump)

;; doom
(setq doom-font (font-spec :family "Iosevka Custom" :size 17 :weight 'light)
      doom-theme 'doom-gruvbox)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(map! :leader "w P" #'+popup/raise)

(map! :leader
      (:prefix "TAB"
       "{" #'+workspace/swap-left
       "}" #'+workspace/swap-right))

;; emacs
(setq calendar-week-start-day 1
      display-line-numbers-type nil
      truncate-string-ellipsis "…"
      user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me")

(global-display-fill-column-indicator-mode +1)

(map! :leader "C" #'calendar)

;; evil
(setq evil-want-fine-undo t)

;; evil-surround
(defun evil-surround-word ()
  (let ((word (evil-surround-read-from-minibuffer "" "")))
    (cons (format "%s(" word) ")")))

(add-hook 'rustic-mode-hook
  (lambda ()
    (with-eval-after-load 'evil-surround
      (push '(?\( . evil-surround-word) evil-surround-pairs-alist))))

;; gcmh
(setq gcmh-high-cons-threshold (* 128 1024 1024)
      gcmh-idle-delay 10.0)

;; ispell
(setq ispell-dictionary "en")

;; ivy
(after! (:and evil-collection ivy)
  (evil-define-key 'normal 'ivy-occur-mode-map
    "gr" 'ivy-occur-revert-buffer))

;; lsp
(setq lsp-file-watch-threshold 5000
      lsp-rust-all-features t
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-signature-auto-activate nil
      lsp-ui-sideline-enable nil)

(define-key evil-normal-state-map (kbd "g:") 'sort-lines)
(define-key evil-normal-state-map (kbd "ga") nil)
(define-key evil-normal-state-map (kbd "gai") 'avy-goto-char)
(define-key evil-normal-state-map (kbd "ghh") 'lsp-goto-type-definition)
(define-key evil-normal-state-map (kbd "ghj") 'lsp-rust-find-parent-module)
(define-key evil-normal-state-map (kbd "ghk") 'lsp-rust-analyzer-open-cargo-toml)
(define-key evil-normal-state-map (kbd "ghl") 'lsp-rust-analyzer-expand-macro)
(define-key evil-normal-state-map (kbd "gsi") 'avy-goto-char-in-line)

;; org
(setq org-agenda-files '("~/org/" "~/org/praca" "~/org/wycieczki")
      org-directory "~/org/")

;; point-history
(point-history-mode t)
(global-set-key (kbd "M-s M-s") 'ivy-point-history)

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

;; rainbow-delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; undo-tree
(setq undo-tree-visualizer-timestamps t)
