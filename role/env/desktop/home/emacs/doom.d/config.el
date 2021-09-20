;; ace-window
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 1.5))

(setq aw-keys '(?a ?s ?d ?h ?j ?k ?l))

(map! :leader "k" #'ace-window)

;; avy
(setq avy-keys '(?a ?s ?d ?h ?j ?k ?l))

(define-key evil-normal-state-map (kbd "ga") 'evil-avy-goto-char-timer)

;; dired
(map! :leader "j" #'dired-jump)

;; doom
(setq doom-font (font-spec :family "Iosevka Custom" :size 17 :weight 'light)
      doom-theme 'doom-gruvbox)

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

;; evil-numbers
(define-key evil-normal-state-map (kbd "g+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "g-") 'evil-numbers/dec-at-pt)

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

(define-key evil-normal-state-map (kbd "ghc") 'lsp-rust-analyzer-open-cargo-toml)
(define-key evil-normal-state-map (kbd "ghe") 'lsp-rust-analyzer-expand-macro)
(define-key evil-normal-state-map (kbd "ghp") 'lsp-rust-find-parent-module)
(define-key evil-normal-state-map (kbd "gj") '+lookup/type-definition)
(define-key evil-normal-state-map (kbd "gk") '+lookup/references)
(define-key evil-normal-state-map (kbd "gt") 'sort-lines)

;; org
(setq org-agenda-files '("~/org/" "~/org/praca" "~/org/wycieczki")
      org-directory "~/org/")

;; parrot
(parrot-mode -1)

(setq parrot-rotate-dict
      '(
        (:rot ("<" ">"))
        (:rot ("<=" ">="))
        (:rot ("&" "|"))
        (:rot ("&&" "||"))
        (:rot ("==" "!="))
        (:rot ("yes" "no") :caps t :upcase t)
        (:rot ("get" "set") :caps t :upcase t)
        (:rot ("high" "low") :caps t :upcase t)
        (:rot ("in" "out") :caps t :upcase t)
        (:rot ("left" "right") :caps t :upcase t)
        (:rot ("min" "max") :caps t :upcase t)
        (:rot ("on" "off") :caps t :upcase t)
        (:rot ("true" "false") :caps t :upcase t)
        (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
        (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))))

(define-key evil-normal-state-map (kbd "[q") 'parrot-rotate-prev-word-at-point)
(define-key evil-normal-state-map (kbd "]q") 'parrot-rotate-next-word-at-point)

;; point-history
(point-history-mode t)
(global-set-key (kbd "M-s M-s") 'ivy-point-history)

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

;; rainbow-delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; subword-mode
(define-key evil-normal-state-map (kbd "g'") 'subword-mode)
(define-key evil-normal-state-map (kbd "g\"") 'global-subword-mode)

;; undo-tree
(setq undo-tree-visualizer-timestamps t)
