;; ace-window
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white"
    :background "red"
    :weight bold))

(setq aw-keys '(?a ?s ?d ?h ?j ?k ?l))

(map! :leader "k" 'ace-window)

;; avy
(setq avy-keys '(?a ?s ?d ?h ?j ?k ?l))

(map! :n "go" 'evil-avy-goto-char-timer)

;; dap
(setq dap-auto-configure-features '(sessions locals controls tooltip)
      dap-lldb-debug-program `(,(getenv "EMACS_LLDB")))

(require 'dap-lldb)

(map! :leader
      :prefix "e"
      "a" 'dap-breakpoint-add
      "d" 'dap-breakpoint-delete
      "r" 'dap-debug)

;; dired
(map! :leader
      "j" 'dired-jump
      "J" 'dired-jump-other-window)

(add-hook 'dired-after-readin-hook 'hl-line-mode)

(defun dired-diff-dwim ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(after! (:and evil dired)
  (map! :map dired-mode-map
        :n "q" 'kill-current-buffer
        :n "Q" '+dired/quit-all
        :n "=" 'dired-diff-dwim
        :n "C-<return>" 'dired-find-file-other-window)

  (map! :map dired-mode-map
        :localleader
        :n "f" 'find-name-dired))

;; doom
(setq doom-font (font-spec :family "Iosevka Custom" :size 18 :weight 'light)
      doom-theme 'doom-gruvbox)

(custom-set-faces!
  '(mode-line-inactive
    :background "#202020"))

(map! :leader
      "w P" '+popup/raise
      "[" '+workspace/switch-left
      "]" '+workspace/switch-right
      "{" '+workspace/swap-left
      "}" '+workspace/swap-right
      "1" '+workspace/switch-to-0
      "2" '+workspace/switch-to-1
      "3" '+workspace/switch-to-2
      "4" '+workspace/switch-to-3
      "5" '+workspace/switch-to-4
      "6" '+workspace/switch-to-5
      "7" '+workspace/switch-to-6
      "8" '+workspace/switch-to-7
      "9" '+workspace/switch-to-8
      "0" '+workspace/switch-to-final)

;; doom-modeline
(after! doom-modeline
  (defun empty-modeline ()
    nil)

  (advice-add 'doom-modeline-segment--vcs :override 'empty-modeline))

;; ediff
(map! :leader
      :prefix "b"
      :desc "ediff" "=" 'ediff-buffers)

;; emacs
(setq calendar-week-start-day 1
      display-line-numbers-type nil
      user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me")

(global-display-fill-column-indicator-mode +1)

(map! :leader
      :prefix "o"
      "c" 'calendar
      "x" 'calc)

;; evil
(setq evil-want-fine-undo t
      +evil-want-o/O-to-continue-comments nil)

(setq evil-normal-state-cursor '(box "#00ff00")
      evil-insert-state-cursor '(bar "#00ff00")
      evil-visual-state-cursor '(hollow "#00ff00"))

(map! :n "gF" 'ffap-other-window)

;; evil-numbers
(map! :n "g+" 'evil-numbers/inc-at-pt
      :n "g-" 'evil-numbers/dec-at-pt)

;; gcmh
(setq gcmh-high-cons-threshold (* 128 1024 1024)
      gcmh-idle-delay 10.0)

;; hl-line
(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

;; ispell
(setq ispell-dictionary "en")

;; json-mode
(defun +format--buffer-maybe-json (orig)
  (if (eq major-mode 'json-mode)
      (json-pretty-print-buffer)
    (funcall orig)))

(after! json
  (advice-add '+format--buffer :around '+format--buffer-maybe-json))

;; lsp
(setq lsp-file-watch-threshold 5000
      lsp-lens-enable nil
      lsp-rust-all-features t
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-signature-auto-activate nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil)

(map! :n "ga" 'lsp-execute-code-action
      :n "gh" 'sort-lines
      :n "gj" '+lookup/references
      :n "gt" '+lookup/type-definition)

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :prefix ("t" . "table")
        :desc "align" "a" 'markdown-table-align))

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

(map! :n "-" 'parrot-rotate-prev-word-at-point
      :n "+" 'parrot-rotate-next-word-at-point)

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

;; rainbow-delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; rustic-mode
(defun rustic-rerun-shell-command ()
  (interactive)
  (rustic-run-cargo-command
   (car compile-history)
   (list :mode 'rustic-cargo-run-mode)))

(defun rustic-cargo-check-workspace ()
  (interactive)
  (let ((cmd "cargo check --workspace --tests --all-features"))
    (push cmd compile-history)
    (rustic-run-cargo-command cmd (list :mode 'rustic-cargo-run-mode))))

(defun rustic-cargo-test-workspace ()
  (interactive)
  (let ((cmd "cargo test --workspace --tests --all-features"))
    (push cmd compile-history)
    (rustic-run-cargo-command cmd (list :mode 'rustic-cargo-run-mode))))

(setq rustic-compile-directory-method 'rustic-buffer-workspace)

(after! rustic
  (map! :map comint-mode-map
        :localleader
        "r"
        'rustic-rerun-shell-command)

  (map! :map rustic-mode-map
        :localleader
        "SPC" 'lsp-rust-analyzer-open-cargo-toml
        "h" 'lsp-rust-analyzer-inlay-hints-mode
        "j" 'lsp-rust-analyzer-move-item-down
        "k" 'lsp-rust-analyzer-move-item-up
        "m" 'lsp-rust-analyzer-expand-macro
        "p" 'lsp-rust-find-parent-module
        "r" 'rustic-rerun-shell-command
        "s" 'rustic-run-shell-command)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("w" . "workspace")
        :desc "cargo check" "c" 'rustic-cargo-check-workspace
        :desc "cargo test" "t" 'rustic-cargo-test-workspace))

;; subword-mode
(map! "C-x s" 'subword-mode
      "C-x S" 'global-subword-mode)

;; undo-tree
(setq undo-tree-visualizer-timestamps t)

;; vertico
(after! vertico
  (map! :map vertico-map
        "M-." 'embark-act))

;; vterm
(map! :leader "d" '+vterm/toggle)

;; ---------------- ;;
;; CLI improvements ;;

(defun term-init-keyboard ()
  (xterm--init-modify-other-keys)

  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (define-key xterm-function-map "\e\[27u" [(escape)])
    (define-key xterm-function-map "\e\[H" [(home)])
    (define-key xterm-function-map "\e\[F" [(end)])

    (let ((c 32))
      (while (<= c 127)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (vector (append (cdr x) (cons c '())))))
              '(("\e\[%d;2u" shift)
                ("\e\[%d;3u" meta)
                ("\e\[%d;4u" shift meta)
                ("\e\[%d;5u" control)
                ("\e\[%d;6u" shift control)
                ("\e\[%d;7u" meta control)
                ("\e\[%d;8u" shift meta control)))
        (setq c (1+ c))))))


(defun term-init ()
  (term-init-keyboard))

(unless (display-graphic-p)
  (eval-after-load "xterm" '(term-init)))
