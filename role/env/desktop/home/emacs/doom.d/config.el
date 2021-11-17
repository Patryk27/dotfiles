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

;; dimmer
(setq dimmer-fraction 0.30)

(dimmer-configure-hydra)
(dimmer-configure-magit)
(dimmer-configure-org)
(dimmer-configure-which-key)

(map! :leader "DEL" 'dimmer-mode)

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
        :n "=" 'dired-diff-dwim))

;; doom
(setq doom-theme 'doom-gruvbox)

(custom-set-faces!
  '(mode-line-inactive
    :background "#202020"))

(map! :leader
      "w P" '+popup/raise
      "[" '+workspace/switch-left
      "]" '+workspace/switch-right
      "{" '+workspace/swap-left
      "}" '+workspace/swap-right)

;; doom-modeline
(after! doom-modeline
  (defun empty-modeline ()
    nil)

  (advice-add 'doom-modeline-segment--vcs :override 'empty-modeline))

;; emacs
(setq calendar-week-start-day 1
      display-line-numbers-type nil
      truncate-string-ellipsis "…"
      user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me")

(global-display-fill-column-indicator-mode +1)

(map! :leader
      :prefix "e"
      "c" 'calc
      "r" 'calendar)

;; evil
(setq evil-want-fine-undo t)
(setq +evil-want-o/O-to-continue-comments nil)

;; evil-numbers
(map! :n "g+" 'evil-numbers/inc-at-pt
      :n "g-" 'evil-numbers/dec-at-pt)

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

;; org
(setq org-agenda-files '("/share/org/" "/share/org/praca" "/share/org/wycieczki")
      org-directory "/share/org/")

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

(after! rustic
  (map! :map comint-mode-map
        :localleader
        "r"
        'rustic-rerun-shell-command)

  (map! :map rustic-mode-map
        :localleader
        "r" 'rustic-rerun-shell-command
        "h" 'lsp-rust-analyzer-inlay-hints-mode
        "m" 'lsp-rust-analyzer-expand-macro
        "p" 'lsp-rust-find-parent-module
        "s" 'rustic-run-shell-command
        "SPC" 'lsp-rust-analyzer-open-cargo-toml)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("w" . "workspace")
        :desc "cargo check" "c" 'rustic-cargo-check-workspace
        :desc "cargo test" "t" 'rustic-cargo-test-workspace))

;; subword-mode
(map! :n "gb" 'subword-mode
      :n "gB" 'global-subword-mode)

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

(defun term-init-clipboard ()
  (when (getenv "WAYLAND_DISPLAY")
    (setq wl-latest-text "")

    (defun wl-copy (text)
      (let ((proc
             (make-process :name "wl-copy"
                           :command '("wl-copy")
                           :connection-type 'pipe)))
        (process-send-string proc text)
        (process-send-eof proc))
      (setq wl-latest-text text))

    (defun wl-paste ()
      (let ((text (shell-command-to-string "wl-paste -t text -n 2>/dev/null")))
        (unless (string= text wl-latest-text) text)))

    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste)))

(defun term-init-cursor ()
  (send-string-to-terminal "\x1b]30001\x1b\\")
  (send-string-to-terminal "\x1b]12;green\x1b\\")

  (add-hook 'kill-emacs-hook
            (lambda ()
              (send-string-to-terminal "\x1b]30101\x1b\\"))))

(defun term-init ()
  (term-init-keyboard)
  (term-init-clipboard)
  (term-init-cursor))

(unless (display-graphic-p)
  (eval-after-load "xterm" '(term-init)))
