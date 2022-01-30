(load "/home/pwy/.doom.d/config/ion-mode.el")

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

;; diag
(require 'quick-peek)

(setq diag--pos nil
      quick-peek-position 'above
      flycheck-display-errors-function nil)

(defun diag--position (err)
  (if (flycheck-relevant-error-other-file-p err)
      (point-min)
    (flycheck-error-pos err)))

(defun diag--message (err)
  (let ((filename (flycheck-error-filename err))
        (id (flycheck-error-id err)))
    (concat (when (and filename (not (equal filename (buffer-file-name))))
              (format "In \"%s\":\n" (file-relative-name filename default-directory)))
            (flycheck-error-message err)
            (when id
              (format " [%s]" id)))))

(defun diag--face (err)
  (pcase (flycheck-error-level err)
    (`info 'compilation-info)
    (`warning 'compilation-warning)
    (`error 'compilation-error)))

(defun diag--show (err)
  (let* ((pos (diag--position err))
         (msg (propertize (diag--message err)
                          'face (diag--face err))))
    (let* ((ov (quick-peek-overlay-ensure-at pos))
           (contents (quick-peek-overlay-contents ov)))
      (setf (quick-peek-overlay-contents ov)
            (concat contents (when contents "\n") msg))
      (quick-peek-update ov))))

(defun diag--toggle (fn)
  (quick-peek-hide)

  (if (or (not diag--pos) (/= diag--pos (point)))
      (progn
        (setq diag--pos (point))
        (funcall fn))
    (setq diag--pos nil)))

(defun diag-toggle-point ()
  "Toggle diagnostics at current point."
  (interactive)

  (diag--toggle
   (lambda ()
     (-when-let (errors (flycheck-overlay-errors-at (point)))
       (mapc #'diag--show
             (seq-uniq
              (seq-mapcat #'flycheck-related-errors errors)))))))

(defun diag-toggle-line ()
  "Toggle diagnostics at current line."
  (interactive)

  (diag--toggle
   (lambda()
     (-when-let (errors (flycheck-overlay-errors-in (line-beginning-position) (line-end-position)))
       (mapc #'diag--show
             (seq-uniq
              (seq-mapcat #'flycheck-related-errors errors)))))))

(map! :n "M-<SPC>" 'diag-toggle-line)
(map! :n "S-M-<SPC>" 'diag-toggle-point)

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
        :n "f" 'find-file
        :n "q" 'kill-current-buffer
        :n "Q" '+dired/quit-all
        :n "=" 'dired-diff-dwim)

  (map! :map dired-mode-map
        :localleader
        :n "f" 'find-name-dired))

;; doom
(setq doom-font (font-spec :family "Iosevka Custom Light" :size 18)
      doom-theme 'doom-gruvbox)

(map! "M-[" '+workspace/switch-left
      "M-]" '+workspace/switch-right
      "M-{" '+workspace/swap-left
      "M-}" '+workspace/swap-right)

(map! :leader
      "b a" 'rename-buffer
      "w P" '+popup/raise
      "o t" '+vterm/here
      "o T" nil
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

(setq-default major-mode 'text-mode)

(global-display-fill-column-indicator-mode +1)

(map! :leader
      :prefix "o"
      :desc "Calendar" "c" 'calendar
      :desc "Calculator" "x" 'calc
      :desc "Quick Calculator" "z" 'quick-calc)

(defun toggle-line-numbers ()
  (interactive)
  (if (eq display-line-numbers-type nil)
      (progn
        (setq display-line-numbers-type 'relative)
        (global-display-line-numbers-mode +1))
    (progn
      (setq display-line-numbers-type nil)
      (global-display-line-numbers-mode -1))))

(map! :leader :prefix "t" "l" 'toggle-line-numbers)

;; evil
(setq evil-want-fine-undo t
      +evil-want-o/O-to-continue-comments nil)

(setq evil-normal-state-cursor '(box "#00ff00")
      evil-insert-state-cursor '(bar "#00ff00")
      evil-visual-state-cursor '(hollow "#00ff00"))

(map! :n "gF" 'ffap-other-window
      :n "gJ" 'drag-stuff-down
      :n "gK" 'drag-stuff-up)

;; evil-numbers
(map! :n "g+" 'evil-numbers/inc-at-pt
      :n "g-" 'evil-numbers/dec-at-pt)

;; flycheck
(map! :leader
      :prefix "c"
      "x" 'flycheck-list-errors
      "X" '+default/diagnostics)

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

(map! :n "g'" 'sort-lines
      :n "ga" 'lsp-execute-code-action
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

(map! :n "g[" 'parrot-rotate-prev-word-at-point
      :n "g]" 'parrot-rotate-next-word-at-point)

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

;; rainbow-delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; rustic-mode
(setq rustic-compile-directory-method 'rustic-buffer-workspace)

(defun rustic-rerun-shell-command ()
  "Run previous 'cargo' command."
  (interactive)
  (rustic-run-cargo-command
   (car compile-history)
   (list :mode 'rustic-cargo-run-mode)))

(defun rustic-cargo-check-crate ()
  "Run 'cargo check' on current crate."
  (interactive)
  (let ((cmd "cargo check --tests --all-features"))
    (push cmd compile-history)
    (rustic-run-cargo-command cmd (list :mode 'rustic-cargo-run-mode))))

(defun rustic-cargo-test-crate ()
  "Run 'cargo test' on current crate."
  (interactive)
  (let ((cmd "cargo test --tests --all-features"))
    (push cmd compile-history)
    (rustic-run-cargo-command cmd (list :mode 'rustic-cargo-run-mode))))

(defun rustic-cargo-check-workspace ()
  "Run 'cargo check' on current workspace."
  (interactive)
  (let ((cmd "cargo check --workspace --tests --all-features"))
    (push cmd compile-history)
    (rustic-run-cargo-command cmd (list :mode 'rustic-cargo-run-mode))))

(defun rustic-cargo-test-workspace ()
  "Run 'cargo check' on current workspace."
  (interactive)
  (let ((cmd "cargo test --workspace --tests --all-features"))
    (push cmd compile-history)
    (rustic-run-cargo-command cmd (list :mode 'rustic-cargo-run-mode))))

(after! rustic
  (map! :map rustic-mode-map
        :n "S-M-<up>" 'lsp-rust-analyzer-move-item-up
        :n "S-M-<down>" 'lsp-rust-analyzer-move-item-down)

  (map! :map rustic-mode-map
        :localleader
        "SPC" 'lsp-rust-analyzer-open-cargo-toml
        "b" nil
        "h" 'lsp-rust-analyzer-inlay-hints-mode
        "m" 'lsp-rust-analyzer-expand-macro
        "p" 'lsp-rust-find-parent-module
        "r" 'rustic-rerun-shell-command
        "s" 'rustic-run-shell-command)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("c" . "crate")
        :desc "cargo check" "c" 'rustic-cargo-check-crate
        :desc "cargo test" "t" 'rustic-cargo-test-crate)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("w" . "workspace")
        :desc "cargo check" "c" 'rustic-cargo-check-workspace
        :desc "cargo test" "t" 'rustic-cargo-test-workspace)

  (map! :map rustic-cargo-plain-run-mode-map
        :localleader
        "r"
        'rustic-rerun-shell-command))

;; subword-mode
(map! "C-x s" 'subword-mode
      "C-x S" 'global-subword-mode)

;; undo-tree
(setq undo-tree-visualizer-timestamps t)

;; vertico
(map! :map vertico-map "M-DEL" 'delete-backward-char)

;; vterm
(map! :leader "d" '+vterm/toggle)
