;;; -*- lexical-binding: t; -*-

(load "/home/pwy/.doom.d/config/ion-mode.el")

;; ace-window
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white"
    :background "red"
    :weight bold))

(setq aw-keys '(?a ?s ?d ?j ?k ?l))

(map! :leader "k" 'ace-window)

;; atomic-chrome
(setq atomic-chrome-default-major-mode 'markdown-mode)

(atomic-chrome-start-server)

;; avy
(setq avy-keys '(?a ?s ?d ?j ?k ?l))

(map! :ni "M-j" 'evil-avy-goto-char-timer
      :ni "M-k" 'evil-avy-goto-char-in-line)

(after! avy
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  (defun avy-action-lookup-documentation (pt)
    (save-excursion
      (goto-char pt)
      (call-interactively '+lookup/documentation))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-lookup-documentation)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

;; dired / dirvish
(map! :leader "j" 'dired-jump)

(setq dirvish-quick-access-entries
      '(
        ("d" "/downloads")
        ("o" "~/org")))

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
  (setq dirvish-attributes '())

  (map! :map dirvish-mode-map
        :n ";" 'dirvish-toggle-fullscreen
        :n "=" 'dired-diff-dwim
        :n "?" 'dirvish-dispatch
        :n "F" nil
        :n "b" 'dirvish-quick-access
        :n "q" 'dirvish-quit))

;; doom
(setq doom-font (font-spec :family "Iosevka Custom Light" :size 18)
      doom-theme 'doom-gruvbox)

(map! :leader
      "b a" 'rename-buffer
      "w P" '+popup/raise
      "o t" '+vterm/here
      "o T" nil
      "[" '+workspace/switch-left
      "TAB [" nil
      "]" '+workspace/switch-right
      "TAB ]" nil
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

(map! :n "\\" '+default/search-buffer
      :ni "M-i" 'insert-char)

(defun calc-eval-region (arg beg end)
  "Calculate region and replace it with the result."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (kill-region beg end)
    (insert result)))

(map! :leader
      :prefix ("=" . "calc")
      :desc "eval" "RET" 'calc-eval-region
      :desc "grab" "g" 'calc-grab-rectangle
      :desc "dispatch" "=" 'calc-dispatch)

(map! :leader
      :prefix "o"
      :desc "Calendar" "c" 'calendar)

(defun toggle-line-numbers ()
  (interactive)
  (if (eq display-line-numbers-type nil)
      (progn
        (setq display-line-numbers-type 'relative)
        (global-display-line-numbers-mode +1))
    (progn
      (setq display-line-numbers-type nil)
      (global-display-line-numbers-mode -1))))

(map! :leader
      :prefix "t"
      :desc "Line numbers" "l" 'toggle-line-numbers)

(defun buffer-fresh-p (buffer)
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))

(defun kill-stale-buffers ()
  (interactive)
  (mapc 'kill-buffer (-remove 'buffer-fresh-p (buffer-list))))

(map! :leader
      :prefix "b"
      :desc "Kill stale buffers" "DEL" 'kill-stale-buffers)

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

;; flycheck
(map! :leader
      :prefix "c"
      "x" 'flycheck-list-errors
      "X" '+default/diagnostics)

;; focus-mode
(map! :leader
      :prefix "-"
      "-" 'focus-mode
      "p" 'focus-mode-pin
      "i" 'focus-change-thing)

;; gcmh
(setq gcmh-high-cons-threshold (* 128 1024 1024)
      gcmh-idle-delay 10.0)

;; hl-line
(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

;; ion-mode
(map! :map ion-mode-map
      :localleader
      :desc "reformat region" "f" 'ion-reformat-region)

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
      lsp-headerline-breadcrumb-enable t
      lsp-lens-enable nil
      lsp-rust-all-features t
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-signature-auto-activate nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil)

(map! :n "z;" 'sort-lines
      :n "ga" '+lookup/references
      :n "gD" nil
      :n "gt" '+lookup/type-definition)

;; markdown-mode
(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :prefix ("t" . "table")
        :desc "align" "a" 'markdown-table-align))

;; olivetti-mode
(map! "M-RET" 'olivetti-mode)

;; org
(setq org-agenda-files '("~/org/" "~/org/praca" "~/org/wycieczki")
      org-directory "~/org/")

;; org-modern
(global-org-modern-mode)

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

(map! :n "z(" 'parrot-rotate-prev-word-at-point
      :n "z)" 'parrot-rotate-next-word-at-point)

;; powerthesaurus
(map! :n "zv" 'powerthesaurus-lookup-dwim)

;; projectile
(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil)

(map! :leader
      :prefix "p"
      "P" 'projectile-switch-open-project)

(after! projectile
  (setq projectile-switch-project-action #'projectile-commander))

;; rainbow-delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; rustic-mode
(setq rustic-compile-directory-method 'rustic-buffer-workspace)

(defun rustic-cargo-check-crate ()
  "Run 'cargo check' on current crate."
  (interactive)
  (rustic-run-cargo-command
   "cargo check --tests --all-features"
   (list :mode 'rustic-cargo-run-mode
         :directory (rustic-buffer-crate))))

(defun rustic-cargo-test-crate ()
  "Run 'cargo test' on current crate."
  (interactive)
  (rustic-run-cargo-command
   "cargo test --all-features"
   (list :mode 'rustic-cargo-run-mode
         :directory (rustic-buffer-crate))))

(defun rustic-cargo-check-workspace ()
  "Run 'cargo check' on current workspace."
  (interactive)
  (rustic-run-cargo-command
   "cargo check --workspace --tests --all-features"
   (list :mode 'rustic-cargo-run-mode)))

(defun rustic-cargo-test-workspace ()
  "Run 'cargo test' on current workspace."
  (interactive)
  (rustic-run-cargo-command
   "cargo test --workspace --all-features"
   (list :mode 'rustic-cargo-run-mode)))

(defun rustic-open-lib-rs()
  "Open related 'lib.rs' file."
  (interactive)
  (let ((file (locate-dominating-file "." "Cargo.toml")))
    (when file
      (find-file (concat file "src/lib.rs")))))

(after! rustic
  (map! :map rustic-mode-map
        :n "S-M-<up>" 'lsp-rust-analyzer-move-item-up
        :n "S-M-<down>" 'lsp-rust-analyzer-move-item-down)

  (map! :map rustic-mode-map
        :localleader
        "b" nil
        "h" 'lsp-rust-analyzer-inlay-hints-mode
        "m" 'lsp-rust-analyzer-expand-macro
        "p" 'lsp-rust-find-parent-module
        "r" 'rustic-rerun-shell-command
        "s" 'rustic-run-shell-command
        "t" nil)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("o" . "open")
        :desc "lib.rs" "l" 'rustic-open-lib-rs
        :desc "Cargo.toml" "o" 'lsp-rust-analyzer-open-cargo-toml)

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
(setq vertico-quick1 "asd"
      vertico-quick2 "jkl")

(map! :map vertico-map
      "M-q" #'vertico-quick-insert)

;; vterm
(map! :leader "d" '+vterm/toggle)
