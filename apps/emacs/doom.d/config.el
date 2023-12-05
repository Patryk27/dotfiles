;;; -*- lexical-binding: t; -*-

(load "%ion-mode%")

(when (eq system-type 'darwin)
  (progn
    (setq insert-directory-program "/opt/homebrew/bin/gls")
    (setq mac-pass-command-to-system nil)))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-kkp-mode)

;; -----------------------------------------------------------------------------

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

(map! :n "=" 'evil-avy-goto-char-timer)

(after! avy
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ? avy-dispatch-alist) 'avy-action-mark-to-char)

  (defun avy-action-lookup-documentation (pt)
    (save-excursion
      (goto-char pt)
      (call-interactively '+lookup/documentation))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-lookup-documentation)

  (defun avy-action-lookup-definition (pt)
    (save-excursion
      (goto-char pt)
      (call-interactively '+lookup/definition))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-lookup-definition)

  (defun avy-action-lookup-references (pt)
    (save-excursion
      (goto-char pt)
      (call-interactively '+lookup/references))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?r avy-dispatch-alist) 'avy-action-lookup-references)

  (defun avy-action-lookup-type-definition (pt)
    (save-excursion
      (goto-char pt)
      (call-interactively '+lookup/type-definition))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-lookup-type-definition)

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
        ("d" "~/Documents")
        ("w" "~/Downloads")))

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
  (setq dirvish-hide-details nil)

  (map! :map dirvish-mode-map
        :n "h" 'dired-up-directory
        :n "l" 'dired-find-file
        :n "F" nil
        :n ";" 'dirvish-layout-toggle
        :n "=" 'dired-diff-dwim))

;; doom
(setq doom-font (font-spec :family "Berkeley Mono" :size 14)
      doom-theme 'doom-gruvbox
      +doom-dashboard-functions '(doom-dashboard-widget-banner))

(defun copy-buffer-relative-path ()
  "Copy buffer's relative path to the kill ring."
  (interactive)
  (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

(defun copy-buffer-absolute-path ()
  "Copy buffer's absolute path to the kill ring."
  (interactive)
  (kill-new buffer-file-name))

(map! "s-[" '+workspace/switch-left
      "s-{" '+workspace/swap-left
      "s-]" '+workspace/switch-right
      "s-}" '+workspace/swap-right
      "s-o" '+workspace/new
      "s-p" 'evil-write-all)

(map! :leader
      "b a" 'rename-buffer
      "b p" 'copy-buffer-relative-path
      "b P" 'copy-buffer-absolute-path
      "w P" '+popup/raise
      "o t" '+vterm/here
      "o s" '+eshell/here
      "(" '+workspace/switch-left
      ")" '+workspace/switch-right
      "[" '+workspace/swap-left
      "]" '+workspace/swap-right
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

;; electric
;; https://github.com/doomemacs/doomemacs/issues/6331#issuecomment-1109981584
(defadvice! --nxml-electric-slash-remove-duplicate-right-angle-and-indent (func arg)
  :around 'nxml-electric-slash
  (let ((point-before (point)))
    (funcall func arg)
    (unless (equal (+ 1 point-before) (point))
      (delete-char 1)
      (funcall indent-line-function))))

;; emacs
(setq calendar-week-start-day 1
      custom-file (file-name-concat doom-local-dir "custom.el")
      display-line-numbers-type nil
      user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me")

(setq-default major-mode 'text-mode)

(global-display-fill-column-indicator-mode +1)

(map! :n "\\" '+default/search-buffer
      :ni "s-i" 'insert-char
      :ni "s-h" 'evil-window-left
      :ni "s-H" '+evil/window-move-left
      :ni "s-j" 'evil-window-down
      :ni "s-J" '+evil/window-move-down
      :ni "s-k" 'evil-window-up
      :ni "s-K" '+evil/window-move-up
      :ni "s-l" 'evil-window-right
      :ni "s-L" '+evil/window-move-right)

(defun calc-eval-region (arg beg end)
  "Calculate region and replace it with the result."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (kill-region beg end)
    (insert result)))

(map! :leader
      :prefix ("=" . "calc")
      :desc "eval" "=" 'calc-eval-region
      :desc "grab" "g" 'calc-grab-rectangle
      :desc "dispatch" "d" 'calc-dispatch)

(map! :v "=" 'calc-eval-region)

(map! :leader
      :prefix "o"
      :desc "Calendar" "c" 'calendar)

(defun toggle-line-numbers ()
  (interactive)
  (if (eq display-line-numbers-type nil)
      (progn
        (setq display-line-numbers-type t)
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

(defun remove-ansi ()
  "Remove ANSI codes from buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (ansi-color-filter-region (region-beginning) (region-end))))

(defun next-error-in-different-file ()
  "Like `next-error', but looks for the error in a different file."
  (interactive)
  (let ((buffer (next-error-find-buffer)))
    (when buffer
      (with-current-buffer buffer
        (compilation-next-file 1)
        (compile-goto-error)))))

(map! :n "] E" 'next-error-in-different-file)

(map! :map 'override
      :v "v" #'er/expand-region
      :v "V" #'er/contract-region)

;; eshell
(defun +eshell/here ()
  (interactive)
  (eshell 'N))

(defun eshell/bcat (&rest args)
  "Output the contents of one or more buffers as a string. "
  (let ((buffers (mapcar #'get-buffer args)))
    (mapconcat (lambda (buf)
                 (save-window-excursion
                   (switch-to-buffer buf)
                   (buffer-substring-no-properties (point-min) (point-max))))
               buffers "\n")))

;; evil
(setq evil-want-fine-undo t
      +evil-want-o/O-to-continue-comments nil)

(setq evil-normal-state-cursor '(box "#00ff00")
      evil-insert-state-cursor '(bar "#00ff00")
      evil-visual-state-cursor '(hollow "#00ff00"))

;; evil-numbers
(map! :n "g=" 'evil-numbers/inc-at-pt
      :n "g-" 'evil-numbers/dec-at-pt)

;; flycheck
(map! :leader
      :prefix "c"
      "x" 'flycheck-list-errors
      "X" '+default/diagnostics)

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
      lsp-inlay-hint-enable t
      lsp-lens-enable nil
      lsp-rust-all-features t
      lsp-rust-analyzer-proc-macro-enable t
      lsp-signature-auto-activate nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil)

(map! :n "z;" 'sort-lines
      :n "ga" '+lookup/references
      :n "gD" nil
      :n "gt" '+lookup/type-definition)

;; magit
(defun magit-copy-buffer-name ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is no current branch"))))

(map! "s-g" 'magit-status)

(map! :map magit-status-mode-map
      :n "yn" 'magit-copy-buffer-name)

;; markdown-mode
(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :prefix ("t" . "table")
        :desc "align" "a" 'markdown-table-align))

;; org
(setq org-agenda-files '("~/Documents/" "~/Documents/praca" "~/Documents/wycieczki")
      org-directory "~/Documents")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/todo.org" "Inbox")
         "* %?" :prepend t)))

(defun org-capture-todo ()
  (interactive)
  (org-capture nil "t"))

(map! "s-§" 'org-agenda-list
      "M-§" 'org-capture-todo)

;; parinfer
(setq-default parinfer-rust-library "%parinfer%")

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

(map! :n "z[" 'parrot-rotate-prev-word-at-point
      :n "z]" 'parrot-rotate-next-word-at-point)

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
  "Run `cargo check' on current crate."
  (interactive)
  (rustic-run-cargo-command
   "cargo check --tests --benches --all-features"
   (list :mode 'rustic-cargo-run-mode
         :directory (rustic-buffer-crate))))

(defun rustic-cargo-test-crate ()
  "Run `cargo test' on current crate."
  (interactive)
  (rustic-run-cargo-command
   "cargo test --all-features"
   (list :mode 'rustic-cargo-run-mode
         :directory (rustic-buffer-crate))))

(defun rustic-cargo-clippy-workspace ()
  "Run `cargo clippy' on current workspace."
  (interactive)
  (rustic-run-cargo-command
   "cargo clippy --workspace --tests --benches --all-features"
   (list :mode 'rustic-cargo-clippy-mode)))

(defun rustic-cargo-check-workspace ()
  "Run `cargo check' on current workspace."
  (interactive)
  (rustic-run-cargo-command
   "cargo check --workspace --tests --benches --all-features"
   (list :mode 'rustic-cargo-run-mode)))

(defun rustic-cargo-test-workspace ()
  "Run `cargo test' on current workspace."
  (interactive)
  (rustic-run-cargo-command
   "cargo test --workspace --all-features"
   (list :mode 'rustic-cargo-run-mode)))

(defun rustic-open-main-rs ()
  "Open closest `main.rs'."
  (interactive)
  (let ((file (locate-dominating-file "." "Cargo.toml")))
    (when file
      (find-file (concat file "src/main.rs")))))

(defun rustic-open-lib-rs ()
  "Open closest `lib.rs'."
  (interactive)
  (let ((file (locate-dominating-file "." "Cargo.toml")))
    (when file
      (find-file (concat file "src/lib.rs")))))

(after! rustic
  ;; TODO https://github.com/brotzeit/rustic/issues/450
  (defun rustic-save-some-buffers-advice (orig-fun &rest args)
    (apply orig-fun args))

  (map! :map rustic-mode-map
        :n "s-<up>" 'lsp-rust-analyzer-move-item-up
        :n "s-<down>" 'lsp-rust-analyzer-move-item-down)

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
        :desc "main.rs" "m" 'rustic-open-main-rs
        :desc "lib.rs" "l" 'rustic-open-lib-rs
        :desc "Cargo.toml" "c" 'lsp-rust-analyzer-open-cargo-toml)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("c" . "crate")
        :desc "cargo check" "c" 'rustic-cargo-check-crate
        :desc "cargo test" "t" 'rustic-cargo-test-crate)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("w" . "workspace")
        :desc "cargo check" "k" 'rustic-cargo-clippy-workspace
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

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq orderless-smart-case-initial orderless-smart-case)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq orderless-smart-case orderless-smart-case-initial)))

(defun vertico-toggle-case-sensitivity ()
  "Toggle case sensitivity of the current search."
  (interactive)
  (setq orderless-smart-case (not orderless-smart-case))
  (when (= (how-many "#" (point-min) (point-max)) 1)
    (insert (minibuffer-contents-no-properties)))
  (consult-vertico--refresh))

(after! vertico
  (map! :map vertico-map
        "DEL" #'backward-delete-char
        "C-DEL" #'vertico-directory-delete-char
        "s-i" #'vertico-toggle-case-sensitivity
        "s-j" #'vertico-quick-jump))

;; vterm
(map! "s-s" '+vterm/toggle)

(add-hook 'vterm-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

;; xml-mode
(defun +format--buffer-maybe-xml (orig)
  (if (eq major-mode 'xml-mode)
      (save-excursion
        (shell-command-on-region (mark) (point) "xmllint --encode utf-8 --format -" (buffer-name) t))
    (funcall orig)))

(advice-add '+format--buffer :around '+format--buffer-maybe-xml)
