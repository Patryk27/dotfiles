;;; -*- lexical-binding: t; -*-

(load "%ion-mode%")
(load "%llvm-mode%")
(toggle-frame-fullscreen)

(defun no-op () nil)

;; -----------------------------------------------------------------------------
;; atomic-chrome

(setq atomic-chrome-default-major-mode 'markdown-mode)

(atomic-chrome-start-server)

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; calc

(map! :leader
      :desc "calc" "=" 'calc)

(map! :v "=" 'calc-eval-region)

(map! :map calc-mode-map
      :desc "yank" "s-y" 'calc-copy-top)

(defun calc-copy-top ()
  "Copy the thing at the top of the calc stack."
  (interactive)
  (let ((val (calc-top)))
    (kill-new (if (Math-scalarp val)
                  (math-format-number val)
                (math-format-flat-expr-fancy val 0)))))

(defun calc-eval-region (_arg beg end)
  "Calculate region and replace it with the result."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (if (equal current-prefix-arg nil)
        (progn
          (kill-region beg end)
          (insert result))
      (progn
        (goto-char end)
        (insert "\n= ")
        (insert result)))))

;; -----------------------------------------------------------------------------
;; csharp-mode

(after! csharp-mode
  (set-popup-rule! "^\\*csharp-compilation" :vslot -1)

  (map! :map csharp-mode-map
        :localleader
        :desc "build" "b" 'csharp-dotnet-build
        :desc "test" "t" 'csharp-dotnet-test)

  (defun csharp-dotnet-build ()
    "Run `dotnet build' on current solution."
    (interactive)
    (compile "dotnet build"))

  (defun csharp-dotnet-test ()
    "Run `dotnet test' on current solution."
    (interactive)
    (compile "dotnet test")))

;; -----------------------------------------------------------------------------
;; dired / dirvish

(map! :leader "j" 'dired-jump)

(after! (:and evil dired)
  (setq dirvish-attributes '(file-time file-size)
        dirvish-hide-details nil
        dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'")

  (setq dirvish-quick-access-entries
        '(
          ("d" "~/Downloads")
          ("f" "/scp:gateway:/var/lib/nixos-containers/nginx/var/www/files")
          ("h" "~")
          ("o" "~/Documents")
          ("p" "/p")
          ("q" "/q")
          ("x" "/x")))

  (map! :map dirvish-mode-map
        :n "?" 'dirvish-dispatch
        :n "a" 'dirvish-quick-access
        :n "s" 'dirvish-quicksort
        :n "-" 'find-file
        :n "h" 'dired-up-directory
        :n "l" 'dired-find-file
        :n "\\" 'dirvish-narrow
        :n "|" 'dirvish-fd
        :n "<tab>" 'dirvish-toggle-subtree
        :n ";" 'dirvish-layout-toggle
        :n "=" 'dired-diff-dwim)

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

  ;; TODO https://github.com/alexluigit/dirvish/pull/251
  (defun dirvish--mode-line-fmt-setter (left right &optional header)
    "Set the `dirvish--mode-line-fmt'.
LEFT and RIGHT are segments aligned to left/right respectively.
If HEADER, set the `dirvish--header-line-fmt' instead."
    (cl-labels ((expand (segments)
                  (cl-loop for s in segments collect
                           (if (stringp s) s
                             `(:eval (,(intern (format "dirvish-%s-ml" s)) (dirvish-curr))))))
                (get-font-scale ()
                  (let* ((face (if header 'header-line 'mode-line-inactive))
                         (defualt (face-attribute 'default :height))
                         (ml-height (face-attribute face :height)))
                    (cond ((floatp ml-height) ml-height)
                          ((integerp ml-height) (/ (float ml-height) defualt))
                          (t 1)))))
      `((:eval
         (let* ((dv (dirvish-curr))
                (buf (and (car (dv-layout dv)) (cdr (dv-index dv))))
                (scale ,(get-font-scale))
                (win-width (floor (/ (window-width) scale)))
                (str-l (format-mode-line
                        ',(or (expand left) mode-line-format) nil nil buf))
                (str-r (format-mode-line ',(expand right) nil nil buf))
                (len-r (string-width str-r)))
           (concat
            (dirvish--bar-image (car (dv-layout dv)) ,header)
            (if (< (+ (string-width str-l) len-r) win-width)
                str-l
              (let ((trim (1- (- win-width len-r))))
                (if (>= trim 0)
                    (substring str-l 0 (min trim (1- (length str-l))))
                  "")))
            (propertize
             " " 'display
             `((space :align-to (- (+ right right-fringe right-margin)
                                   ,(ceiling (* scale (string-width str-r)))))))
            str-r)))))))

;; -----------------------------------------------------------------------------
;; doom

(setq doom-font (font-spec :family "Berkeley Mono" :size 16.5)
      doom-theme 'doom-gruvbox
      +doom-dashboard-functions '(doom-dashboard-widget-banner))

(map! "C-{" '+workspace/switch-left
      "C-}" '+workspace/switch-right)

(map! :leader
      "b a" 'rename-buffer
      "b p" 'copy-buffer-relative-path
      "b P" 'copy-buffer-absolute-path
      "w P" '+popup/raise
      "o s" '+vterm/here
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

(defun copy-buffer-relative-path ()
  "Copy buffer's relative path to the kill ring."
  (interactive)
  (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

(defun copy-buffer-absolute-path ()
  "Copy buffer's absolute path to the kill ring."
  (interactive)
  (kill-new buffer-file-name))

;; -----------------------------------------------------------------------------
;; diary

(defun open-diary (&rest _)
  "Open the diary."
  (interactive)

  (tramp-cleanup-connection
   (tramp-dissect-file-name "/scp:warp:"))

  (unless (file-exists-p "/scp:warp:/mnt/diary/.mounted")
    (progn
      (eshell-command
       (format
        "echo %s | ssh warp -- 'gocryptfs /var/lib/storages/diary /mnt/diary'"
        (read-passwd "Password: ")))))

  (evil-normal-state)
  (dired "/scp:warp:/mnt/diary"))

(defun close-diary (&rest _)
  "Close the diary."
  (interactive)

  (tramp-cleanup-connection
   (tramp-dissect-file-name "/scp:warp:"))

  (if (file-exists-p "/scp:warp:/mnt/diary/.mounted")
      (progn
        (eshell-command "ssh warp -- 'umount /mnt/diary'"))))

;; -----------------------------------------------------------------------------
;; doom-modeline

(after! doom-modeline
  (advice-add 'doom-modeline-segment--vcs :override 'no-op))

;; -----------------------------------------------------------------------------
;; ediff

(map! :leader
      :prefix "b"
      :desc "ediff" "=" 'ediff-buffers)

;; -----------------------------------------------------------------------------
;; emacs

(global-display-fill-column-indicator-mode +1)

(setq calendar-week-start-day 1
      display-line-numbers-type nil
      user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me"
      warning-minimum-level :error)

(setq-default major-mode 'text-mode)

(map! :map minibuffer-mode-map
      "C-u" 'universal-argument)

(map! :n "\\" '+default/search-buffer
      :n "] E" 'next-error-in-different-file)

(map! :leader
      :prefix "b"
      :desc "Kill stale buffers" "DEL" 'kill-stale-buffers)

(map! :leader
      :prefix "o"
      :desc "Calendar" "c" 'calendar)

(map! :leader
      :prefix "t"
      :desc "Line numbers" "l" 'toggle-line-numbers)

(map! :map 'override
      :v "v" 'er/expand-region
      :v "V" 'er/contract-region)

(defun toggle-line-numbers ()
  (interactive)
  (if (eq display-line-numbers-type nil)
      (progn
        (setq display-line-numbers-type t)
        (global-display-line-numbers-mode +1))
    (progn
      (setq display-line-numbers-type nil)
      (global-display-line-numbers-mode -1))))

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

;; -----------------------------------------------------------------------------
;; evil

(setq evil-want-fine-undo t
      evil-normal-state-cursor '(box "#00ff00")
      evil-insert-state-cursor '(bar "#00ff00")
      evil-visual-state-cursor '(hollow "#00ff00")
      evil-replace-state-cursor '(hbar "#00ff00"))

(map! :ni "C-<tab>" 'evil-write-all
      :nv "C-e" 'evil-scroll-up
      :nv "C-u" 'evil-scroll-line-down
      :ni "C-<left>" 'evil-beginning-of-line
      :ni "C-<right>" 'evil-end-of-line
      :n "z;" 'sort-lines
      :n "ga" '+lookup/references
      :n "gD" nil
      :n "gt" '+lookup/type-definition
      :n "gh" '+lookup/domentation)

;; -----------------------------------------------------------------------------
;; evil-numbers

(map! :n "g=" 'evil-numbers/inc-at-pt
      :n "g-" 'evil-numbers/dec-at-pt)

;; -----------------------------------------------------------------------------
;; flycheck

(map! :leader
      :prefix "c"
      "x" 'flycheck-list-errors
      "X" '+default/diagnostics)

;; -----------------------------------------------------------------------------
;; hl-line

(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

;; -----------------------------------------------------------------------------
;; indent-bars

(use-package indent-bars
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-width-frac 0.12)
  :hook ((emacs-lisp-mode rustic-mode) . indent-bars-mode))

;; -----------------------------------------------------------------------------
;; ion-mode

(map! :map ion-mode-map
      :localleader
      :desc "reformat region" "f" 'ion-reformat-region)

;; -----------------------------------------------------------------------------
;; json

(after! json
  (defun +format--buffer-maybe-json-a (fn &rest args)
    (if (eq major-mode 'json-mode)
        (json-pretty-print-buffer)
      (apply fn args)))

  (advice-add '+format--buffer :around '+format--buffer-maybe-json-a))

;; -----------------------------------------------------------------------------
;; lsp

(setq lsp-file-watch-threshold 5000
      lsp-lens-enable nil
      lsp-rust-all-features t
      lsp-signature-auto-activate nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil
      lsp-use-plists t)

(after! lsp-mode
  (defun +format-with-lsp-mode--maybe-disable-a (fn &rest args)
    (let ((file (buffer-file-name)))
      (unless (or (string-suffix-p ".vue" file)
                  (string-suffix-p ".ts" file))
        (apply fn args))))

  (advice-add '+format-with-lsp-mode :around '+format-with-lsp-mode--maybe-disable-a))

;; -----------------------------------------------------------------------------
;; lsp-nix

(after! lsp-mode
  (use-package lsp-nix
    :demand t
    :custom
    (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

  (use-package nix-mode
    :hook (nix-mode . lsp-deferred)))

;; -----------------------------------------------------------------------------
;; magit

(after! magit
  (map! :map magit-status-mode-map
        :n "yn" 'magit-copy-buffer-name)

  (defun magit-copy-buffer-name ()
    "Show the current branch in the echo-area and add it to the `kill-ring'."
    (interactive)
    (let ((branch (magit-get-current-branch)))
      (if branch
          (progn (kill-new branch)
                 (message "%s" branch))
        (user-error "There is no current branch")))))

;; -----------------------------------------------------------------------------
;; markdown-mode

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :prefix ("t" . "table")
        :desc "align" "a" 'markdown-table-align))

;; -----------------------------------------------------------------------------
;; nxml-mode

(after! nxml-mode
  (defun +format--buffer-maybe-xml-a (fn &rest args)
    (if (eq major-mode 'xml-mode)
        (save-excursion
          (shell-command-on-region
           (mark)
           (point)
           "xmllint --encode utf-8 --format -"
           (buffer-name)
           t))
      (apply fn args)))

  (advice-add '+format--buffer :around '+format--buffer-maybe-xml-a))

;; -----------------------------------------------------------------------------
;; org

(setq org-agenda-files '("~/Documents/org/")
      org-directory "~/Documents/org/"
      org-log-into-drawer t
      org-hide-emphasis-markers t)

(map! :leader
      "e" 'org-agenda-list
      "E" 'org-capture-todo
      "\\" '+default/org-notes-headlines
      "|" '+default/org-notes-search)

(defun org-capture-todo ()
  (interactive)
  (org-capture nil "t"))

(defun org-open-next-section ()
  (interactive)
  (org-forward-heading-same-level nil)
  (+org/open-fold))

(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/todo.org" "Inbox")
           "* %?" :prepend t))))

;; -----------------------------------------------------------------------------
;; pcre2el

(defmacro prx (&rest expressions)
  "Convert the rx-compatible regular EXPRESSIONS to PCRE."
  `(rxt-elisp-to-pcre (rx ,@expressions)))

;; -----------------------------------------------------------------------------
;; projectile

(setq projectile-project-search-path '("/x" "/x/anixe" "~/.emacs.d")
      projectile-track-known-projects-automatically nil
      projectile-verbose nil)

(map! :leader
      :prefix "p"
      "P" 'projectile-switch-open-project)

(after! projectile
  (setq projectile-switch-project-action 'projectile-commander))

;; -----------------------------------------------------------------------------
;; rainbow-delimeters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; -----------------------------------------------------------------------------
;; rustic-mode

(setq rustic-compile-directory-method 'rustic-buffer-workspace)

(after! rustic
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

  ;; TODO https://github.com/brotzeit/rustic/issues/450
  (defun rustic-save-some-buffers-advice (orig-fun &rest args)
    (apply orig-fun args))

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

;; -----------------------------------------------------------------------------
;; spell-fu

(after! spell-fu
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "pl"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "sv")))

;; -----------------------------------------------------------------------------
;; subword-mode

(map! "<f1>" 'subword-mode
      "C-<f1>" 'global-subword-mode)

;; -----------------------------------------------------------------------------
;; typescript-mode

(setq-default typescript-indent-level 2)

;; -----------------------------------------------------------------------------
;; undo-tree

(setq undo-tree-visualizer-timestamps t)

;; -----------------------------------------------------------------------------
;; vertico

(after! vertico
  (map! :map vertico-map
        "DEL" 'backward-delete-char
        "C-DEL" 'vertico-directory-delete-char))

;; -----------------------------------------------------------------------------
;; vlf

(require 'vlf-setup)

;; vterm
(map! :leader "d" '+vterm/toggle)

(add-hook 'vterm-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

;; -----------------------------------------------------------------------------
;; web-mode

(after! web-mode
  (map! :map web-mode-map
        :localleader
        "c" 'web-run-vue-tsc)

  (map! :map typescript-mode-map
        :localleader
        "c" 'web-run-vue-tsc)

  (defun web-run-vue-tsc ()
    "Run `vue-tsc' on current project."
    (interactive)
    (compile "npm exec vue-tsc"))

  ;; ---

  (defun lsp-vue-activate ()
    (when (string-suffix-p ".vue" (buffer-file-name))
      (lsp)))

  (add-hook 'web-mode-hook 'lsp-vue-activate))
