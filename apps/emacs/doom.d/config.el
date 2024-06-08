;;; -*- lexical-binding: t; -*-

(load "%ion-mode%")
(load "%llvm-mode%")

(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

(global-kkp-mode)
(toggle-frame-fullscreen)

(defun no-op () nil)

;; -----------------------------------------------------------------------------
;; ace-window

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white"
    :background "red"
    :weight bold))

(setq aw-keys '(?a ?s ?d ?j ?k ?l))

(map! :leader "k" 'ace-window)

;; -----------------------------------------------------------------------------
;; alert

(require 'alert)

(setq alert-default-style 'osx-notifier)

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

(set-popup-rule! "^\\*csharp-compilation" :vslot -1)

(after! csharp-mode
  (map! :map csharp-mode-map
        :localleader
        :desc "build" "b" 'csharp-dotnet-build
        :desc "test" "t" 'csharp-dotnet-test))

(defun csharp-dotnet-build ()
  "Run `dotnet build' on current solution."
  (interactive)
  (csharp-compilation "dotnet" "build"))

(defun csharp-dotnet-test ()
  "Run `dotnet test' on current solution."
  (interactive)
  (csharp-compilation "dotnet" "test"))

(defun csharp-compilation (command args)
  (let ((buffer (get-buffer-create "*csharp-compilation*")))
    (with-current-buffer buffer
      (setq-local buffer-read-only t
                  default-directory (projectile-project-root))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s %s \n" command args)))
      (display-buffer buffer)
      (let ((process (start-process "csharp-dotnet-test" buffer command args)))
        (set-process-filter process 'csharp-compilation-filter)))))

(defun csharp-compilation-filter (process output)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (window (get-buffer-window buffer)))
          (goto-char (point-max))
          (insert output)
          (set-window-point window (point-max)))))))

;; -----------------------------------------------------------------------------
;; dired / dirvish

(map! :leader "j" 'dired-jump)

(setq dirvish-quick-access-entries
      '(
        ("q" "~/Desktop/queue")
        ("o" "~/Documents")
        ("d" "~/Downloads")
        ("i" "/private/diary")
        ("f" "/scp:gateway:/var/lib/nixos-containers/nginx/var/www/files")))

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
  (setq dirvish-attributes '(file-time file-size)
        dirvish-hide-details nil
        dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'")

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

(setq doom-font (font-spec :family "Berkeley Mono" :size 14)
      doom-theme 'doom-gruvbox
      +doom-dashboard-functions '(doom-dashboard-widget-banner))

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
      "o t" 'eat
      "(" '+workspace/switch-left
      ")" '+workspace/switch-right
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
;; eat

(add-hook 'eshell-load-hook #'eat-eshell-mode)

(add-hook 'eat-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

(defun +eat/fix-keys ()
  (when (bound-and-true-p eat--eshell-char-mode)
    (evil-emacs-state)

    (map! :map eat-eshell-char-mode-map
          :i "ESC" 'eat-self-input
          :i "C-c" 'eat-self-input
          :in "s-v" 'eat-yank))

  (when (bound-and-true-p eat--eshell-semi-char-mode)
    (map! :map eat-eshell-semi-char-mode-map
          :in "s-v" 'eat-yank))

  (evil-normal-state))

(add-hook 'eat--eshell-char-mode-hook '+eat/fix-keys)
(add-hook 'eat--eshell-semi-char-mode-hook '+eat/fix-keys)

;; -----------------------------------------------------------------------------
;; ediff

(map! :leader
      :prefix "b"
      :desc "ediff" "=" 'ediff-buffers)

;; -----------------------------------------------------------------------------
;; emacs

(global-display-fill-column-indicator-mode +1)

(setq calendar-week-start-day 1
      custom-file (file-name-concat doom-local-dir "custom.el")
      display-line-numbers-type nil
      user-full-name "Patryk Wychowaniec"
      user-mail-address "pwychowaniec@pm.me"
      warning-minimum-level :error)

(setq-default major-mode 'text-mode)

(map! :n "\\" '+default/search-buffer
      :n "] E" 'next-error-in-different-file
      :ni "s-i" 'insert-char
      :ni "s-h" 'evil-window-left
      :ni "s-H" '+evil/window-move-left
      :ni "s-j" 'evil-window-down
      :ni "s-J" '+evil/window-move-down
      :ni "s-k" 'evil-window-up
      :ni "s-K" '+evil/window-move-up
      :ni "s-l" 'evil-window-right
      :ni "s-L" '+evil/window-move-right)

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
      :v "v" #'er/expand-region
      :v "V" #'er/contract-region)

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
;; eshell

(map! "s-s" '+eshell/toggle)
(map! :leader "o s" '+eshell/here)

;;;; ----

(advice-add 'eshell-did-you-mean-setup :override 'no-op)
(advice-add 'setup-esh-help-eldoc :override 'no-op)

(defvar eshell-global-history-ring nil
  "The history ring shared across Eshell sessions.")

(defun +eshell/use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-global-history-ring
    (when eshell-history-file-name
      (eshell-read-history nil t))
    (setq eshell-global-history-ring
      (or eshell-history-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-global-history-ring))

(add-hook 'eshell-mode-hook '+eshell/use-global-history)

(defun eshell/bcat (&rest args)
  "Output the contents of one or more buffers as a string. "
  (let ((buffers (mapcar #'get-buffer args)))
    (mapconcat (lambda (buf)
                 (save-window-excursion
                   (switch-to-buffer buf)
                   (buffer-substring-no-properties (point-min) (point-max))))
               buffers "\n")))

(after! eshell
  (defvar +eshell--id nil)

  (map! :map eshell-mode-map
        "s-r" 'consult-history)

  (setq eshell-bad-command-tolerance 999
        eshell-history-size 4096
        eshell-prompt-function '+eshell/prompt
        eshell-prompt-regexp "λ ")

  (add-to-list 'eshell-modules-list 'eshell-elecslash)

  (set-eshell-alias!
   "ca" "clear && cargo $*"
   "cab" "clear && cargo build $*"
   "cabr" "clear && cargo build --release $*"
   "cac" "clear && cargo check $*"
   "cacw" "clear && cargo check --workspace --tests $*"
   "caf" "clear && cargo fmt $*"
   "car" "clear && cargo run $*"
   "carb" "clear && RUST_BACKTRACE=1 cargo run $*"
   "carr" "clear && cargo run --release $*"
   "carrb" "clear && RUST_BACKTRACE=1 cargo run --release $*"
   "cate" "clear && cargo test --quiet $*"
   "cateb" "clear && RUST_BACKTRACE=1 cargo test $*"
   "cater" "clear && cargo test --quiet --release $*"
   "catew" "clear && cargo test --quiet --workspace $*"
   "catewr" "clear && cargo test --quiet --workspace --release $*"
   "catewb" "clear && RUST_BACKTRACE=1 cargo test --workspace $*"
   "catewf" "clear && cargo test --all-features --quiet --workspace $*"
   "catewfb" "clear && RUST_BACKTRACE=1 cargo test --all-features --workspace $*"
   "cau" "clear && cargo update $*"
   "caup" "clear && cargo update --package $*"
   "cds" "eshell/cd /scp:$1:/"
   "d" "docker $*"
   "dc" "docker-compose $*"
   "ssh-copy-terminfo" "infocmp | ssh $1 tic -")

  (defun +eshell/toggle (&rest _)
    "Toggle eshell popup window."
    (interactive "P")
    (let ((buffer-name
           (get-buffer-create
            (format "*doom:eshell-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      "main")))))
      (if-let (win (get-buffer-window buffer-name))
          (delete-window win)
        (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'eshell-mode)
                                   if (equal (buffer-local-value '+eshell--id buf)
                                             buffer-name)
                                   return buf)
                          (get-buffer-create buffer-name))))
          (with-current-buffer buffer
            (setq-local +eshell--id buffer-name)
            (unless (eq major-mode 'eshell-mode)
              (progn
                (setq-local default-directory (or (doom-project-root) default-directory))
                (eshell-mode))))
          (pop-to-buffer buffer)))
      (get-buffer buffer-name)))

  (defun +eshell/prompt ()
    (require 'shrink-path)
    (concat (if (bobp) "" "\n")
            (let ((pwd (eshell/pwd)))
              (propertize (if (or (file-remote-p pwd) (equal pwd "~"))
                              pwd
                            (abbreviate-file-name pwd))
                          'face '+eshell-prompt-pwd))
            "\n"
            (propertize "λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " ")))

;; -----------------------------------------------------------------------------
;; evil

(setq evil-want-fine-undo t
      +evil-want-o/O-to-continue-comments nil)

(setq evil-normal-state-cursor '(box "#00ff00")
      evil-insert-state-cursor '(bar "#00ff00")
      evil-visual-state-cursor '(hollow "#00ff00")
      evil-replace-state-cursor '(hbar "#00ff00"))

(map! "s-e" 'evil-scroll-up
      "s-r" 'evil-scroll-line-up
      "s-d" 'evil-scroll-down
      "s-f" 'evil-scroll-line-down
      "s-W" '+workspace/delete
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
;; ion-mode

(map! :map ion-mode-map
      :localleader
      :desc "reformat region" "f" 'ion-reformat-region)

;; -----------------------------------------------------------------------------
;; ispell

(setq ispell-dictionary "en")

(advice-add 'ispell-lookup-words :around 'doom-shut-up-a)

;; -----------------------------------------------------------------------------
;; json-mode

(defun +format--buffer-maybe-json (orig)
  (if (eq major-mode 'json-mode)
      (json-pretty-print-buffer)
    (funcall orig)))

(after! json
  (advice-add '+format--buffer :around '+format--buffer-maybe-json))

;; -----------------------------------------------------------------------------
;; lsp

(setq lsp-file-watch-threshold 5000
      lsp-lens-enable nil
      lsp-rust-all-features t
      lsp-signature-auto-activate nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil
      lsp-use-plists t)

;; -----------------------------------------------------------------------------
;; lsp-nix

(use-package lsp-mode)

(use-package lsp-nix
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred))

;; -----------------------------------------------------------------------------
;; magit

(map! "s-g" 'magit-status
      "s-G" 'magit-status-here)

(map! :map magit-status-mode-map
      :n "yn" 'magit-copy-buffer-name)

(defun magit-copy-buffer-name ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is no current branch"))))

;; -----------------------------------------------------------------------------
;; markdown-mode

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :prefix ("t" . "table")
        :desc "align" "a" 'markdown-table-align))

;; -----------------------------------------------------------------------------
;; minibuffer

(map! :map minibuffer-mode-map
      "C-u" 'universal-argument)

;; -----------------------------------------------------------------------------
;; org

(setq org-agenda-files '("~/Documents/org/")
      org-directory "~/Documents/org/"
      org-log-into-drawer t)

(map! "s-§" 'org-agenda-list
      "M-§" 'org-capture-todo)

(defun org-capture-todo ()
  (interactive)
  (org-capture nil "t"))

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

(setq projectile-project-search-path '("~/Projects" "~/Projects/anixe")
      projectile-track-known-projects-automatically nil
      projectile-verbose nil)

(map! :leader
      :prefix "p"
      "P" 'projectile-switch-open-project)

(after! projectile
  (setq projectile-switch-project-action #'projectile-commander))

;; -----------------------------------------------------------------------------
;; rainbow-delimeters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; subword-mode

(map! "C-x s" 'subword-mode
      "C-x S" 'global-subword-mode)

;; -----------------------------------------------------------------------------
;; undo-tree

(setq undo-tree-visualizer-timestamps t)

;; -----------------------------------------------------------------------------
;; vertico

(after! vertico
  (map! :map vertico-map
        "DEL" #'backward-delete-char
        "C-DEL" #'vertico-directory-delete-char))

;; -----------------------------------------------------------------------------
;; vlf

(require 'vlf-setup)

;; -----------------------------------------------------------------------------
;; xml-mode

(defun +format--buffer-maybe-xml (orig)
  (if (eq major-mode 'xml-mode)
      (save-excursion
        (shell-command-on-region (mark) (point) "xmllint --encode utf-8 --format -" (buffer-name) t))
    (funcall orig)))

(advice-add '+format--buffer :around '+format--buffer-maybe-xml)
