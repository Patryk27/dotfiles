;;; -*- lexical-binding: t; -*-

(load "%llvm-mode%")

(when (eq system-type 'gnu/linux)
  (toggle-frame-fullscreen))

(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"
        mac-command-modifier 'control))

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

(setq calc-kill-line-numbering nil)

(map! :leader
      :desc "calc" "=" 'calc)

(map! :v "=" 'calc-eval-region)

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
        dirvish-hide-details nil)

  (setq dirvish-quick-access-entries
        '(
          ("d" "~/Downloads")
          ("h" "~")
          ("i" "~/diary")
          ("o" "~/Documents")
          ("t" "~/t")
          ("x" "~/x")
          ("F" "/scp:gateway:/var/lib/nixos-containers/nginx/var/www/files")
          ("K" "/scp:gateway:/var/lib/nixos-containers/kartoffels/var/lib/kartoffels")))

  (map! :map dirvish-mode-map
        :n "?" 'dirvish-dispatch
        :n "a" 'dirvish-quick-access
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
        (error "no more than 2 files should be marked")))))

;; -----------------------------------------------------------------------------
;; doom

(setq doom-font (font-spec
                 :family "Berkeley Mono"
                 :size (if (eq system-type 'gnu/linux) 22 15))
      doom-theme 'doom-gruvbox
      +doom-dashboard-functions '(doom-dashboard-widget-banner))

(map! :leader
      "b a" 'rename-buffer
      "b p" 'copy-buffer-relative-path
      "b P" 'copy-buffer-absolute-path
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

  (unless (file-exists-p "~/diary/mnt/pwy")
    (progn
      (eshell-command
       (format
        "echo %s | gocryptfs ~/diary/src ~/diary/mnt"
        (read-passwd "Password: ")))))

  (evil-normal-state)
  (dired "~/diary/mnt"))

(defun close-diary (&rest _)
  "Close the diary."
  (interactive)

  (if (file-exists-p "~/diary/mnt/pwy")
      (progn
        (eshell-command "umount ~/diary/mnt"))))

;; -----------------------------------------------------------------------------
;; doom-modeline

(after! doom-modeline
  (advice-add 'doom-modeline-segment--vcs :override 'no-op))

;; -----------------------------------------------------------------------------
;; eat

(add-hook 'eshell-load-hook 'eat-eshell-mode)

(add-hook 'eat-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

(map! :map eat-eshell-semi-char-mode-map
      :g "M-RET" 'eat-eshell-char-mode)

(map! :map eat-eshell-char-mode-map
      :g "<escape>" 'eat-self-input)

(defun +eat/evil-setup ()
  (map! :map eat-eshell-char-mode-map
        :g "C-V" 'eat-yank)

  (if (bound-and-true-p eat--eshell-char-mode)
      (progn
        (turn-off-evil-mode)
        (evil-refresh-cursor))
    (turn-on-evil-mode)))

(add-hook 'eat--eshell-char-mode-hook '+eat/evil-setup)

(defun +eat/evil-initialize-a (fn &rest args)
  (unless (bound-and-true-p eat--eshell-char-mode)
    (apply fn args)))

(advice-add 'evil-initialize :around '+eat/evil-initialize-a)

(defun +eat/evil-refresh-cursor-a (fn &rest args)
  (if (bound-and-true-p eat--eshell-char-mode)
      (progn
        (set-cursor-color "#ffffff")
        (setq cursor-type 'box)))
  (apply fn args))

(advice-add 'evil-refresh-cursor :around '+eat/evil-refresh-cursor-a)

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
;; eshell

(advice-add 'eshell-did-you-mean-setup :override 'no-op)
(advice-add 'setup-esh-help-eldoc :override 'no-op)

(add-hook 'eshell-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

(add-hook 'eshell-post-command-hook 'evil-insert-state)

(map! :leader
      "d" '+eshell/toggle
      "o s" '+eshell/here)

(map! :map eshell-mode-map
      :n "RET" '+eshell/goto-end-of-prompt
      :n "p" '+eshell/ctrl-v)

(map! :map eshell-mode-map
      :ni "C-a" '+eshell/ctrl-a
      :ni "C-c C-c" '+eshell/ctrl-c
      :ni "C-r" '+eshell/ctrl-r
      :ni "C-v" '+eshell/ctrl-v
      :ni "C-w" '+eshell/ctrl-w)

(defun +eshell/ctrl-a ()
  (interactive)
  (if eat-terminal
      (eat-self-input 1 1)
    (goto-char eshell-last-output-end)
    (beginning-of-line)))

(defun +eshell/ctrl-c ()
  (interactive)
  (if eat-terminal
      (eat-self-input 1 3)
    (eshell-interrupt-process)))

(defun +eshell/ctrl-r ()
  (interactive)
  (if eat-terminal
      (eat-self-input 1 18)
    (consult-history)))

(defun +eshell/ctrl-v ()
  (interactive)
  (if eat-terminal
      (eat-yank)
    (yank)))

(defun +eshell/ctrl-w ()
  (interactive)
  (if eat-terminal
      (eat-self-input 1 23)
    (evil-delete-backward-word)))

;; ---

(setq eshell-save-history-on-exit nil)

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((ring (make-ring 1)))
      (ring-insert ring (car (ring-elements eshell-history-ring)))

      (setq eshell-history-ring--prev (ring-copy eshell-history-ring)
            eshell-history-ring ring
            eshell-hist--new-items 1)

      (eshell-write-history eshell-history-file-name t)

      (setq eshell-history-ring eshell-history-ring--prev
            eshell-history-ring--prev nil))))

(add-hook 'eshell-pre-command-hook 'eshell-append-history)

;; ---

(defun eshell/bcat (&rest args)
  (let ((buffers (mapcar 'get-buffer args)))
    (mapconcat (lambda (buf)
                 (save-window-excursion
                   (switch-to-buffer buf)
                   (buffer-substring-no-properties (point-min) (point-max))))
               buffers "\n")))

(defun eshell/ccat (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))

;; ---

(after! eshell
  (push "nix" eshell-complex-commands))

(defun eshell/nix (&rest args)
  (let ((cmd (car args)))
    (if (or (string= "develop" cmd) (string= "shell" cmd))
        (progn (eshell-nix args) nil)
      (throw 'eshell-replace-command
             (eshell-parse-command
              (concat (char-to-string eshell-explicit-command-char) "nix") args)))))

(defun eshell-nix (args)
  (make-local-variable 'process-environment)
  (eshell-nix-leave)
  (eshell-nix-enter (eshell-nix-eval args)))

(defun eshell-nix-enter (env)
  (setq-local eshell-nix-prev-env process-environment
              process-environment env)

  (eshell-nix-refresh-env))

(defun eshell-nix-eval (args)
  (with-temp-buffer
    (let ((call-process-args (list "nix" nil (current-buffer) nil)))
      (nconc call-process-args args)
      (nconc call-process-args '("--command" "sh" "-c" "export"))

      (let ((exit-code (apply 'call-process call-process-args)))
        (when (not (= 0 exit-code))
          (error (buffer-string)))

        (let ((env '())
              (env-regex
               (rx "export "
                   (group (one-or-more (or alpha ?_)))
                   "=\""
                   (group (zero-or-more (not "\""))))))
          (save-match-data
            (goto-char (point-min))
            (while (search-forward-regexp env-regex nil t 1)
              (let ((env-name (match-string 1))
                    (env-value (match-string 2)))
                (setq env (setenv-internal env env-name env-value nil)))))
          env)))))

(defun eshell-nix-leave ()
  (when (boundp 'eshell-nix-prev-env)
    (setq-local process-environment eshell-nix-prev-env)
    (makunbound 'eshell-nix-prev-env)
    (eshell-nix-refresh-env)))

(defun eshell-nix-leave-a (fn &rest args)
  (if (boundp 'eshell-nix-prev-env)
      (eshell-nix-leave)
    (apply fn args)))

(advice-add 'eshell/exit :around 'eshell-nix-leave-a)

(defun eshell-nix-refresh-env ()
  (eshell-set-path (getenv "PATH"))
  nil)

;; ---

(after! eshell
  (defvar +eshell--id nil)

  (setq eshell-bad-command-tolerance 999
        eshell-banner-message ""
        eshell-buffer-maximum-lines 32768
        eshell-history-size 32768
        eshell-prompt-function '+eshell/prompt
        eshell-prompt-regexp "; ")

  (add-to-list 'eshell-modules-list 'eshell-elecslash)

  (set-eshell-alias!
   ;; docker
   "d" "docker $*"
   "dc" "docker-compose $*"

   ;; eshell
   "cdp" "eshell/cd (projectile-project-root)"
   "cds" "eshell/cd /scp:$1:/"

   ;; rust
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

   ;; ssh
   "st" "ssh $1 -t tmux"
   "sta" "ssh $1 -t tmux a"
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
            (if (boundp 'eshell-nix-prev-env) " | nix" "")
            "\n"
            (propertize ";" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  (defun +eshell--unused-buffer (&optional new-p)
    (or (unless new-p
          (cl-loop for buf in (+eshell-buffers)
                   if (and (buffer-live-p buf)
                           (not (get-buffer-window buf t))
                           (not (with-current-buffer buf +eshell--id)))
                   return buf))
        (generate-new-buffer eshell-buffer-name))))

;; -----------------------------------------------------------------------------
;; evil

(setq evil-want-fine-undo t
      evil-normal-state-cursor '(box "#00ff00")
      evil-insert-state-cursor '(bar "#00ff00")
      evil-visual-state-cursor '(hollow "#00ff00")
      evil-replace-state-cursor '(hbar "#00ff00"))

(defun +lookup/parent ()
  (interactive)
  (cond
   ((eq major-mode 'nix-mode) (+lookup/parent-naive "nix"))
   ((eq major-mode 'rustic-mode) (lsp-rust-find-parent-module))
   (t (error "don't know how to lookup the parent here"))))

(defun +lookup/parent-naive (ext)
  (find-file-existing
   (format
    "%s.%s"
    (directory-file-name (file-name-directory buffer-file-name))
    ext)))

(map! :ni "C-<tab>" 'evil-write-all
      :nv "C-e" 'evil-scroll-up
      :nv "C-u" 'evil-scroll-line-down
      :ni "C-<left>" 'evil-beginning-of-line
      :ni "C-<right>" 'evil-end-of-line
      :n "z;" 'sort-lines
      :n "ga" '+lookup/references
      :n "gD" nil
      :n "gt" '+lookup/type-definition
      :n "gp" '+lookup/parent)

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
;; ibuffer

(after! ibuffer
  (define-key ibuffer-mode-map [remap ibuffer-visit-buffer] nil)
  (define-key ibuffer-mode-map (kbd "M-RET") '+ibuffer/visit-workspace-buffer))

;; -----------------------------------------------------------------------------
;; indent-bars

(use-package indent-bars
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-width-frac 0.12)
  :hook ((emacs-lisp-mode
          rustic-mode
          typescript-mode
          web-mode)
         . indent-bars-mode))

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

(setq lsp-lens-enable nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil)

;; -----------------------------------------------------------------------------
;; lsp-nix

(after! lsp-mode
  (use-package lsp-nix
    :demand t
    :custom
    (lsp-nix-nil-formatter ["nixfmt"]))

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
      org-log-into-drawer t)

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

(advice-add 'projectile-project-root :before-while
            (lambda (&optional dir)
              (not (file-remote-p (or dir default-directory)))))

(setq projectile-track-known-projects-automatically nil
      projectile-verbose nil)

(when (eq system-type 'gnu/linux)
  (setq projectile-project-search-path
        '("~/t/" "~/x/" "~/.emacs.d")))

(when (eq system-type 'darwin)
  (setq projectile-project-search-path
        '("~/x/" "~/.emacs.d")))

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

  (defun rustic-open-cargo-toml ()
    "Open closest `Cargo.toml'"
    (interactive)
    (let ((file (locate-dominating-file "." "Cargo.toml")))
      (when file
        (find-file (concat file "Cargo.toml")))))

  ;; TODO https://github.com/brotzeit/rustic/issues/450
  (defun rustic-save-some-buffers-advice (orig-fun &rest args)
    (apply orig-fun args))

  (map! :map rustic-mode-map
        :localleader
        "b" nil
        "r" 'rustic-rerun-shell-command
        "s" 'rustic-run-shell-command
        "t" nil)

  (map! :map rustic-mode-map
        :localleader
        :prefix ("o" . "open")
        :desc "main.rs" "m" 'rustic-open-main-rs
        :desc "lib.rs" "l" 'rustic-open-lib-rs
        :desc "Cargo.toml" "c" 'rustic-open-cargo-toml)

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

(require 'ispell)
(require 'spell-fu)

(setq ispell-dictionary "en")

(spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
(spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "pl"))
(spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "sv"))

;; -----------------------------------------------------------------------------
;; subword-mode

(map! "<f1>" 'subword-mode
      "C-<f1>" 'global-subword-mode)

;; -----------------------------------------------------------------------------
;; tramp

(after! tramp
  (add-to-list 'tramp-methods
               `("fudo"
                 (tramp-login-program     "fj")
                 (tramp-login-args        (("console") ("%h")))
                 (tramp-remote-shell      "/bin/sh")
                 (tramp-remote-shell-args ("-i" "-c")))))

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
