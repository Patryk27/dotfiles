;;; ion-mode.el --- A major-mode for editing ION contracts -*-lexical-binding: t-*-

(defgroup ion-mode nil
  "Support for ION contracts."
  :group 'languages)

;;;

(defface ion-header
  '((t :inherit font-lock-type-face))
  "Face for section and table headers."
  :group 'ion-mode)

(defface ion-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for keywords."
  :group 'ion-mode)

(defface ion-number
  '((t :inherit font-lock-constant-face))
  "Face for numbers."
  :group 'ion-mode)

(defface ion-section-key
  '((t :inherit font-lock-variable-name-face))
  "Face for section keys."
  :group 'ion-mode)

(defface ion-table-layout
  '((t :foreground "gray40"))
  "Face for table layouts."
  :group 'ion-mode)

;;;

(defvar ion-keywords '("true" "false"))

(defvar ion-keywords-regexp (regexp-opt ion-keywords 'words))

(defvar ion-font-lock-keywords
  `(
    (,"^\\[\\([a-zA-Z0-9_\.]+\\)\\]" (1 'ion-header))
    (,"^\\([a-zA-Z_]+\\)" . 'ion-section-key)
    (,"\\(?:+\\|\-\\|%\\)*[0-9]+\\(?:\\.[0-9]\\)?" . 'ion-number)
    (,"\\(|\\|\-\\)" . 'ion-table-layout)
    (,ion-keywords-regexp . 'ion-keyword)))

(defvar ion-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for 'ion-mode'.")

;;;

(define-derived-mode ion-mode prog-mode "ION"
  "Major mode for editing ION contracts."
  :group 'ion-mode
  (setq-local comment-start "#")
  (setq-local font-lock-defaults `(ion-font-lock-keywords))
  (setq-local imenu-generic-expression
              '(("Section" "^\\[\\([a-zA-Z0-9_\.]+\\)\\]" 1))))

(add-to-list 'auto-mode-alist (cons "\\.ion\\'" 'ion-mode))

;;;

(defun ion-mode--disable-minor-modes ()
  (when (fboundp 'highlight-numbers-mode)
    (highlight-numbers-mode 0))

  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode 0)))

(add-hook 'ion-mode-hook 'ion-mode--disable-minor-modes)

;;;

(provide 'ion-mode)
