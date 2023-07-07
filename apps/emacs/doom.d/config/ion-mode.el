;;; ion-mode.el --- A major-mode for editing ION contracts -*-lexical-binding: t-*-

(require 'evil-lion)

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
  "Syntax table for `ion-mode'.")

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

(defun ion-reformat-region (beg end)
  "Reformat the ION in the specified region."
  (interactive "*r")
  (let ((interesting-line-regex ".*|.*"))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)

        ;;; Step 1/5: For each line, remove its beginning & ending `|`
        ;;;           (otherwise evil-lion will format the output a bit worse)
        (goto-char (point-min))

        (while (not (eobp))
          (when (looking-at-p interesting-line-regex)
            (skip-chars-forward " ")
            (when (= ?| (char-after))
              (delete-char 1))
            (while (= ?\s (char-after))
              (delete-char 1))
            (end-of-line)
            (when (= ?| (char-before))
              (delete-char -1)))

          (forward-line))

        ;;; Step 2/5: For each line, ensure each `|` is separated with a space
        (goto-char (point-min))

        (while (not (eobp))
          (when (looking-at-p interesting-line-regex)
            (save-excursion
              (save-restriction
                (narrow-to-region (line-beginning-position) (line-end-position))

                (while (not (eobp))
                  (skip-chars-forward "^|")
                  (unless (eobp)
                    (unless (= ?\s (char-before))
                      (insert " "))
                    (goto-char (1+ (point)))
                    (unless (= ?\s (following-char))
                      (insert " "))))

                ;;; Ensure there are no leftover whitespaces at the end
                (while (= ?\s (char-before))
                  (delete-char -1)))))

          (forward-line))

        ;;; Step 3/5: For each line, restore its ending `|`
        (goto-char (point-min))

        (while (not (eobp))
          (when (looking-at-p interesting-line-regex)
            (end-of-line)
            (insert " |"))

          (forward-line))

        ;;; Step 4/5: Reformat!
        (evil-lion--align (point-min) (point-max) 0 'left ?|)

        ;;; Step 5/5: For each line, restore its beginning `|`
        (goto-char (point-min))

        (while (not (eobp))
          (when (looking-at-p interesting-line-regex)
            (skip-chars-forward " ")
            (insert "| "))

          (forward-line))))))

;;;

(provide 'ion-mode)
