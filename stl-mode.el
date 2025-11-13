;;; stl-mode.el --- Major mode for editing STL interface definition files -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Juan Fernándenz
;; Version: 0.1.0
;; Keywords: stl sen

;;; Commentary:

;; Major mode for editing Sen Template Language files.

;;; Code:

(defvar stl-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Operators
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?, "." table)
    table)
  "Syntax table for `stl-mode'.")

(defvar stl-font-lock-keywords
  (let* ((stl-keywords
          '("package" "import" "struct" "variant" "enum" "optional" "array" "sequence"))
         (stl-types
          '("bool" "u8" "u16" "u32" "u64" "i8" "i16" "i32" "i64"
            "f32" "f64" "string" "Duration"))
         (stl-constants
          '("true" "false"))

         (stl-keywords-regexp (regexp-opt stl-keywords 'words))
         (stl-types-regexp (regexp-opt stl-types 'words))
         (stl-constants-regexp (regexp-opt stl-constants 'words)))

    `((,stl-keywords-regexp . font-lock-keyword-face)
      (,stl-types-regexp . font-lock-type-face)
      (,stl-constants-regexp . font-lock-constant-face)
      ;; Package and type names (qualified names with dots)
      ("\\([a-zA-Z_][a-zA-Z0-9_]*\\.\\)+[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-type-face)
      ;; Type definitions (after struct, enum, variant)
      ("\\(struct\\|enum\\|variant\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (2 font-lock-function-name-face))
      ;; Field names (before colon)
      ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*:" (1 font-lock-variable-name-face))
      ;; Numbers
      ("[^a-zA-Z_]\\([0-9]+\\)" (1 font-lock-constant-face))
      ;; Strings
      ("\"[^\"]*\"" . font-lock-string-face)))
  "Font lock keywords for `stl-mode'.")

(defun stl-indent-line ()
  "Indent current line for STL mode."
  (interactive)
  (let ((indent-col 0)
        (cur-indent 0))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq cur-indent 0)
        ;; Check if we're closing a block
        (if (looking-at "^[ \t]*}")
            (progn
              ;; Find matching opening brace
              (condition-case nil
                  (progn
                    (forward-char)
                    (backward-list)
                    (setq cur-indent (current-indentation)))
                (error (setq cur-indent 0))))
          ;; Not a closing brace
          (progn
            ;; Look at previous non-empty line
            (let ((prev-indent 0)
                  (found-prev nil))
              (save-excursion
                (forward-line -1)
                (while (and (not (bobp)) (not found-prev))
                  (if (not (looking-at "^[ \t]*$"))
                      (progn
                        (setq prev-indent (current-indentation))
                        (setq found-prev t))
                    (forward-line -1))))

              ;; Check if previous line opens a block
              (save-excursion
                (forward-line -1)
                (end-of-line)
                (skip-chars-backward " \t")
                (if (and found-prev (eq (char-before) ?{))
                    (setq cur-indent (+ prev-indent 2))
                  ;; Check if previous line is inside a block
                  (condition-case nil
                      (progn
                        (backward-up-list)
                        (setq cur-indent (+ (current-indentation) 2)))
                    (error (setq cur-indent prev-indent))))))))))

    ;; Apply the indentation
    (if (looking-at "^[ \t]+")
        (replace-match ""))
    (indent-to cur-indent)))

(defvar stl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    map)
  "Keymap for `stl-mode'.")

;;;###autoload
(define-derived-mode stl-mode prog-mode "STL"
  "Major mode for editing STL interface definition files.

\\{stl-mode-map}"
  :syntax-table stl-mode-syntax-table

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Font lock
  (setq-local font-lock-defaults '(stl-font-lock-keywords))

  ;; Indentation
  (setq-local indent-line-function 'stl-indent-line)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stl\\'" . stl-mode))

(provide 'stl-mode)

;;; stl-mode.el ends here
