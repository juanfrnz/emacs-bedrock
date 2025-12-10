;;; stl-mode.el --- Major mode for editing STL interface definition files -*- lexical-binding: t; -*-
;; Major mode for editing Sen Query Language files.

(defvar stl-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Punctuation
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?[ "." table)
    (modify-syntax-entry ?] "." table)                         
    table)
  "Syntax table for `stl-mode'.")

(defvar stl-font-lock-keywords
  (let* ((stl-keywords
          '("package" "import" "struct" "class" "variant" "enum" "optional" "array" "sequence" "const" "static" "fn"))
         (stl-types
          '("bool" "u8" "u16" "u32" "u64" "i8" "i16" "i32" "i64"
            "f32" "f64" "string" "Duration" "TimeStamp"))
         (stl-constants
          '("true" "false"))

         (stl-keywords-regexp (regexp-opt stl-keywords 'words))
         (stl-types-regexp (regexp-opt stl-types 'words))
         (stl-constants-regexp (regexp-opt stl-constants 'words)))

    `((,stl-keywords-regexp . font-lock-keyword-face)
      (,stl-types-regexp . font-lock-type-face)
      (,stl-constants-regexp . font-lock-constant-face)
      ;; Field names
      ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*:" (1 font-lock-variable-name-face))
      ))
  "Font lock keywords for `stl-mode'.")

(defun stl-indent-line ()
  "Indent current line for STL mode."
  (interactive)
  (let (indent)
    (save-excursion
      (beginning-of-line)

      (setq indent
            (cond
             ((looking-at "^[ \t]*}")
              (condition-case nil
                  (progn (forward-char) (backward-list)
                         (current-indentation))
                (error 0)))

             (t
              (condition-case nil
                  (save-excursion
                    (backward-up-list)
                    (+ (current-indentation) 2))
                (error (save-excursion
                         (forward-line -1)
                         (while (and (not (bobp))
                                     (looking-at "^[ \t]*$"))
                           (forward-line -1))
                         (current-indentation)))))))

      (if (looking-at "^[ \t]+")
          (replace-match ""))
      (indent-to indent))))

(defvar stl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    map)
  "Keymap for `stl-mode'.")

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

;;;autoload
(add-to-list 'auto-mode-alist '("\\.stl\\'" . stl-mode))

(provide 'stl-mode)

;;; stl-mode.el ends here
