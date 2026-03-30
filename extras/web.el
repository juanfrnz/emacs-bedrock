(package-install 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))

(define-minor-mode biome-format-on-save-mode
  "Minor mode to format buffer with biome on save."
  :lighter " Biome"
  (if biome-format-on-save-mode
      (add-hook 'after-save-hook #'biome-format-buffer nil t)
    (remove-hook 'after-save-hook #'biome-format-buffer t)))

(dolist (hook '(astro-ts-mode-hook
                web-mode
                html-mode-hook
                mhtml-mode-hook
                css-mode-hook
                css-ts-mode-hook
                json-mode-hook
                json-ts-mode-hook
                js-mode-hook
                js-ts-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook hook #'biome-format-on-save-mode))

(defun biome-format-buffer ()
  "Format the current buffer using biome."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (let ((output (shell-command-to-string
                     (format "bunx biome format --write %s" (shell-quote-argument file)))))
        (revert-buffer t t t)))))
