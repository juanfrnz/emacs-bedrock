(use-package prettier-js
  :ensure t
  :hook ((typescript-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (js-ts-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (css-ts-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (json-ts-mode . prettier-js-mode)
         (html-mode . prettier-js-mode)
         (mhtml-mode . prettier-js-mode)
         (astro-ts-mode . prettier-js-mode)
         (markdown-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

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
                astro-mode-hook
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

;; Lit web components: use lit-ts-mode (html`...`/css`...` template
;; highlighting + class/id completion) for .ts/.js files that import Lit.
(use-package lit-ts-mode
  :ensure t
  :init
  (defun my/lit-component-file-p ()
    "Return non-nil if the current buffer is a JS/TS file importing Lit."
    (and buffer-file-name
         (string-match-p "\\.[cm]?[jt]s\\'" buffer-file-name)
         (save-excursion
           (goto-char (point-min))
           (re-search-forward
            "from[ \t]+['\"]\\(lit\\|lit-element\\|lit-html\\|@lit/[^'\"]+\\)\\(/[^'\"]*\\)?['\"]"
            magic-mode-regexp-match-limit t))))

  (defun my/lit-mode ()
    "Enable the Lit major mode matching the file extension."
    (if (string-match-p "\\.[cm]?ts\\'" buffer-file-name)
        (lit-ts-typescript-mode)
      (lit-ts-js-mode)))

  ;; magic-mode-alist is consulted before auto-mode-alist, so Lit files
  ;; get the Lit modes while other .ts/.js files keep their usual modes.
  (add-to-list 'magic-mode-alist '(my/lit-component-file-p . my/lit-mode))
  :config
  (unless (treesit-language-available-p 'lit-html)
    (treesit-install-language-grammar 'lit-html)))
