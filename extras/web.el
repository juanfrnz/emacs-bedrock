;; (use-package format-all
;;   :ensure t
;;   :hook ((typescript-mode . format-all-mode)
;;          (typescript-ts-mode . format-all-mode)
;;          (tsx-ts-mode . format-all-mode)
;;          (js-mode . format-all-mode)
;;          (js-ts-mode . format-all-mode)
;;          (css-mode . format-all-mode)
;;          (css-ts-mode . format-all-mode)
;;          (json-mode . format-all-mode)
;;          (web-mode . format-all-mode)
;;          (format-all-mode . format-all-ensure-formatter)))

;; (add-hook 'before-save-hook #'format-all-buffer)

(use-package prettier
  :ensure t
  :hook ((typescript-mode . prettier-mode)
         (typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)
         (js-mode . prettier-mode)
         (js-ts-mode . prettier-mode)
         (css-mode . prettier-mode)
         (json-mode . prettier-mode)
         (web-mode . prettier-mode)))

(package-install 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))

