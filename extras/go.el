;; Force use of go-mode instead of go-ts-mode to avoid issues with electric mode
(add-to-list 'major-mode-remap-alist '(go-ts-mode . go-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package go-mode
  :ensure t
  :hook ((go-mode . my-go-mode-setup)))

(defun my-go-mode-setup ()
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t)
  ;; Keybinding for finding implementations (gopls implements)
  (local-set-key (kbd "C-c C-i") 'eglot-find-implementation))

(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure))
