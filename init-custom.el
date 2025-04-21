;; Enable:
;;  mouse-mode
;;  C-x p f -> fuzzy find

(xterm-mouse-mode)
(setq scroll-step 1)       ;; Scroll one line at a time
(setq scroll-conservatively 101) ;; Avoid recentering the cursor while scrolling
(setq scroll-margin 0)     ;; Do not keep a margin at the top/bottom
(setq scroll-preserve-screen-position 'always) ;; Keep cursor position while scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; Scroll one line per mouse wheel tick
(setq mouse-wheel-progressive-speed nil) ;; Disable speedup when scrolling fast
(setq select-enable-clipboard t)
(setq select-enable-primary t)
;; (when (and (not (display-graphic-p)) (eq system-type 'darwin))
;;   (defun copy-to-clipboard ()
;;     (interactive)
;;     (if (region-active-p)
;;         (progn
;;           (shell-command-on-region (region-beginning) (region-end) "pbcopy")
;;           (deactivate-mark))
;;       (message "No region active; can't copy!")))

;;   (defun paste-from-clipboard ()
;;     (interactive)
;;     (insert (shell-command-to-string "pbpaste")))

;;   (global-set-key (kbd "M-w") 'copy-to-clipboard)
;;   (global-set-key (kbd "C-y") 'paste-from-clipboard))

;; Auto completion example
(use-package corfu
  :custom
  (corfu-auto t)          ;; Enable auto completion
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :bind
  ;; Another key binding can be used, such as S-SPC.
  ;; (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

;; (use-package corfu
;;   :custom
;;   ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
;;   :bind
;;   ;; Configure SPC for separator insertion
;;   (:map corfu-map ("SPC" . corfu-insert-separator))
;;   :init
;;   (global-corfu-mode))

(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package nerd-icons-corfu
  :ensure t)

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(setq nerd-icons-corfu-mapping
      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        ;; You can alternatively specify a function to perform the mapping,
        ;; use this when knowing the exact completion candidate is important.
        (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
        ;; ...
        (t :style "cod" :icon "code" :face font-lock-warning-face)))
        ;; Remember to add an entry for `t', the library uses that as default.


(use-package catppuccin-theme
  :ensure t
  :init (setq catppuccin-flavor 'mocha)
  :hook (after-init . (lambda () (load-theme 'catppuccin))))
;; (use-package emacs
;;   :config
;;   (load-theme 'modus-vivendi-tinted))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer))) ;; More powerful buffer switcher

(dolist (mode '(c++-mode-hook
                emacs-lisp-mode-hook
                js-ts-mode-hook
                typescript-ts-mode-hook
                rust-ts-mode-hook
                go-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook mode #'eglot-ensure))

(use-package prettier
  :ensure t
  :hook (typescript-ts-mode . prettier-mode) ; Enable Prettier for specific modes
  :config
  (setq prettier-mode-sync-config-flag t)) ; Sync Prettier config with buffer

(defun copilot-keybindings ()
  (local-set-key (kbd "M-q") 'copilot-accept-completion-by-paragraph))

(add-hook 'copilot-mode-hook 'copilot-keybindings)
