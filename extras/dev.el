;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
;; magit-status lives in a dedicated right-hand side window (like treemacs on
;; the left); everything it opens (diffs, logs, commits, files) goes to the
;; main editing area instead of replacing the sidebar.
(defvar my/magit-side-width 50
  "Width of the magit-status side window.")

(defun my/magit-display-buffer (buffer)
  "Show magit-status in the right side window, other magit buffers elsewhere."
  (if (eq (buffer-local-value 'major-mode buffer) 'magit-status-mode)
      (display-buffer buffer
                      `(display-buffer-in-side-window
                        (side . right) (slot . 0)
                        (window-width . ,my/magit-side-width)
                        (preserve-size . (t . nil))
                        (dedicated . t)
                        (window-parameters . ((no-delete-other-windows . t)
                                              (no-other-window . nil)))))
    (display-buffer buffer
                    '((display-buffer-reuse-window
                       display-buffer-use-some-window)
                      (inhibit-same-window . t)))))

(defun my/magit-sidebar-visit-file ()
  "Visit file at point; from the sidebar, open it in the main window."
  (interactive)
  (if (window-parameter nil 'window-side)
      (magit-diff-visit-file-other-window)
    (magit-diff-visit-file)))

(defun my/magit-sidebar-toggle ()
  "Toggle the magit-status sidebar for the current repository."
  (interactive)
  (let ((win (seq-find (lambda (w)
                         (with-current-buffer (window-buffer w)
                           (derived-mode-p 'magit-status-mode)))
                       (window-list))))
    (if win
        (delete-window win)
      (magit-status))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("<f9>"  . my/magit-sidebar-toggle))
  :config
  (setq magit-display-buffer-function #'my/magit-display-buffer
        magit-bury-buffer-function #'magit-restore-window-configuration)
  (define-key magit-status-mode-map [remap magit-diff-visit-file]
              #'my/magit-sidebar-visit-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )
(use-package eglot
  :ensure t
  :config
  (setq eglot-autoshutdown t
        eldoc-echo-area-use-multiline-p t)
  (define-key eglot-mode-map (kbd "C-c i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c d") #'eglot-find-definition)
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-find-references))

(use-package eldoc-box
  :ensure t
  :config
  (setq eldoc-idle-delay 0.1))

(use-package flymake
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short) ; show errors inline while editing
  :hook (flymake-mode . (lambda () (setq-local truncate-lines t)))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; Show Flymake diagnostics alongside Eglot hover docs instead of hover hiding them
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local eldoc-documentation-strategy
                        #'eldoc-documentation-compose-eagerly)))

(load-file (expand-file-name "stl-mode.el" user-emacs-directory))

(use-package breadcrumb
  :ensure t
  :config
  (setq breadcrumb-imenu-max-length 50)
  (breadcrumb-mode 1))

(use-package symbols-outline
  :ensure t
  :bind ("C-c o" . symbols-outline-show)
  :init
  (add-hook 'eglot-managed-mode-hook'
            (lambda ()
              (setq-local symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)))
  :config
  (setq symbols-outline-window-position 'left)
  (symbols-outline-follow-mode))

;; Dart / Flutter
(use-package dart-mode
  :ensure t
  :hook (dart-mode . eglot-ensure))

(add-to-list 'eglot-server-programs
             '(dart-mode . ("dart" "language-server"
                            "--client-id" "emacs.eglot-dart"
                            "--protocol" "lsp")))
(use-package diff-hl
  :ensure t
  )
(global-diff-hl-mode)
(global-diff-hl-show-hunk-mouse-mode)
