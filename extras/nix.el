(use-package nix-mode
  :ensure t
  :hook ((nix-mode nix-ts-mode) . my-nix-mode-setup))

(defun my-nix-mode-setup ()
  ;; Start nixd so completions come from the language server
  (eglot-ensure)
  ;; Show the completion popup automatically while typing
  (setq-local corfu-auto t
              corfu-auto-delay 0.2
              corfu-auto-prefix 2)
  ;; Format on save (nixd uses nixfmt for this)
  (add-hook 'before-save-hook
            (lambda () (when (eglot-managed-p) (eglot-format-buffer)))
            nil t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nixd")))
  ;; There are no channels, so <nixpkgs> doesn't resolve; use the flake registry
  ;; so nixd can suggest pkgs.* and lib.* attributes
  (setq-default eglot-workspace-configuration
                '(:nixd (:nixpkgs (:expr "import (builtins.getFlake \"nixpkgs\") { }")))))
