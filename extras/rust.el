(add-hook 'rust-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer)))

