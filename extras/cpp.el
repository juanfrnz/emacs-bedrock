(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode) .
    ("clangd" :initializationOptions
     (:hints (:parameterNames t
              :rangeVariableTypes t
              :functionTypeParameters t
              :assignVariableTypes t
              :compositeLiteralFields t
              :compositeLiteralTypes t
              :constantValues t)))))

