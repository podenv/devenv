(use-package lsp-mode)

(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode-hook . lsp-ui-mode)))

(use-package lsp-ivy)
