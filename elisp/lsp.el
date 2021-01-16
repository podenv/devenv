(use-package lsp-mode
  :config
  ;; disable fancy features
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-lens-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  )

(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode-hook . lsp-ui-mode)))

(use-package lsp-ivy)
