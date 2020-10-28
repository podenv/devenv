(use-package lsp-mode)

(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode-hook . lsp-ui-mode)))

(use-package lsp-ivy)

;; company-lsp simplifies completion-at-point
(use-package company-lsp
  :ensure t
  :after company
  :init
  (push 'company-lsp company-backends))
