;; When editing .purs file, checkout the psc- commands, like:
;; 'psc-ide-server-start'. Then hit C-c C-b,
;; Use: 'psc-ide-flycheck-insert-suggestion'
(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (format-all-mode)
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (flymake-mode -1)
              (turn-on-purescript-indentation)))
  (add-hook 'purescript-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'purescript-sort-imports nil t)))
  )
(use-package psci)
