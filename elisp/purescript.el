(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (add-hook 'purescript-mode-hook
    (lambda ()
      (format-all-mode)
      (psc-ide-mode)
      (company-mode)
      (turn-on-purescript-indentation)
)))
(use-package psci)
