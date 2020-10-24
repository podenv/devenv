(use-package haskell-mode
  :config
  ;; ensure run-haskell uses the simplest ghci subprocess
  (setq-default haskell-process-type 'ghci)
  (add-hook 'haskell-mode-hook 'ormolu-format-on-save-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template))
(use-package ormolu)
