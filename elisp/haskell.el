(use-package haskell-mode
  :config
  ;; ensure run-haskell uses the simplest ghci subprocess
  (setq-default haskell-process-type 'ghci)
  (add-hook 'haskell-mode-hook 'ormolu-format-on-save-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  ;; configure interactive mode
  (require 'haskell-interactive-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)

  ;; add custom
  (setq-default
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-log t)
  )
(use-package ormolu)

(use-package lsp-haskell
  :requires lsp)
