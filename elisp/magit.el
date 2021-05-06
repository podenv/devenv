(use-package magit
  :commands (magit-status)
  :bind ("C-x g"   . magit-status)
  :config
  (magit-auto-revert-mode)
  (setq magit-push-always-verify nil))

(use-package forge
  :after magit)
