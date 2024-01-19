(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package magit
  :commands (magit-status)
  :bind ("C-x g"   . magit-status)
  :config
  (magit-auto-revert-mode)
  (setq magit-push-always-verify nil))

;; (use-package forge
;;   :after magit)

;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode +1)
;;   (setq git-gutter:modified-sign " ")
;;   (setq git-gutter:added-sign " ")
;;   (setq git-gutter:deleted-sign " "))
