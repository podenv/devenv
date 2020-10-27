(use-package python
  :config
  (add-hook 'python-mode-hook 'format-all-mode)
  (setq-default python-shell-interpreter "python3"))

(use-package flycheck-mypy
  :config
  (add-to-list 'flycheck-checkers 'python-mypy t))
