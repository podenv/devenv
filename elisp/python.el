(use-package python
  :config
  (add-hook 'python-mode-hook 'format-all-mode)
  (setq-default python-shell-interpreter "python3"))
