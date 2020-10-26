(use-package python
  :config
  (setq-default python-shell-interpreter "python3"))

(use-package flycheck-mypy
  :config
  (add-to-list 'flycheck-checkers 'python-mypy t))
