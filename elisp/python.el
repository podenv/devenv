(use-package python)

(use-package flycheck-mypy
  :config
  (add-to-list 'flycheck-checkers 'python-mypy t))
