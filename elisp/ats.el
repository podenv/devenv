;; This is a bit odd because ats-mode is provided in an extra derivation and the filename is ats2-mode...
(setq ats-mode-load-path (getenv "ATS_LOADPATH"))
(use-package ats2-mode
  :load-path ats-mode-load-path
  :no-require t
  :config
  (require 'ats-mode "ats2-mode.el")
  (add-hook 'ats-mode-hook 'format-all-mode))
