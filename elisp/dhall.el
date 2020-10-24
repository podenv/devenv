(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  (setq
   dhall-format-arguments (\` ("--ascii"))
   dhall-use-header-line nil))
