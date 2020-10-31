;; Ensure nix-shell command does not fail: see https://github.com/NixOS/nix-mode/pull/114
(unless (boundp 'irony-additional-clang-options)
  (setq irony-additional-clang-options nil))
(unless (boundp 'woman-manpath)
  (require 'woman))

(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook 'format-all-mode))
