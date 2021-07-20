(use-package evil
  :disabled t
  :hook (after-init . evil-mode)
  :config
  (progn
    ;; stop messing with clipboard please
    (evil-define-operator evil-destroy (beg end type register yank-handler)
      (evil-delete beg end type ?_ yank-handler))
    (evil-define-operator evil-destroy-replace (beg end type register yank-handler)
      (evil-destroy beg end type register yank-handler)
      (evil-paste-before 1 register))

    ;; Make evil undo only one char at a time
    (advice-add 'undo-auto--last-boundary-amalgamating-number
                :override #'ignore)
    (setq evil-want-fine-undo t)
    (add-hook 'with-editor-mode-hook 'evil-insert-state)))
