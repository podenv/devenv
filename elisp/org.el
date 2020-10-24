(use-package org
  :config
  ;; Tell org where are the files
  (setq-default org-directory "~/org/")

  ;; Display image inline
  (setq org-startup-with-inline-images t)

  ;; Ensure shift arrows execute org commands, e.g. change todo state.
  (setq  org-support-shift-select nil)

  ;; Insead of "..." show "…" when there's hidden folded content
  ;; Some characters to choose from: …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱
  (setq org-ellipsis "⤵")

  ;; Change state using C-c C-t
  (setq org-use-fast-todo-selection t)

  ;; Disable shift-arrow binding
  (setq org-support-shift-select 'always)
  (progn
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right))
  )

;; Capture note with `C-c c t'
(use-package org-capture
  :bind ("C-c c" . org-capture)
  :demand
  :config
  (setq-default
   ;; Use any org-agendas file as refile target, only first level
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   ;; Use full outline paths for refile targets
   org-refile-use-outline-path t
   ;; Targets complete directly
   org-outline-path-complete-in-steps nil
   ))

(use-package org-agenda
  :config
  (setq-default
   ;; Start agenda at today
   org-agenda-start-on-weekday nil
   ;; Look for agenda item in every org files
   org-agenda-files '("~/org")
   ;; Match encrypted files too
   org-agenda-file-regexp "\\`[^.].*\\.org\\(.gpg\\)?\\'"
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil
   ;; Compact the block agenda view
   org-agenda-compact-blocks t
   ))
