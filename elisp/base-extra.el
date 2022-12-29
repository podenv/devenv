;; Setup ivy
(use-package smex)
(use-package counsel
  :config
  (counsel-mode 1)
  )
(use-package swiper
  :bind (("M-s" . swiper))
  )
(use-package ivy
  :diminish
  :config
  ;; only show 18 candidates
  (setq ivy-height 18)
  ;; load recenf and bookmarks when using ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; allow out of order inputs
  (setq ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
  ;; Show full path for virtual buffers
  (setq ivy-virtual-abbreviate 'full)
  ;; Press C-p when you're on the first candidate to select your input
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1)
  )

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dash
  :config
  ;; Syntax highlighting
  (dash-enable-font-lock))

;; Use C-<insert> and Shift-<insert> to copy and paste the clipboard.
;; Use mouse and middle-mouse to copy and paste the primary clipboard.
(use-package simpleclip
  :config
  (simpleclip-mode 1))

(use-package projectile
  :defer t
  :bind-keymap (("s-p"   . projectile-command-map)
                ("C-c p" . projectile-command-map))
  :init
  ;; Allow all file-local values for project root
  (put 'projectile-project-root 'safe-local-variable 'stringp)
  :config
  ;; cache projectile project files
  ;; projectile-find-files will be much faster for large projects.
  ;; C-u C-c p f to clear cache before search.
  (setq-default
   ;; Start magit-status when switching project
   ;; projectile-switch-project-action (quote magit-status)

   ;; Don't show "Projectile" as liter when not in a project
   projectile-mode-line-prefix ""
   ;; Cache management
   projectile-file-exists-local-cache-expire 30
   projectile-enable-caching t
   ;; Use ivy
   projectile-completion-system 'ivy)
  (projectile-mode))

(use-package company
  :diminish
  :config
  (global-company-mode 1))

(use-package bufler)

;; visual replace feedback
(use-package anzu
  :diminish
  :config
  (global-anzu-mode 1))
