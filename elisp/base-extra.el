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

;; Setup flycheck
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (("<f3>" . flycheck-next-error))
  :config
  (global-flycheck-mode)
  ;; Only do flycheck on save
  (setq flycheck-check-syntax-automatically '(save mode-enable))

  ;; Enable next/prev error to cycle, from https://github.com/flycheck/flycheck/issues/64
  (defun flycheck-next-error-loop-advice (orig-fun &optional n reset)
    (condition-case err
        (apply orig-fun (list n reset))
      ((user-error)
       (let ((error-count (length flycheck-current-errors)))
         (if (and
              (> error-count 0)
              (equal (error-message-string err) "No more Flycheck errors"))
             (let* ((req-n (if (numberp n) n 1))
                    (curr-pos (if (> req-n 0) (- error-count 1) 0))
                    (next-pos (mod (+ curr-pos req-n) error-count)))
               (apply orig-fun (list (+ 1 next-pos) 'reset)))
           (signal (car err) (cdr err)))))))
  (advice-add 'flycheck-next-error :around #'flycheck-next-error-loop-advice))

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
