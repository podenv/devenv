;; The base configuration

(progn ; startup
  ;; increase gc to fasten load
  (setq gc-cons-threshold (* 256 1024 1024))

  ;; setup use-package
  (require 'package)
  (package-initialize 'noactivate)
  (eval-when-compile
    (require 'use-package))
  (setq use-package-verbose t)

  ;; bind-key enables use-package to use :bind for custom keybindings
  (use-package bind-key)

  ;; fullscreen no about
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message t)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message nil)

  ;; remove menu and scrollbars
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

;; start server for emacsclient
(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package man
  :defer t)

(use-package woman
  :defer t)

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (setq-default
   recentf-max-saved-items 1024))

(use-package saveplace
  :unless noninteractive
  :config
  (setq save-place-file (concat user-emacs-directory "saveplace"))
  (save-place-mode 1))

(use-package savehist
  :unless noninteractive
  :config
  (savehist-mode 1))

(progn ; better defaults
  ;; Ensure latest version of a file is loaded
  (setq-default load-prefer-newer t)
  ;; Navigate window with shift arrow
  (windmove-default-keybindings)
  ;; Do not ask for permission to kill a buffer
  (global-set-key (kbd "C-x k") 'kill-this-buffer)
  ;; Delete trailing space on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; Ensure file ends with newline
  (setq-default require-final-newline t)
  ;; Mark file executable if it has a shebang
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  ;; automatic parens pairing
  (electric-pair-mode 1)
  ;; show line and column in status line
  (line-number-mode 1)
  (column-number-mode 1)
  ;; make common symbols pretty
  (global-prettify-symbols-mode t)
  ;; visualize matching parens
  (show-paren-mode)
  ;; do not create lockfile
  (setq create-lockfiles nil)
  ;; set font
  (set-frame-font "Hack 9")
  (setq-default   ;; Select in primary selection, not clipboard
   select-enable-primary t
   select-enable-clipboard nil
   )
  (setq-default
   ;; Scroll one line when cursor moves out of the window
   scroll-step 1
   ;; Scroll up to 100 lines to bring back the cursor on screen
   scroll-conservatively 100
   )
  (setq-default
   ;; Display filepath in window title
   frame-title-format (list '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
   )
  (setq-default
   ;; Don't use tabs to indent, use 4 spaces instead
   indent-tabs-mode nil
   tab-width 4
   ;; smart tab behavior - indent or complete
   tab-always-indent 'complete
   )
  (setq-default
   ;; Paste at cursor position, not at mouse pointer
   mouse-yank-at-point t
   )
  (setq-default
   ;; Do not save backup in projects, keep them in home
   auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "saves/") t))
   backup-directory-alist `((".*" . ,(concat user-emacs-directory "saves/")))
   )
  (progn
    ;; Do not truncate lines by default
    (toggle-truncate-lines -1)
    ;; Default to utf-8 unix encoding
    (prefer-coding-system 'utf-8-unix)
    ;; Accept 'UTF-8' (uppercase) as a valid encoding in the coding header
    (define-coding-system-alias 'UTF-8 'utf-8)
    )
  ;; Auto wrap after some columns
  (setq-default fill-column 120)
  ;; Don't assume that sentences should have two spaces after periods. This ain't a typewriter.
  (setq sentence-end-double-space nil)
  )
(use-package diminish)

(use-package uniquify
  :defer 5
  :config
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  )

(use-package custom
  :no-require t
  :config
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package comint
  :defer t
  :config
  (setq comint-buffer-maximum-size 32768))

(use-package subword
  :diminish
  :hook
  ((python-mode yaml-mode go-mode clojure-mode cider-repl-mode) . subword-mode))

(use-package shr
  :defer t
  :config
  (setq shr-width 80)
  (setq shr-external-browser 'eww-browse-url)
  (setq shr-color-visible-luminance-min 80))

(use-package make-mode
  :mode (("Makefile" . makefile-gmake-mode)))

(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :config
  (global-eldoc-mode))

;; highlight odd whitespaces
(use-package whitespace
  :diminish
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style
        (quote
         (face trailing tabs lines empty space-after-tab space-before-tab tab-mark)))

  ;; highlight lines with more than `fill-column' characters
  (setq whitespace-line-column nil))

;; use colors to distinguish parens
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((emacs-lisp-mode lisp-mode hy-mode) . rainbow-delimiters-mode))

(use-package undo-tree
  :diminish
  :config
  ;; (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode))

;; enable gnupg integration
(use-package epa
  :defer t
  :config
  ;; Always replace encrypted text with plain text version
  (setq epa-replace-original-text t))
(use-package epa-file
  :config
  (epa-file-enable))
(use-package epg
  :defer t
  :config
  ;; Let Emacs query the passphrase through the minibuffer
  (setq epg-pinentry-mode 'loopback))

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

(use-package shell
  :config
  (add-hook 'shell-mode-hook 'turn-on-comint-history))
(use-package eshell
  :config
  (setq-default
   eshell-hist-ignoredups t
   ))

(use-package company
  :diminish
  :config
  (global-company-mode 1))

(use-package format-all
  :config
  (add-hook 'emacs-lisp-mode-hook 'format-all-mode))

;; utility
(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))
(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))
