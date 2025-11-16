;;; jotain-core.el --- Core Emacs settings and built-ins -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Core Emacs configuration including:
;; - Fundamental Emacs settings and behavior
;; - Built-in package configurations
;; - Essential editing enhancements
;; - Performance optimizations

;;; Code:

;;; Emacs Core Settings

(use-package emacs
  :custom
  ;; Emacs 30+ settings
  (text-mode-ispell-word-completion nil "Disable Ispell completion function")

  ;; UX improvements
  (use-short-answers t "Use y/n instead of yes/no")
  (create-lockfiles nil "Don't create lock files")
  (delete-by-moving-to-trash t "Move to trash instead of deleting")
  (visible-bell nil "No visual bell")
  (ring-bell-function #'ignore "No audible bell")

  ;; Text editing
  (sentence-end-double-space nil "Single space ends sentences")
  (word-wrap t "Wrap at word boundaries")
  (require-final-newline t "Files end with newline")

  ;; Scrolling
  (scroll-preserve-screen-position 1 "Keep cursor position while scrolling")

  ;; Minibuffer
  (enable-recursive-minibuffers t "Allow nested minibuffers")
  (minibuffer-follows-selected-frame t "Minibuffer follows frame")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)
   "Protect minibuffer prompt")

  ;; Completion
  (completion-ignore-case t "Case-insensitive completion")
  (read-buffer-completion-ignore-case t "Case-insensitive buffer names")
  (read-file-name-completion-ignore-case t "Case-insensitive file names")

  ;; Mouse
  (mouse-yank-at-point t "Paste at cursor, not click position")

  ;; Performance
  (load-prefer-newer t "Load newer compiled files")

  :config
  ;; Enable useful modes
  (context-menu-mode 1)
  (save-place-mode 1)
  (minibuffer-depth-indicate-mode 1)

  :bind
  ;; Unbind annoying suspend keys
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ;; Better navigation
   ("M-o" . other-window)
   ("M-p" . scroll-down-line)
   ("M-n" . scroll-up-line)))

;;; Window Management

(use-package window
  :custom
  (split-width-threshold 170 "Prefer side-by-side splits")
  (split-height-threshold nil "Avoid horizontal splits"))

;;; File Management

(use-package files
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :custom
  (auto-save-default nil "No autosave files")
  (make-backup-files nil "No backup files")
  (confirm-kill-processes nil "Don't ask when killing processes")
  (enable-local-variables t "Allow local variables")
  (require-final-newline t "Files end with newline"))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t "Revert non-file buffers too")
  :init
  (global-auto-revert-mode 1))

(use-package recentf
  :custom
  (recentf-max-saved-items 50 "Track 50 recent files")
  :init
  (recentf-mode 1))

(use-package savehist
  :init
  (savehist-mode 1))

;;; Editing Enhancements

(use-package simple
  :custom
  (read-extended-command-predicate
   #'command-completion-default-include-p
   "Hide unavailable commands in M-x")
  (kill-do-not-save-duplicates t "No duplicate kills")
  :config
  (line-number-mode 1)
  (column-number-mode 1))

(use-package subword
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package repeat
  :init
  (repeat-mode 1))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

;;; Dired

(use-package dired
  :custom
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t "Smart copy/move targets")
  (dired-listing-switches "-alh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-vc-rename-file t)
  (dired-omit-files
   (concat "\\`[.]?#\\|\\`[.][.]?\\'"
           "\\|^\\.git\\'"
           "\\|^__pycache__\\'")))

(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

(use-package ffap
  :custom
  (ffap-machine-p-known 'reject))

;;; Essential Packages

(use-package super-save
  :ensure t
  :diminish
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config
  (super-save-mode 1))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package drag-stuff
  :ensure t
  :diminish
  :hook ((text-mode prog-mode) . drag-stuff-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

(use-package pretty-sha-path
  :ensure t
  :hook ((shell-mode dired-mode) . pretty-sha-path-mode))

;;; Performance Monitoring

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;;; GC Optimizations

;; Trigger GC when idle
(run-with-idle-timer 5 t #'garbage-collect)

;; Prevent GC during minibuffer operations
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'jotain-core)
;;; jotain-core.el ends here
