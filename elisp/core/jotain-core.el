;;; jotain-core.el --- Core Emacs settings for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Core Emacs configuration including UTF-8, sensible defaults,
;; backup file handling, and custom file management.

;;; Code:

(require 'jotain-paths)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Sensible defaults
(setq-default
 ;; Indentation
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 
 ;; Line endings
 require-final-newline t
 
 ;; Editing
 truncate-lines nil
 word-wrap t
 sentence-end-double-space nil)

;; Better defaults
(setq
 ;; Increase undo limits
 undo-limit (* 8 1024 1024)
 undo-strong-limit (* 12 1024 1024)
 undo-outer-limit (* 120 1024 1024)
 
 ;; Allow recursive minibuffers
 enable-recursive-minibuffers t
 
 ;; Don't warn about large files
 large-file-warning-threshold (* 100 1024 1024)
 
 ;; Always load newest bytecode
 load-prefer-newer t
 
 ;; Confirm before quitting
 confirm-kill-emacs 'y-or-n-p
 
 ;; Use y/n instead of yes/no
 use-short-answers t
 
 ;; Move deleted files to trash
 delete-by-moving-to-trash t
 
 ;; Better scrolling
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 
 ;; No ring bell
 ring-bell-function 'ignore
 visible-bell nil)

;; Enable useful modes
(winner-mode 1)                  ; Undo/redo window configurations
(global-auto-revert-mode 1)      ; Auto-revert buffers when files change
(delete-selection-mode 1)        ; Replace selection when typing
(savehist-mode 1)               ; Save minibuffer history
(save-place-mode 1)             ; Remember cursor position
(electric-pair-mode 1)          ; Auto-pair brackets
(show-paren-mode 1)             ; Highlight matching parens

;; Configure backup files - keep them out of the way
(setq backup-directory-alist
      `(("." . ,(jotain-paths-cache-file "backups/")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil)

;; Auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,(jotain-paths-cache-file "auto-save/") t)))

;; Save history across sessions
(setq savehist-file (jotain-paths-state-file "history"))
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Save cursor position
(setq save-place-file (jotain-paths-state-file "places"))

;; Custom file - don't pollute init.el
(setq custom-file (jotain-paths-state-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Recent files
(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (jotain-paths-state-file "recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude (jotain-paths-cache-file ".*"))
  (add-to-list 'recentf-exclude (jotain-paths-state-file ".*"))
  (recentf-mode 1))

;; Better buffer names for duplicate filenames
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Project management (built-in)
(use-package project
  :ensure nil
  :config
  (setq project-list-file (jotain-paths-state-file "projects")))

(provide 'jotain-core)
;;; jotain-core.el ends here
