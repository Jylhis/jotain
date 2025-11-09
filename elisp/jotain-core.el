;;; jotain-core.el --- Core Emacs settings for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Core Emacs configuration including UTF-8, sensible defaults,
;; backup file handling, and custom file management.

(message "loading core")
;;; Code:
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setopt tab-width 4)
(setopt fill-column 120)
(setopt sentence-end-double-space nil)

(setopt enable-recursive-minibuffers t)

(setopt confirm-kill-emacs 'y-or-n-p)
(setopt use-short-answers t)
(setopt delete-by-moving-to-trash t)

(setopt ring-bell-function 'ignore)
(setopt visible-bell nil)


(provide 'jotain-core)
;;; jotain-core.el ends here
