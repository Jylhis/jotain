;;; jotain-editor-core.el --- Core editor functionality for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Core editor functionality including useful editing commands,
;; multiple cursors, better undo, etc.

;;; Code:

;; Better undo system
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree" (or (getenv "XDG_CACHE_HOME")
                                                    "~/.cache/jotain")))))
  (global-undo-tree-mode 1))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; Expand region - smart selection expansion
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Move text around easily
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; Better search and replace
(use-package anzu
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

;; Highlight symbol at point
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

;; Rainbow delimiters for programming
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Smartparens for better paren handling
(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Which-key for keybinding help
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))

(provide 'jotain-editor-core)
;;; jotain-editor-core.el ends here
