;;; init-editing.el --- Text editing primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-level editing behaviour: pair insertion, region selection,
;; whitespace handling, undo. Things you'd expect any modern editor to
;; do without thinking about it.

;;; Code:

(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode 1))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode 1))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face trailing tabs tab-mark))
  (whitespace-action '(auto-cleanup)))

(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)))

;; expreg is the smaller, treesit-aware successor to expand-region.
;; Wherever treesit is on (which is everywhere now), it produces better
;; semantic expansions with much less code.
(use-package expreg
  :bind ("C-=" . expreg-expand))

(use-package multiple-cursors
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Visual undo tree on C-x u. Stateless, no .undo-tree files cluttering
;; the filesystem like undo-tree.el used to leave behind.
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; Auto-save buffers when idle and on focus loss. Replaces the default
;; auto-save mechanism (which writes to #foo# files) with regular saves
;; to the actual file. Trims trailing whitespace except on the line
;; you're currently editing.
(use-package super-save
  :diminish
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config (super-save-mode 1))

(provide 'init-editing)
;;; init-editing.el ends here
