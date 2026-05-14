;;; init-editing.el --- Text editing primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-level editing behaviour: pair insertion, region selection,
;; whitespace handling, undo. Things you'd expect any modern editor to
;; do without thinking about it.

;;; Code:

;;; @doc Auto-insert matching delimiters as you type. Built-in.
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode 1))

;;; @doc When a region is active, typing replaces it instead of leaving
;;; the selection alone. Built-in, on by default in most modern
;;; editors — Emacs needs this opt-in.
(use-package delsel
  :ensure nil
  :config (delete-selection-mode 1))

;;; @doc Strip trailing whitespace and stray tabs on save without
;;; reformatting the rest of the buffer. Built-in.
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face trailing tabs tab-mark))
  (whitespace-action '(auto-cleanup)))

;;; @doc Treats CamelCase / snake_case word parts as separate words for
;;; M-f / M-b / M-d. Built-in. Programming-mode only — prose still
;;; gets whole-word motion.
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)))

;;; @doc Bindings for the two transpose commands Emacs ships without
;;; defaults, so the prose-level family (sentence, paragraph) is
;;; reachable alongside the built-in C-t (chars), M-t (words),
;;; C-x C-t (lines), and C-M-t (sexps, tree-sitter aware in Emacs
;;; 30+). Caveat: transpose-lines works on real newlines, so it
;;; gives surprising results under visual-line-mode where wrapped
;;; "lines" are visual only.
(use-package emacs
  :ensure nil
  :bind
  (("C-x M-t"   . transpose-sentences)
   ("C-x C-M-t" . transpose-paragraphs)))

;;; @doc Treesit-aware semantic region expansion. Smaller, faster
;;; successor to expand-region; produces better expansions with
;;; much less code now that treesit is everywhere.
(use-package expreg
  :bind ("C-=" . expreg-expand))

;;; @doc Visual multi-cursor editing. C-> and C-< select the next/prev
;;; occurrence; C-c C-< selects every occurrence in the buffer.
(use-package multiple-cursors
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;;; @doc Visual undo tree on C-x u. Stateless — no .undo-tree side files
;;; cluttering the filesystem like undo-tree.el used to leave
;;; behind.
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;;; @doc Auto-saves buffers when idle and on focus loss, writing the
;;; actual file rather than #foo# auto-save side files. Also strips
;;; trailing whitespace except on the current line so you don't
;;; fight your own cursor.
(use-package super-save
  :diminish
  :functions (super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config (super-save-mode 1))

(provide 'init-editing)
;;; init-editing.el ends here
