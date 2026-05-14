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

;;; @doc Defaults for the built-in comment commands (M-;, C-x C-;, M-j).
;;; `comment-multi-line' makes M-j continue inside an open block
;;; comment instead of closing/reopening; `extra-line' style puts
;;; opening and closing delimiters on their own lines for
;;; `comment-region'; `comment-empty-lines' makes `comment-region'
;;; treat blank lines the same as content lines;
;;; `comment-auto-fill-only-comments' keeps automatic line wrapping
;;; (when `auto-fill-mode' is on) confined to comments. C-c ; is an
;;; ergonomic alias for `comment-line' — C-; is taken by embark-dwim.
;;; Per-mode overrides go in the language module via a named hook:
;;;   (defun my-foo-mode-setup ()
;;;     (setq-local comment-multi-line nil))
;;;   (add-hook 'foo-mode-hook #'my-foo-mode-setup)
(use-package newcomment
  :ensure nil
  :bind ("C-c ;" . comment-line)
  :custom
  (comment-multi-line t)
  (comment-style 'extra-line)
  (comment-empty-lines t)
  (comment-auto-fill-only-comments t))

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

;;; @doc Make `M-x re-builder' use string syntax — the same form you'd
;;; paste into `re-search-forward' — instead of the default `read'
;;; syntax that requires escaping every backslash twice.
(use-package re-builder
  :ensure nil
  :custom (reb-re-syntax 'string))

(provide 'init-editing)
;;; init-editing.el ends here
