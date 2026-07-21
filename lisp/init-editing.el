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

;;; @doc Two small built-in editing knobs. `kill-region-dwim' makes C-w
;;; with no active region kill a word backwards instead of erroring;
;;; `delete-pair-push-mark' leaves a mark on the former pair contents so
;;; C-x C-x re-selects them. Both are Emacs 31+, guarded so the config
;;; loads on Emacs 30. (The "diff this buffer against its file" action
;;; under `d' during `save-some-buffers' is already built in since Emacs
;;; 30, so it needs no config here.)
(use-package simple
  :ensure nil
  :config
  (when (boundp 'kill-region-dwim)
    (setopt kill-region-dwim 'emacs-word))
  (when (boundp 'delete-pair-push-mark)
    (setopt delete-pair-push-mark t)))

;;; @doc Strip trailing whitespace and stray tabs on save without
;;; reformatting the rest of the buffer. Built-in. Skipped during
;;; super-save's automatic saves so its except-current-line
;;; handling keeps whitespace at point intact.
(use-package whitespace
  :ensure nil
  :preface
  (defun jotain-editing--whitespace-cleanup-unless-auto-save ()
    "Run `whitespace-cleanup' except during super-save's automatic saves."
    (unless (bound-and-true-p super-save-in-progress)
      (whitespace-cleanup)))
  :hook (before-save . jotain-editing--whitespace-cleanup-unless-auto-save)
  :custom
  (whitespace-style '(face trailing tabs tab-mark)))

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

;;; @doc Emacs 30 ships `replace-regexp-as-diff' and
;;; `multi-file-replace-regexp-as-diff' — run a regex replacement, but
;;; see the result as a unified diff first and either apply it as a
;;; patch or abort. Worth reaching for on any non-trivial refactor.
;;; The dired-marked variant is bound in `init-navigation.el'.
(use-package replace
  :ensure nil
  :bind (("M-s R"   . replace-regexp-as-diff)
         ("M-s M-R" . multi-file-replace-regexp-as-diff)))

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
  :hook (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line))

;;; @doc Make `M-x re-builder' use string syntax — the same form you'd
;;; paste into `re-search-forward' — instead of the default `read'
;;; syntax that requires escaping every backslash twice.
(use-package re-builder
  :ensure nil
  :custom (reb-re-syntax 'string))

(provide 'init-editing)
;;; init-editing.el ends here
