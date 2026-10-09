;;; init-editing.el --- Text editing primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-level editing behaviour: pair insertion, region selection,
;; whitespace handling, undo, auto-save.

;;; Code:

;;; @doc Auto-insert matching delimiters as you type. Built-in.
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode 1))

;;; @doc Typing replaces the active region, as in most editors.
;;; Built-in.
(use-package delsel
  :ensure nil
  :config (delete-selection-mode 1))

;;; @doc Small built-in editing knobs. The shell-command prompt shows
;;; the working directory. On Emacs 31+, `kill-region-dwim' makes C-w
;;; with no active region kill the previous word instead of erroring,
;;; and `delete-pair-push-mark' leaves a mark on the former pair
;;; contents so C-x C-x re-selects them.
(use-package simple
  :ensure nil
  :custom
  (shell-command-prompt-show-cwd t)
  :config
  (when (boundp 'kill-region-dwim)
    (setopt kill-region-dwim 'emacs-word))
  (when (boundp 'delete-pair-push-mark)
    (setopt delete-pair-push-mark t)))

;; Spaces by default.  editorconfig and dtrt-indent (init-prog.el) still
;; override per project/file, and tab-requiring modes (makefile-mode,
;; go-ts-mode) set it themselves.
(setopt indent-tabs-mode nil)

;;; @doc Strip trailing whitespace and stray tabs on save without
;;; reformatting the rest of the buffer. Built-in. Skipped during
;;; super-save's automatic saves, which keep whitespace on the current
;;; line.
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

;;; @doc Treats CamelCase word parts as separate words for M-f / M-b /
;;; M-d. Built-in; programming modes only.
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)))

;;; @doc Defaults for the built-in comment commands (M-;, C-x C-;, M-j).
;;; M-j continues an open block comment instead of closing and
;;; reopening it; `comment-region' puts the delimiters on their own lines
;;; and comments blank lines too; auto-fill wraps only comments. C-c ; is
;;; an alias for `comment-line' (C-; is embark-dwim); in Org it is
;;; `org-toggle-comment'. Override per mode with a named hook in the
;;; language module that sets these with `setq-local'.
(use-package newcomment
  :ensure nil
  :bind ("C-c ;" . comment-line)
  :custom
  (comment-multi-line t)
  (comment-style 'extra-line)
  (comment-empty-lines t)
  (comment-auto-fill-only-comments t))

;;; @doc Bindings for the two transpose commands Emacs ships unbound:
;;; sentences (C-x M-t) and paragraphs (C-x C-M-t), next to the stock
;;; C-t, M-t, C-x C-t and C-M-t.
(use-package emacs
  :ensure nil
  :bind
  (("C-x M-t"   . transpose-sentences)
   ("C-x C-M-t" . transpose-paragraphs)))

;;; @doc Regex replacement previewed as a unified diff (Emacs 30), which
;;; you then apply as a patch or discard: `M-s R` in the buffer, `M-s M-R`
;;; across files. The dired variant is bound in `init-navigation.el`.
(use-package replace
  :ensure nil
  :bind (("M-s R"   . replace-regexp-as-diff)
         ("M-s M-R" . multi-file-replace-regexp-as-diff)))

;;; @doc Semantic, tree-sitter-aware region expansion on C-=, a
;;; smaller successor to expand-region.
(use-package expreg
  :bind ("C-=" . expreg-expand))

;;; @doc Visual multi-cursor editing. C-> and C-< select the next/prev
;;; occurrence; C-S-c C-S-c selects every occurrence in the buffer.
(use-package multiple-cursors
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-S-c C-S-c" . mc/mark-all-like-this)))

;;; @doc Visual undo tree on C-x u, built on the native undo list (no
;;; side files).
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;;; @doc Saves file buffers automatically (when idle, on buffer switch
;;; and on focus loss), writing the real file. Local files only. Strips
;;; trailing whitespace except on the current line.
(use-package super-save
  :hook (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line))

;;; @doc `M-x re-builder' uses `string' syntax (the form of a regexp typed
;;; interactively) instead of `read' syntax with doubled backslashes.
(use-package re-builder
  :ensure nil
  :defer t
  :custom (reb-re-syntax 'string))

;;; @doc Counts command invocations to disk; `M-x keyfreq-show` ranks
;;; the busiest commands so you can spot rebinding opportunities.
(use-package keyfreq
  :functions (keyfreq-mode keyfreq-autosave-mode)
  :hook (after-init . keyfreq-mode)
  :custom
  (keyfreq-file (jotain-var-file "keyfreq.el"))
  (keyfreq-file-lock (jotain-var-file "keyfreq.lock"))
  :config
  (keyfreq-autosave-mode 1))

(provide 'init-editing)
;;; init-editing.el ends here
