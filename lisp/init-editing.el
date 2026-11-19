;;; init-editing.el --- Text editing primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-level editing behaviour: pair insertion, region selection,
;; whitespace handling, undo, auto-save.

;;; Code:

(use-package simple
  :ensure nil
  :config
  (setopt kill-region-dwim 'emacs-word)
  (setopt delete-pair-push-mark t))

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

(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)))

(use-package newcomment
  :ensure nil
  :bind ("C-c ;" . comment-line)
  :custom
  (comment-multi-line t)
  (comment-style 'extra-line)
  (comment-empty-lines t)
  (comment-auto-fill-only-comments t))

(use-package emacs
  :ensure nil
  :bind
  (("C-x M-t"   . transpose-sentences)
   ("C-x C-M-t" . transpose-paragraphs)))

(use-package replace
  :ensure nil
  :bind (("M-s R"   . replace-regexp-as-diff)
         ("M-s M-R" . multi-file-replace-regexp-as-diff)))

(use-package expreg
  :bind ("C-=" . expreg-expand))

(use-package multiple-cursors
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-S-c C-S-c" . mc/mark-all-like-this)))

(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(use-package super-save
  :hook (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line))

(use-package re-builder
  :ensure nil
  :defer t
  :custom (reb-re-syntax 'string))

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
