;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Prose: wrapping, proportional fonts, spell check, Markdown, notes, and
;; PDFs.  Org lives in init-org.el.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))
(declare-function global-jinx-mode "jinx" (&optional arg))

(defgroup jotain-writing nil
  "Prose and note-taking settings."
  :group 'text)

(defcustom jotain-notes-directory (expand-file-name "~/Documents/notes/")
  "Directory holding plain-text notes.
Shared root for `denote-directory' (below) and `org-directory'
(init-org.el), so denote, org-capture and org-roam files land in
one tree.  It need not exist yet."
  :type 'directory
  :group 'jotain-writing)

(use-package text-mode
  :ensure nil
  :custom
  ;; Emacs 30's default adds `ispell-completion-at-point' to text-mode
  ;; capfs.  Without a word list at `ispell-alternate-dictionary' (no
  ;; /usr/share/dict/words on NixOS) every corfu popup in a text buffer
  ;; errors "No plain word-list found".  jinx does spell checking.
  (text-mode-ispell-word-completion nil)
  :hook
  ((text-mode . visual-line-mode)
   (text-mode . visual-wrap-prefix-mode)))

(use-package mixed-pitch
  :preface
  (defun jotain-writing--keep-monospace ()
    "Turn off `mixed-pitch-mode' in a column-sensitive text-mode buffer."
    (mixed-pitch-mode -1))
  :hook ((text-mode        . mixed-pitch-mode)
         (git-commit-setup . jotain-writing--keep-monospace)
         (message-mode     . jotain-writing--keep-monospace))
  :config
  ;; Extend the list after load: a `:custom' value that reads the
  ;; variable runs before the deferred package defines it.
  (dolist (face '(markdown-list-face markdown-code-face markdown-inline-code-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(use-package jinx
  :preface
  (defun jotain-writing--enable-jinx ()
    "Enable `global-jinx-mode', demoting errors so startup continues.
Jinx compiles a native module against enchant on first load; if that
fails (MELPA fallback without cc/enchant), a raw error would abort the
remaining `emacs-startup-hook' functions."
    (with-demoted-errors "init-writing: jinx unavailable: %S"
      (global-jinx-mode 1)))
  :custom
  ;; The Nix distribution bundles en/fi/de/fr aspell dictionaries
  ;; (nix/mk-overlay.nix); switch per buffer with C-M-$, or set e.g.
  ;; "en_GB fi".  One default language stops foreign words passing as
  ;; correct English.
  (jinx-languages "en_GB")
  :hook (emacs-startup . jotain-writing--enable-jinx)
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package markdown-mode
  :preface
  (defun jotain-writing--disable-visual-wrap-prefix ()
    "Turn off `visual-wrap-prefix-mode' in the current buffer.
The `text-mode' hook enables it, and `markdown-mode' derives from
`text-mode'.  The mode measures the list marker prefix in pixels and
re-measures it while font lock reruns, so with a proportional font the
text of a list item shifts as you type.  `visual-line-mode' stays on."
    (when (bound-and-true-p visual-wrap-prefix-mode)
      (visual-wrap-prefix-mode -1)))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.mdx\\'"      . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  :hook (markdown-mode . jotain-writing--disable-visual-wrap-prefix)
  :bind (:map markdown-mode-map
              ("C-c m h" . markdown-toggle-markup-hiding)
              ("C-c m i" . markdown-toggle-inline-images)
              ("C-c m l" . markdown-insert-link)
              ("C-c m c" . markdown-insert-gfm-code-block)
              ("C-c m o" . markdown-follow-thing-at-point)
              ("C-c m d" . markdown-do)
              ("C-c m p" . markdown-live-preview-mode)
              ("C-c m <left>"  . markdown-promote)
              ("C-c m <right>" . markdown-demote)))

(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory jotain-notes-directory)
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
