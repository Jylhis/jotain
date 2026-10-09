;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Prose: wrapping, proportional fonts, spell check, Markdown, notes, and
;; PDFs.  Org lives in init-org.el.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))
(declare-function global-jinx-mode "jinx" (&optional arg))

(defgroup jotain-writing nil
  "Jotain prose and note-taking settings."
  :group 'text)

(defcustom jotain-notes-directory (expand-file-name "~/Documents/notes/")
  "Directory holding plain-text notes.
Shared root for `denote-directory' (below) and `org-directory'
(init-org.el), so denote, org-capture and org-roam files land in
one tree.  It need not exist yet."
  :type 'directory
  :group 'jotain-writing)

;;; @doc Built-in text-mode tweaks for prose: visual line wrap and
;;; hanging indent on wrapped lines. Proportional fonts come from
;;; `mixed-pitch' (below); code modes stay monospaced.
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

;;; @doc Proportional fonts for prose via the `variable-pitch' face,
;;; while code, tables, and verbatim spans stay monospaced
;;; (`fixed-pitch'). This keeps Org tables aligned, where a blanket
;;; `variable-pitch-mode' would leave them ragged. Enabled in every
;;; text-mode buffer and opted out of the column-sensitive ones: YAML
;;; (see init-lang-data), commit messages, and email.
(use-package mixed-pitch
  :preface
  (defun jotain-writing--keep-monospace ()
    "Turn off `mixed-pitch-mode' in a column-sensitive text-mode buffer."
    (mixed-pitch-mode -1))
  :hook ((text-mode        . mixed-pitch-mode)
         (git-commit-setup . jotain-writing--keep-monospace)
         (message-mode     . jotain-writing--keep-monospace)))

;;; @doc Just-in-time spell check using enchant. `global-jinx-mode'
;;; replaces `flyspell-mode' and `flyspell-prog-mode': jinx keys off
;;; faces, so in code it checks only comments and strings. M-$ corrects
;;; the word at point (C-u M-$ walks every misspelling); C-M-$ switches
;;; dictionaries. YAML buffers opt out (init-lang-data).
(use-package jinx
  :preface
  (defun jotain-writing--enable-jinx ()
    "Enable `global-jinx-mode', demoting errors so startup continues.
Jinx compiles a native module against enchant on first load; if that
fails (MELPA fallback without cc/enchant), a raw error would abort the
remaining `emacs-startup-hook' functions."
    (with-demoted-errors "jotain: jinx unavailable: %S"
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

;;; @doc Markdown major mode with native code-block fontification,
;;; heading scaling, and nested imenu. README.md opens in gfm-mode. Beside
;;; the stock `C-c C-...' bindings, a `C-c m ...' prefix holds the common
;;; ones: markup/image hiding, link and code-block insertion, follow-link,
;;; promote/demote, and live preview. gfm-mode inherits them.
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-nested-imenu-heading-index t)
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

;;; @doc Plain-text notes with strict file-naming rules, stored under
;;; `jotain-notes-directory` (default `~/Documents/notes`), shared with
;;; Org (init-org.el).
(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory jotain-notes-directory)
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

;;; @doc In-Emacs PDF viewing: search, annotate, follow links, outline.
;;; The Nix pdf-tools package ships the poppler-based `epdfinfo` server.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
