;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing prose is a different mode of editing than writing code: word
;; wrap, spell check, link insertion all matter, while flymake and
;; eldoc don't. Org has its own file because it's huge enough to deserve
;; one.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))

;;; @doc Built-in text-mode tweaks for prose: visual line wrap and
;;; hanging indent on wrapped lines. Proportional fonts come from
;;; `mixed-pitch' (below); code modes stay monospaced.
(use-package text-mode
  :ensure nil
  :custom
  ;; Emacs 30 added `ispell-completion-at-point' to every text-mode
  ;; buffer's `completion-at-point-functions' (this option defaults to
  ;; `completion-at-point'). With no word list at the
  ;; `ispell-alternate-dictionary' default (/usr/share/dict/words is
  ;; absent on NixOS), every corfu auto-completion in a text/YAML/
  ;; markdown/commit buffer throws "No plain word-list found". We spell
  ;; check with jinx (below), not the ispell capf, so turn it off.
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

;;; @doc Just-in-time spell check using enchant — no on-save scan, no
;;; per-buffer flyspell setup. M-$ corrects the word at point;
;;; C-M-$ switches dictionaries.
(use-package jinx
  :hook ((text-mode . jinx-mode)
         (org-mode  . jinx-mode))
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;; @doc Markdown major mode with native code-block fontification and
;;; heading scaling. README.md opens in gfm-mode for the GitHub
;;; dialect.
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t))

;;; @doc Plain-text note system with strict file-naming rules so notes
;;; are findable years later. Stored under `~/Documents/notes`.
(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

;;; @doc In-Emacs PDF viewing — search, annotate, follow links, outline.
;;; Needs libpoppler at build time, which the emacs-pgtk / emacs-mac
;;; variants in nixpkgs already provide.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
