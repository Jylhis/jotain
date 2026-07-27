;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing prose is a different mode of editing than writing code: word
;; wrap, spell check, link insertion all matter, while flymake and
;; eldoc don't. Org has its own file because it's huge enough to deserve
;; one.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))
(declare-function global-jinx-mode "jinx" (&optional arg))

(defgroup jotain-writing nil
  "Jotain prose and note-taking settings."
  :group 'text)

(defcustom jotain-notes-directory (expand-file-name "~/Documents/notes/")
  "Directory holding plain-text notes.
Single root for the whole note-taking stack: `denote-directory'
(below) and `org-directory' (init-org.el) both read it, so denote
notes, org-capture inboxes, and org-roam files all land under one
tree. Nothing breaks if it does not exist yet — denote and
org-roam create it on first use."
  :type 'directory
  :group 'jotain-writing)

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
;;; per-buffer flyspell setup. `global-jinx-mode' replaces both
;;; `flyspell-mode' and `flyspell-prog-mode' in one shot: jinx keys off
;;; faces, so in a code buffer it checks only comments and strings and
;;; leaves the code alone, no `flyspell-prog-mode' equivalent needed.
;;; Enabled from `emacs-startup' rather than a bag of per-mode hooks so
;;; prose *and* code get checked. M-$ corrects the word at point
;;; (C-u M-$ walks every misspelling); C-M-$ switches dictionaries.
;;; Data/config buffers that only look like prose opt back out in
;;; init-lang-data.
(use-package jinx
  :preface
  (defun jotain-writing--enable-jinx ()
    "Enable `global-jinx-mode', demoting errors so startup continues.
Jinx compiles a native module against enchant on first load; if
that fails (MELPA-fallback mode without cc/enchant), a raw error
here would abort every later `emacs-startup-hook' entry."
    (with-demoted-errors "jotain: jinx unavailable: %S"
      (global-jinx-mode 1)))
  :custom
  ;; British English by default. The Nix distribution bundles the en/fi/de/
  ;; fr aspell dictionaries (nix/mk-overlay.nix); switch or combine them per
  ;; buffer with C-M-$, or set this to e.g. "en_GB fi" for a bilingual
  ;; buffer. A single default keeps a foreign-language word from silently
  ;; counting as correctly spelled English.
  (jinx-languages "en_GB")
  :hook (emacs-startup . jotain-writing--enable-jinx)
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
;;; are findable years later. Stored under `jotain-notes-directory`
;;; (default `~/Documents/notes`), shared with Org (init-org.el).
(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory jotain-notes-directory)
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

;;; @doc In-Emacs PDF viewing — search, annotate, follow links, outline.
;;; Needs libpoppler at build time, which the emacs-pgtk / emacs-mac
;;; variants in nixpkgs already provide.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
