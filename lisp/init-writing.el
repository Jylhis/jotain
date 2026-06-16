;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing prose is a different mode of editing than writing code: word
;; wrap, spell check, link insertion all matter, while flymake and
;; eldoc don't. Org has its own file because it's huge enough to deserve
;; one.

;;; Code:

;;; @doc Built-in text-mode tweaks for prose: visual line wrap, hanging
;;; indent on wrapped lines, variable-pitch face. Code modes stay
;;; monospaced.
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
   (text-mode . visual-wrap-prefix-mode)
   (text-mode . variable-pitch-mode)))

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
