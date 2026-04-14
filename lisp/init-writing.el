;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Writing prose is a different mode of editing than writing code: word
;; wrap, spell check, link insertion all matter, while flymake and
;; eldoc don't. Org has its own file because it's huge enough to deserve
;; one.

;;; Code:

(use-package text-mode
  :ensure nil
  :hook
  ((text-mode . visual-line-mode)
   (text-mode . visual-wrap-prefix-mode)
   (text-mode . variable-pitch-mode)))

(use-package jinx
  :hook ((text-mode . jinx-mode)
         (org-mode  . jinx-mode))
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t))

(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

;; In-Emacs PDF viewing — search, annotate, follow links, outline view.
;; Requires libpoppler at build time. Should already be available via the
;; emacs-pgtk / emacs-mac variants in nixpkgs.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
