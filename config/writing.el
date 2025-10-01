;;; writing.el --- Writing and documentation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Org-mode, note-taking, and documentation tools.

;;; Code:

;; Load custom utilities for org
(require 'utils (expand-file-name "lisp/utils" user-emacs-directory))

(use-package org
  :custom
  (org-directory "~/Documents")
  (org-pretty-entities t)
  (org-clock-persist 'history)
  :config
  (org-clock-persistence-insinuate)
  
  ;; Set up dynamic agenda files
  (my/setup-org-agenda-files)
  
  :hook ((org-mode . visual-line-mode)
         (org-mode . j10s-setup-org-fonts))
  :bind (
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package org-appear
  :ensure
  :hook
  (org-mode . org-appear-mode)
  :after org)

(use-package org-modern
  :ensure
  :after org
  :hook
  (org-mode . global-org-modern-mode))

;; Obsidian integration for note-taking and knowledge management
;; (use-package obsidian
;;   :ensure
;;   :demand t
;;   :if (locate-library "obsidian")
;;   :custom
;;   ;; Default vault location - can be customized per user
;;   (obsidian-vault-directory (expand-file-name "~/Documents/Obsidian"))
;;   ;; Enable completion for wiki-style links
;;   (obsidian-enable-completion t)
;;   ;; Integration with Vertico/Consult completion
;;   (obsidian-use-advanced-ui t)
;;   :config
;;   ;; Set up file detection for Obsidian markdown files
;;   (obsidian-specify-path obsidian-vault-directory)
;;   ;; Enable global mode for Obsidian features
;;   (global-obsidian-mode t)
;;   :hook
;;   ;; Enhance markdown files in Obsidian vault with obsidian-mode
;;   (markdown-mode . (lambda ()
;;                      (when (obsidian-file-p buffer-file-name)
;;                        (obsidian-mode 1))))
;;   :bind
;;   ;; Convenient keybindings for Obsidian operations
;;   (:map obsidian-mode-map
;;         ("C-c C-o" . obsidian-follow-link-at-point)
;;         ("C-c C-b" . obsidian-backlink-jump)
;;         ("C-c C-l" . obsidian-insert-wikilink)
;;         ("C-c C-f" . obsidian-jump)
;;         ("C-c C-n" . obsidian-capture)
;;         ("C-c C-r" . obsidian-move-file)
;;         ("C-c C-t" . obsidian-tag-find)
;;         ("C-c C-g" . obsidian-search))
;;   ;; Global keybindings for Obsidian
;;   :bind (("C-c o j" . obsidian-jump)
;;          ("C-c o c" . obsidian-capture)
;;          ("C-c o d" . my/obsidian-daily-note)
;;          ("C-c o s" . obsidian-search)
;;          ("C-c o t" . obsidian-tag-find)))

;; Org exporters
(use-package ox-slack
  :ensure
  :after org
  :defer t)

(use-package ox-jira
  :ensure
  :after org
  :defer t)

(use-package ox-hugo
  :ensure
  :after org
  :defer t)

(use-package ox-gfm
  :ensure
  :after org
  :defer t)

;;; Variable-pitch font setup for better typography

(defun j10s-setup-org-fonts ()
  "Configure mixed fonts for org-mode - variable pitch for text, fixed for code."
  (interactive)
  ;; Enable variable pitch mode for better prose readability
  (variable-pitch-mode 1)
  
  ;; Keep code elements in fixed-pitch font
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-tag nil :inherit 'fixed-pitch)
  
  ;; Better visual hierarchy with font sizes  
  (set-face-attribute 'org-document-title nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.1 :weight 'semi-bold)
  (set-face-attribute 'org-level-3 nil :height 1.05)
  
  ;; Subtle styling for meta elements
  (set-face-attribute 'org-meta-line nil :height 0.9 :slant 'italic)
  (set-face-attribute 'org-drawer nil :height 0.9 :slant 'italic))

;; Also update fonts.el serif font references
(when (facep 'j10s-fonts-serif)
  (set-face-attribute 'j10s-fonts-serif nil :family "Source Serif Pro"))

(provide 'writing)
;;; writing.el ends here
