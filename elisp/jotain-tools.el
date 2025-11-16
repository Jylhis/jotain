;;; jotain-tools.el --- Git, Org, and productivity tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Productivity tools including:
;; - Git integration (Magit, diff-hl)
;; - Org-mode for writing and notes
;; - Enhanced help system
;; - Documentation tools

;;; Code:

;;; Git Integration

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-hide-trailing-cr-characters t))

(use-package magit-todos
  :ensure t
  :after magit
  :init
  (magit-todos-mode 1))

(use-package diff-hl
  :ensure t
  :after magit
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (diff-hl-flydiff-mode 1))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w"))

;;; Org-mode and Writing

(use-package org
  :custom
  (org-directory "~/Documents")
  (org-pretty-entities t)
  (org-clock-persist 'history)
  :config
  (org-clock-persistence-insinuate)
  :hook ((org-mode . visual-line-mode)
         (org-mode . jotain--setup-org-fonts))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :hook (org-mode . global-org-modern-mode))

;; Org exporters
(use-package ox-gfm :ensure t :defer t)
(use-package ox-hugo :ensure t :defer t)

(defun jotain--setup-org-fonts ()
  "Configure mixed fonts for org-mode."
  (interactive)
  (variable-pitch-mode 1)

  ;; Keep code elements in fixed-pitch
  (dolist (face '(org-table org-code org-block org-block-begin-line
                  org-block-end-line org-verbatim org-special-keyword
                  org-property-value org-tag))
    (set-face-attribute face nil :inherit 'fixed-pitch))

  ;; Visual hierarchy
  (set-face-attribute 'org-document-title nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.1 :weight 'semi-bold)
  (set-face-attribute 'org-level-3 nil :height 1.05))

;;; Enhanced Help System

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ([remap describe-symbol] . helpful-symbol)))

(use-package apropos
  :custom
  (apropos-do-all t))

(use-package dash-docs
  :ensure t
  :defer t
  :custom
  (dash-docs-docsets-path "~/.local/share/Zeal/Zeal/docsets/")
  (dash-docs-browser-func 'eww))

(use-package consult-dash
  :ensure t
  :defer t
  :after consult)

(provide 'jotain-tools)
;;; jotain-tools.el ends here
