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
  
  :hook (org-mode . visual-line-mode)
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

(provide 'writing)
;;; writing.el ends here
