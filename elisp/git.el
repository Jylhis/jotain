;;; git.el --- Git and version control configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Git integration with Magit and related tools.

;;; Code:

(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t "Show word-granularity differences within diff hunks")
  (magit-diff-refine-ignore-whitespace t "Ignore whitespace changes in word-granularity differences")
  (magit-diff-hide-trailing-cr-characters t "Hide trailing ^M")
  (magit-diff-context-lines 5 "Show more context lines for better understanding")
  (magit-repository-directories '(("~/Developer" . 1)))
  :config
  ;; Show worktrees section in status buffer if multiple worktrees exist
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t))

(use-package magit-todos
  :ensure t
  :after magit
  :commands (magit-todos-mode global-magit-todos-mode)
  :custom
  (magit-todos-depth 1 "Limit scanning depth to improve performance on large repositories"))

(use-package gited
  :ensure t
  :after magit)

(use-package diff-hl
  :ensure t
  :after magit
  :demand t
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  (diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  ;; Enhanced three-way merge configuration
  (ediff-diff-options "-w")
  (ediff-custom-diff-options "-u")
  (ediff-merge-revisions-with-ancestor t)
  :config
  (setq ediff-control-frame-parameters
        '((name . "Ediff Control")
          (width . 60)
          (height . 14)
          (left . 200)
          (top . 200)
          (minibuffer . nil)
          (user-position . t)
          (vertical-scroll-bars . nil)
          (scrollbar-width . 0)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0))))

;; Git prefix keymap: C-c g is a prefix for all git commands
(defvar-keymap jotain-git-prefix-map
  :doc "Prefix keymap for git commands."
  "s" #'magit-status
  "t" #'git-timemachine)
(keymap-global-set "C-c g" jotain-git-prefix-map)

;; Forge: GitHub/GitLab/etc. integration for Magit
;; Authentication: configure auth-source with a GitHub token.
;; Add to ~/.authinfo.gpg:
;;   machine api.github.com login YOUR_USERNAME^forge password ghp_YOUR_TOKEN
;; See https://magit.vc/manual/forge/Token-Creation.html
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-connector 'emacsql-sqlite-builtin "Use Emacs 30 built-in SQLite for forge database"))

(use-package git-timemachine
  :ensure t
  :defer t)

(provide 'git)
;;; git.el ends here
