;;; init-vc.el --- Version control: vc + magit + diff-hl -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in `vc' bits live next to `magit' because in practice you tweak
;; them as a unit ("when I open a git file, what happens?"). diff-hl
;; ties the two together with fringe indicators.
;;
;; Future ideas (not yet wired up):
;;   - mergiraf      — syntax-aware structural merge driver
;;   - magit-delta   — render magit diffs through `delta' for syntax
;;                     highlighting (the same engine git-delta uses)
;;   - smerge / vc.el integration with the above for syntax-aware
;;     conflict resolution

;;; Code:

;;; @doc Built-in version control. Pinned to Git only — every other
;;; backend is a slow startup tax (probes every visited file's
;;; parents) you almost never benefit from.
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

;;; @doc Quick jump to a file git status reports as changed. Runs
;;; `git status --porcelain=v1' and offers M/A/D/R/?? entries
;;; through `completing-read'. Adapted from Rahul M. Juliato's
;;; emacs-solo/switch-git-status-buffer.
(use-package vc-git
  :ensure nil
  :bind ("C-x C-g" . jotain-switch-git-status-buffer)
  :preface
  (declare-function vc-git-root "vc-git" (file))
  (defun jotain-switch-git-status-buffer ()
    "Switch to a file git status reports as changed in this repo.
Candidates are parsed from `git status --porcelain=v1' (modified,
added, deleted, renamed, untracked) and offered through
`completing-read'."
    (interactive)
    (require 'vc-git)
    (let ((repo-root (vc-git-root default-directory)))
      (if (not repo-root)
          (message "Not inside a Git repository.")
        (let* ((expanded-root (expand-file-name repo-root))
               (default-directory expanded-root)
               (cmd-output (shell-command-to-string
                            "git status --porcelain=v1"))
               (target-files
                (let (files)
                  (dolist (line (split-string cmd-output "\n" t)
                                (nreverse files))
                    (when (> (length line) 3)
                      (let ((status (substring line 0 2))
                            (path-info (substring line 3)))
                        (cond
                         ((string-match-p "^R" status)
                          (let ((new-path (cadr (split-string
                                                 path-info " -> " t))))
                            (when new-path
                              (push (cons (format "R  %s" new-path)
                                          new-path)
                                    files))))
                         ((string-match-p "[MAD?]" status)
                          (push (cons (format "%s %s" status path-info)
                                      path-info)
                                files)))))))))
          (if (not target-files)
              (message "No changed files in this repository.")
            (let* ((selection (completing-read
                               "Switch to git-changed file: "
                               (mapcar #'car target-files) nil t))
                   (file-path (cdr (assoc selection target-files))))
              (when file-path
                (find-file (expand-file-name file-path
                                             expanded-root))))))))))

;;; @doc The Git porcelain. Bound C-x g for status, C-x M-g for global
;;; dispatch, C-c g for the file-specific menu. Refined hunks +
;;; whitespace-ignoring diffs are turned on globally.
(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g"   . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-hide-trailing-cr-characters t)
  (magit-diff-context-lines 5)
  (magit-save-repository-buffers 'dontask)
  (magit-repository-directories '(("~/Developer" . 2)))
  :config
  ;; Show worktrees as a section in magit-status when more than one exists.
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t))

;;; @doc Surfaces TODO/FIXME/HACK comments as a section in magit-status.
;;; Scan depth pinned to 1 so it stays fast on large repos.
(use-package magit-todos
  :after magit
  :commands (magit-todos-mode global-magit-todos-mode)
  :custom
  (magit-todos-depth 1))

;;; @doc PRs, issues, and reviews from GitHub/GitLab/Forgejo inside
;;; magit. Uses the Emacs-30 built-in sqlite so no external
;;; emacsql binary is needed. Auth via ~/.authinfo.gpg
;;; (machine api.github.com login USER^forge password ghp_…).
(use-package forge
  :after magit
  :custom
  (forge-database-file (jotain-var-file "forge/database.sqlite"))
  (forge-post-directory (jotain-var-file "forge/posts/"))
  (forge-database-connector 'emacsql-sqlite-builtin))

;;; @doc Built-in transient menu system that magit/forge are built on.
;;; Themed under var/ so its three state files don't drop at the
;;; repo root.
(use-package transient
  :ensure nil
  :custom
  (transient-history-file (jotain-var-file "transient/history.el"))
  (transient-values-file  (jotain-var-file "transient/values.el"))
  (transient-levels-file  (jotain-var-file "transient/levels.el")))

;;; @doc Fringe indicators for added/changed/removed lines in the buffer
;;; you're editing. `diff-hl-flydiff-mode` updates pre-save so the
;;; indicators reflect uncommitted edits, not just the last save.
(use-package diff-hl
  :after magit
  :demand t
  :functions (diff-hl-flydiff-mode diff-hl-magit-post-refresh diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  (diff-hl-side 'left)
  :hook
  ((after-init . global-diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh  . ignore)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Live, pre-save diff indicators.
  (diff-hl-flydiff-mode 1))

;;; @doc Built-in conflict-marker editor. Custom prefix C-c ^ groups
;;; upper/lower/next/prev so resolving merges doesn't require
;;; scrolling through the smerge menu.
(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

;;; @doc Built-in interactive diff. Configured with `plain` window setup
;;; so the control panel doesn't pop a separate frame, plus
;;; whitespace-ignoring diffs for less merge noise.
(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w")
  (ediff-custom-diff-options "-u")
  (ediff-merge-revisions-with-ancestor t)
  :config
  (setopt ediff-control-frame-parameters
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

(provide 'init-vc)
;;; init-vc.el ends here
