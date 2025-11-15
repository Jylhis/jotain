;;; work.el --- Work management and automation -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic work management module that integrates various tools and workflows:
;; - Worktree management with JIRA integration (worktree-manager)
;; - Worktime tracking (future)
;; - Task automation (future)
;; - Project-specific workflows (future)
;;
;; This module serves as the central hub for work-related functionality,
;; providing a consistent interface for managing development workflows.

;;; Code:

;;; Worktree Management

(use-package worktree-manager
  :commands (worktree-manager-new
             worktree-manager-new-from-jira
             worktree-manager-load
             worktree-manager-remove
             worktree-manager-list
             worktree-manager-sync-registry)
  :custom
  ;; Base directory for all worktrees
  (worktree-manager-base-directory (expand-file-name "~/worktrees")
   "Directory where all managed worktrees are created")

  ;; Registry file location
  (worktree-manager-registry-file
   (expand-file-name "worktree-registry.json" user-emacs-directory)
   "JSON file tracking all managed worktrees")

  ;; Branch type options
  (worktree-manager-branch-types
   '("feature" "bugfix" "hotfix" "refactor" "docs" "test" "chore")
   "Valid branch type prefixes for worktree branches")

  ;; Default branch type
  (worktree-manager-default-branch-type "feature"
   "Default branch type when creating new worktrees")

  ;; JIRA integration
  (worktree-manager-jira-enabled t
   "Enable JIRA integration for issue-based workflow")

  (worktree-manager-auto-fetch-jira t
   "Automatically fetch JIRA issues when needed")

  :bind (("C-c w n" . worktree-manager-new)
         ("C-c w j" . worktree-manager-new-from-jira)
         ("C-c w l" . worktree-manager-load)
         ("C-c w r" . worktree-manager-remove)
         ("C-c w L" . worktree-manager-list)
         ("C-c w s" . worktree-manager-sync-registry))

  :config
  ;; Ensure base directory exists
  (unless (file-exists-p worktree-manager-base-directory)
    (make-directory worktree-manager-base-directory t)))

;;; Worktime Tracking
;; Future: Integration with org-clock, org-pomodoro, or custom time tracking

;;; Task Automation
;; Future: Project-specific automation, build triggers, etc.

;;; Keybinding Prefix
;; All work-related commands use the "C-c w" prefix:
;; C-c w n - New worktree (manual)
;; C-c w j - New worktree from JIRA
;; C-c w l - Load (switch to) worktree
;; C-c w r - Remove worktree
;; C-c w L - List all worktrees
;; C-c w s - Sync registry (clean stale entries)

(provide 'work)

;;; work.el ends here
