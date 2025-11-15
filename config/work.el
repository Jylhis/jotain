;;; work.el --- Work management and automation -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic work management module that integrates various tools and workflows:
;; - Worktree management with JIRA integration (work-manager)
;; - Worktime tracking (future)
;; - Task automation (future)
;; - Project-specific workflows (future)
;;
;; This module serves as the central hub for work-related functionality,
;; providing a consistent interface for managing development workflows.

;;; Code:

;;; Worktree Management

(use-package work-manager
  :commands (work-manager-new
             work-manager-new-from-jira
             work-manager-load
             work-manager-remove
             work-manager-list
             work-manager-sync-registry)
  :custom
  ;; Base directory for all worktrees
  (work-manager-base-directory (expand-file-name "~/worktrees")
   "Directory where all managed worktrees are created")

  ;; Registry file location
  (work-manager-registry-file
   (expand-file-name "worktree-registry.json" user-emacs-directory)
   "JSON file tracking all managed worktrees")

  ;; Branch type options
  (work-manager-branch-types
   '("feature" "bugfix" "hotfix" "refactor" "docs" "test" "chore")
   "Valid branch type prefixes for worktree branches")

  ;; Default branch type
  (work-manager-default-branch-type "feature"
   "Default branch type when creating new worktrees")

  ;; JIRA integration
  (work-manager-jira-enabled t
   "Enable JIRA integration for issue-based workflow")

  (work-manager-auto-fetch-jira t
   "Automatically fetch JIRA issues when needed")

  :bind (("C-c w n" . work-manager-new)
         ("C-c w j" . work-manager-new-from-jira)
         ("C-c w l" . work-manager-load)
         ("C-c w r" . work-manager-remove)
         ("C-c w L" . work-manager-list)
         ("C-c w s" . work-manager-sync-registry))

  :config
  ;; Ensure base directory exists
  (unless (file-exists-p work-manager-base-directory)
    (make-directory work-manager-base-directory t)))

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
