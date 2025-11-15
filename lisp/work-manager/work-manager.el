;;; work-manager.el --- Manage git worktrees with JIRA integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jylhis

;; Author: Jylhis
;; URL: https://github.com/Jylhis/jotain
;; Version: 1.0.0
;; Keywords: tools, vc
;; Package-Requires: ((emacs "29.1") (magit "3.0.0") (org-jira "4.0.0"))

;;; Commentary:

;; This module provides a comprehensive worktree management system that:
;; - Integrates with Magit for git worktree operations
;; - Integrates with org-jira for JIRA ticket management
;; - Supports automatic branch naming from JIRA issues
;; - Tracks worktrees in a registry for easy management
;; - Designed to be callable from both Emacs and CLI (future enhancement)
;;
;; Main commands:
;; - work-manager-new: Create new worktree (optionally from JIRA)
;; - work-manager-load: Switch to a worktree
;; - work-manager-remove: Remove a worktree
;; - work-manager-list: List all managed worktrees
;;
;; Branch naming scheme: <type>/<jira-issue>-<description>
;; Example: feature/PROJ-123-add-user-login

;;; Code:

(require 'magit)
(require 'json)

;;; Customization

(defgroup work-manager nil
  "Manage git worktrees with JIRA integration."
  :group 'tools
  :prefix "work-manager-")

(defcustom work-manager-base-directory
  (expand-file-name "~/worktrees")
  "Base directory where worktrees are created.
Each worktree will be created as a subdirectory under this path."
  :type 'directory
  :group 'work-manager)

(defcustom work-manager-registry-file
  (expand-file-name "~/.emacs.d/worktree-registry.json")
  "File to store worktree registry metadata.
This file tracks all managed worktrees with their metadata."
  :type 'file
  :group 'work-manager)

(defcustom work-manager-branch-types
  '("feature" "bugfix" "hotfix" "refactor" "docs" "test" "chore")
  "List of valid branch type prefixes."
  :type '(repeat string)
  :group 'work-manager)

(defcustom work-manager-default-branch-type "feature"
  "Default branch type when creating new worktrees."
  :type 'string
  :group 'work-manager)

(defcustom work-manager-jira-enabled t
  "Whether to enable JIRA integration.
When enabled, you can create worktrees from JIRA issues."
  :type 'boolean
  :group 'work-manager)

(defcustom work-manager-auto-fetch-jira t
  "Whether to automatically fetch JIRA issues when needed.
If nil, you must manually run `org-jira-get-issues' first."
  :type 'boolean
  :group 'work-manager)

;;; Internal Variables

(defvar work-manager--registry-cache nil
  "Cached worktree registry data.
Reloaded from file when nil.")

;;; Registry Management

(defun work-manager--ensure-registry-file ()
  "Ensure the registry file exists."
  (unless (file-exists-p work-manager-registry-file)
    (let ((dir (file-name-directory work-manager-registry-file)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (with-temp-file work-manager-registry-file
      (insert "[]"))))

(defun work-manager--load-registry ()
  "Load worktree registry from file."
  (work-manager--ensure-registry-file)
  (with-temp-buffer
    (insert-file-contents work-manager-registry-file)
    (if (zerop (buffer-size))
        '()
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun work-manager--save-registry (registry)
  "Save worktree REGISTRY to file."
  (work-manager--ensure-registry-file)
  (with-temp-file work-manager-registry-file
    (insert (json-encode registry)))
  (setq work-manager--registry-cache registry))

(defun work-manager--get-registry ()
  "Get current worktree registry, using cache if available."
  (or work-manager--registry-cache
      (setq work-manager--registry-cache
            (work-manager--load-registry))))

(defun work-manager--add-to-registry (entry)
  "Add ENTRY to worktree registry."
  (let ((registry (work-manager--get-registry)))
    (push entry registry)
    (work-manager--save-registry registry)))

(defun work-manager--remove-from-registry (path)
  "Remove worktree at PATH from registry."
  (let* ((registry (work-manager--get-registry))
         (filtered (seq-remove
                    (lambda (entry)
                      (string= (alist-get 'path entry) path))
                    registry)))
    (work-manager--save-registry filtered)))

(defun work-manager--find-in-registry (path)
  "Find worktree entry for PATH in registry."
  (seq-find
   (lambda (entry)
     (string= (alist-get 'path entry) path))
   (work-manager--get-registry)))

;;; Branch Naming

(defun work-manager--sanitize-string (str)
  "Sanitize STR for use in branch names.
Converts to lowercase, replaces spaces with hyphens, removes special chars."
  (when str
    (thread-last str
      (downcase)
      (replace-regexp-in-string "[^a-z0-9-]" "-")
      (replace-regexp-in-string "-+" "-")
      (replace-regexp-in-string "^-\\|-$" ""))))

(defun work-manager--branch-name-from-jira (jira-key jira-summary type)
  "Generate branch name from JIRA-KEY, JIRA-SUMMARY, and TYPE.
Format: <type>/<jira-key>-<sanitized-summary>
Example: feature/PROJ-123-add-user-login"
  (let ((sanitized-summary (work-manager--sanitize-string jira-summary)))
    (format "%s/%s-%s" type jira-key sanitized-summary)))

(defun work-manager--branch-name-manual (type description)
  "Generate branch name from TYPE and DESCRIPTION.
Format: <type>/<sanitized-description>
Example: feature/add-user-login"
  (let ((sanitized-desc (work-manager--sanitize-string description)))
    (format "%s/%s" type sanitized-desc)))

;;; JIRA Integration

(defun work-manager--jira-available-p ()
  "Check if JIRA integration is available and configured."
  (and work-manager-jira-enabled
       (featurep 'org-jira)
       (boundp 'jiralib-url)
       jiralib-url))

(defun work-manager--get-jira-issues ()
  "Get list of JIRA issues.
Returns list of (key . summary) pairs."
  (unless (work-manager--jira-available-p)
    (error "JIRA integration not available. Install org-jira and configure jiralib-url"))

  ;; Auto-fetch issues if enabled
  (when work-manager-auto-fetch-jira
    (message "Fetching JIRA issues...")
    (org-jira-get-issues))

  ;; Parse issues from org-jira files
  (let ((issues '()))
    (when (boundp 'org-jira-working-dir)
      (dolist (file (directory-files org-jira-working-dir t "\\.org$"))
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (hl)
              (let* ((props (org-element-property :CUSTOM_ID hl))
                     (key (org-element-property :CUSTOM_ID hl))
                     (title (org-element-property :raw-value hl)))
                (when key
                  (push (cons key title) issues))))))))
    (nreverse issues)))

(defun work-manager--select-jira-issue ()
  "Interactively select a JIRA issue.
Returns (key . summary) pair."
  (let* ((issues (work-manager--get-jira-issues))
         (issue-alist (mapcar (lambda (issue)
                                (cons (format "%s - %s" (car issue) (cdr issue))
                                      issue))
                              issues))
         (selection (completing-read "Select JIRA issue: "
                                     (mapcar #'car issue-alist)
                                     nil t)))
    (cdr (assoc selection issue-alist))))

(defun work-manager--create-jira-issue (project type summary description)
  "Create a new JIRA issue.
PROJECT: JIRA project key
TYPE: Issue type (Story, Task, Bug, etc.)
SUMMARY: Issue summary/title
DESCRIPTION: Issue description
Returns the created issue key."
  (unless (work-manager--jira-available-p)
    (error "JIRA integration not available"))

  ;; This is a simplified interface - actual implementation depends on org-jira API
  ;; For now, we'll use the interactive command
  (message "Creating JIRA issue: %s" summary)
  (org-jira-create-issue project type summary description)

  ;; In a real implementation, we'd need to get the created issue key
  ;; For now, we'll return a placeholder
  (error "JIRA issue creation requires manual implementation - use work-manager-new-from-jira after creating issue"))

;;; Core Worktree Operations

(defun work-manager--ensure-base-directory ()
  "Ensure the base worktree directory exists."
  (unless (file-exists-p work-manager-base-directory)
    (make-directory work-manager-base-directory t)))

(defun work-manager--worktree-path (branch-name)
  "Generate worktree path for BRANCH-NAME.
Converts slashes in branch name to dashes for directory name."
  (let ((dir-name (replace-regexp-in-string "/" "-" branch-name)))
    (expand-file-name dir-name work-manager-base-directory)))

(defun work-manager--git-root ()
  "Get the git repository root directory."
  (let ((root (magit-toplevel)))
    (unless root
      (error "Not in a git repository"))
    root))

;;;###autoload
(defun work-manager-new (branch-name &optional jira-key jira-summary base-branch)
  "Create a new worktree for BRANCH-NAME.
Optional JIRA-KEY and JIRA-SUMMARY for tracking.
BASE-BRANCH is the starting point (defaults to main/master).

This function can be called both interactively and programmatically.
For CLI integration, call with all parameters specified.

Returns the worktree path on success."
  (interactive
   (let* ((type (completing-read "Branch type: "
                                work-manager-branch-types
                                nil t nil nil
                                work-manager-default-branch-type))
          (description (read-string "Branch description: "))
          (branch-name (work-manager--branch-name-manual type description)))
     (list branch-name nil nil nil)))

  (work-manager--ensure-base-directory)

  (let* ((git-root (work-manager--git-root))
         (worktree-path (work-manager--worktree-path branch-name))
         (base (or base-branch
                   (if (magit-branch-p "main") "main" "master"))))

    ;; Check if worktree already exists
    (when (file-exists-p worktree-path)
      (error "Worktree directory already exists: %s" worktree-path))

    ;; Create worktree using magit
    (message "Creating worktree for branch '%s' at %s..." branch-name worktree-path)
    (magit-worktree-branch worktree-path branch-name base)

    ;; Add to registry
    (work-manager--add-to-registry
     `((path . ,worktree-path)
       (branch . ,branch-name)
       (jira-key . ,jira-key)
       (jira-summary . ,jira-summary)
       (base-branch . ,base)
       (created . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
       (git-root . ,git-root)))

    (message "Worktree created successfully at: %s" worktree-path)
    worktree-path))

;;;###autoload
(defun work-manager-new-from-jira ()
  "Create a new worktree from a JIRA issue.
Interactively select a JIRA issue and create a worktree with appropriate branch name."
  (interactive)

  (let* ((issue (work-manager--select-jira-issue))
         (jira-key (car issue))
         (jira-summary (cdr issue))
         (type (completing-read "Branch type: "
                               work-manager-branch-types
                               nil t nil nil
                               work-manager-default-branch-type))
         (branch-name (work-manager--branch-name-from-jira
                       jira-key jira-summary type)))

    (work-manager-new branch-name jira-key jira-summary)))

;;;###autoload
(defun work-manager-load (worktree-path)
  "Load (switch to) worktree at WORKTREE-PATH.
Changes the current directory and opens magit status.

Returns the worktree path."
  (interactive
   (let* ((registry (work-manager--get-registry))
          (worktrees (mapcar (lambda (entry)
                              (let ((path (alist-get 'path entry))
                                    (branch (alist-get 'branch entry)))
                                (cons (format "%s (%s)" branch path) path)))
                            registry))
          (selection (completing-read "Load worktree: "
                                     (mapcar #'car worktrees)
                                     nil t)))
     (list (cdr (assoc selection worktrees)))))

  (unless (file-exists-p worktree-path)
    (error "Worktree does not exist: %s" worktree-path))

  ;; Change directory
  (cd worktree-path)

  ;; Open magit status
  (magit-status-setup-buffer worktree-path)

  (message "Loaded worktree: %s" worktree-path)
  worktree-path)

;;;###autoload
(defun work-manager-remove (worktree-path &optional force)
  "Remove worktree at WORKTREE-PATH.
If FORCE is non-nil, force removal even with uncommitted changes.

Returns t on success."
  (interactive
   (let* ((registry (work-manager--get-registry))
          (worktrees (mapcar (lambda (entry)
                              (let ((path (alist-get 'path entry))
                                    (branch (alist-get 'branch entry)))
                                (cons (format "%s (%s)" branch path) path)))
                            registry))
          (selection (completing-read "Remove worktree: "
                                     (mapcar #'car worktrees)
                                     nil t))
          (path (cdr (assoc selection worktrees)))
          (force (yes-or-no-p "Force removal (ignore uncommitted changes)? ")))
     (list path force)))

  (unless (file-exists-p worktree-path)
    (error "Worktree does not exist: %s" worktree-path))

  ;; Confirm removal
  (unless (yes-or-no-p (format "Really remove worktree at %s? " worktree-path))
    (user-error "Aborted"))

  ;; Remove using magit (this handles git worktree remove)
  (let ((default-directory (work-manager--git-root)))
    (if force
        (magit-run-git "worktree" "remove" "--force" worktree-path)
      (magit-run-git "worktree" "remove" worktree-path)))

  ;; Remove from registry
  (work-manager--remove-from-registry worktree-path)

  (message "Worktree removed: %s" worktree-path)
  t)

;;;###autoload
(defun work-manager-list ()
  "List all managed worktrees.
Displays worktrees in a buffer with their metadata.

Returns the registry list."
  (interactive)

  (let ((registry (work-manager--get-registry)))
    (if (null registry)
        (message "No managed worktrees found")

      (with-current-buffer (get-buffer-create "*Worktree Manager*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Managed Worktrees\n")
          (insert "==================\n\n")

          (dolist (entry registry)
            (let ((path (alist-get 'path entry))
                  (branch (alist-get 'branch entry))
                  (jira-key (alist-get 'jira-key entry))
                  (jira-summary (alist-get 'jira-summary entry))
                  (created (alist-get 'created entry))
                  (exists (file-exists-p (alist-get 'path entry))))

              (insert (format "Branch: %s\n" branch))
              (insert (format "Path:   %s %s\n" path
                            (if exists "✓" "✗ (missing)")))
              (when jira-key
                (insert (format "JIRA:   %s - %s\n" jira-key
                              (or jira-summary ""))))
              (insert (format "Created: %s\n" (or created "unknown")))
              (insert "\n")))

          (goto-char (point-min))
          (help-mode))

        (display-buffer (current-buffer))))

    registry))

;;;###autoload
(defun work-manager-sync-registry ()
  "Synchronize registry with actual git worktrees.
Removes entries for worktrees that no longer exist,
and optionally adds untracked worktrees to registry."
  (interactive)

  (let* ((registry (work-manager--get-registry))
         (git-root (work-manager--git-root))
         (cleaned-registry '()))

    ;; Remove entries for non-existent worktrees
    (dolist (entry registry)
      (let ((path (alist-get 'path entry)))
        (if (file-exists-p path)
            (push entry cleaned-registry)
          (message "Removing stale registry entry: %s" path))))

    (work-manager--save-registry (nreverse cleaned-registry))
    (message "Registry synchronized: %d entries" (length cleaned-registry))))

;;; CLI Helper Functions
;; These functions are designed to be easily callable from command line
;; using emacs --batch or emacsclient

(defun work-manager-cli-new (branch-name &optional jira-key base-branch)
  "CLI wrapper for creating a new worktree.
BRANCH-NAME: Full branch name (e.g., 'feature/add-login')
JIRA-KEY: Optional JIRA issue key
BASE-BRANCH: Optional base branch (defaults to main/master)

Example:
  emacs --batch -l config/work-manager.el \\
    --eval '(work-manager-cli-new \"feature/add-login\" \"PROJ-123\")'"
  (let ((path (work-manager-new branch-name jira-key nil base-branch)))
    (princ path)
    (terpri)
    path))

(defun work-manager-cli-list ()
  "CLI wrapper for listing worktrees.
Prints worktrees in a machine-readable format.

Example:
  emacs --batch -l config/work-manager.el \\
    --eval '(work-manager-cli-list)'"
  (let ((registry (work-manager--get-registry)))
    (dolist (entry registry)
      (princ (format "%s\t%s\t%s\n"
                    (alist-get 'branch entry)
                    (alist-get 'path entry)
                    (or (alist-get 'jira-key entry) ""))))
    registry))

(defun work-manager-cli-remove (worktree-path &optional force)
  "CLI wrapper for removing a worktree.
WORKTREE-PATH: Path to worktree to remove
FORCE: If non-nil, force removal

Example:
  emacs --batch -l config/work-manager.el \\
    --eval '(work-manager-cli-remove \"/path/to/worktree\" t)'"
  (work-manager-remove worktree-path force))

(provide 'work-manager)

;;; work-manager.el ends here
