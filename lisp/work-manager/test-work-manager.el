;;; test-work-manager.el --- Tests for work-manager -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jylhis

;;; Commentary:

;; Tests for the work-manager module.

;;; Code:

(require 'ert)
(require 'work-manager)
(require 'json)

;;; Helper Functions

(defmacro with-temp-registry (&rest body)
  "Execute BODY with a temporary registry file."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "worktree-test-" t))
          (work-manager-registry-file
           (expand-file-name "registry.json" temp-dir))
          (work-manager--registry-cache nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p temp-dir)
         (delete-directory temp-dir t)))))

;;; Registry Management Tests

(ert-deftest test-work-manager/registry-creation ()
  "Test registry file creation."
  :tags '(unit fast work-manager)
  (with-temp-registry
    (work-manager--ensure-registry-file)
    (should (file-exists-p work-manager-registry-file))
    (should (equal (work-manager--load-registry) '()))))

(ert-deftest test-work-manager/registry-save-load ()
  "Test saving and loading registry."
  :tags '(unit fast work-manager)
  (with-temp-registry
    (let ((test-data '(((path . "/path/to/worktree")
                       (branch . "feature/test")
                       (created . "2025-01-01")))))
      (work-manager--save-registry test-data)
      (setq work-manager--registry-cache nil)
      (let ((loaded (work-manager--load-registry)))
        (should (equal (length loaded) 1))
        (should (equal (alist-get 'path (car loaded)) "/path/to/worktree"))
        (should (equal (alist-get 'branch (car loaded)) "feature/test"))))))

(ert-deftest test-work-manager/registry-add ()
  "Test adding entry to registry."
  :tags '(unit fast work-manager)
  (with-temp-registry
    (work-manager--add-to-registry
     '((path . "/path/1") (branch . "feature/one")))
    (work-manager--add-to-registry
     '((path . "/path/2") (branch . "feature/two")))

    (let ((registry (work-manager--get-registry)))
      (should (equal (length registry) 2))
      ;; Most recent should be first
      (should (equal (alist-get 'path (car registry)) "/path/2")))))

(ert-deftest test-work-manager/registry-remove ()
  "Test removing entry from registry."
  :tags '(unit fast work-manager)
  (with-temp-registry
    (work-manager--add-to-registry
     '((path . "/path/1") (branch . "feature/one")))
    (work-manager--add-to-registry
     '((path . "/path/2") (branch . "feature/two")))

    (work-manager--remove-from-registry "/path/1")

    (let ((registry (work-manager--get-registry)))
      (should (equal (length registry) 1))
      (should (equal (alist-get 'path (car registry)) "/path/2")))))

(ert-deftest test-work-manager/registry-find ()
  "Test finding entry in registry."
  :tags '(unit fast work-manager)
  (with-temp-registry
    (work-manager--add-to-registry
     '((path . "/path/1") (branch . "feature/one")))
    (work-manager--add-to-registry
     '((path . "/path/2") (branch . "feature/two")))

    (let ((entry (work-manager--find-in-registry "/path/1")))
      (should entry)
      (should (equal (alist-get 'branch entry) "feature/one")))

    (should-not (work-manager--find-in-registry "/nonexistent"))))

;;; Branch Naming Tests

(ert-deftest test-work-manager/sanitize-string ()
  "Test string sanitization for branch names."
  :tags '(unit fast work-manager)
  (should (equal (work-manager--sanitize-string "Simple Test")
                "simple-test"))
  (should (equal (work-manager--sanitize-string "Test with   spaces")
                "test-with-spaces"))
  (should (equal (work-manager--sanitize-string "Test@#$%Special!Chars")
                "test-special-chars"))
  (should (equal (work-manager--sanitize-string "Multiple---Dashes")
                "multiple-dashes"))
  (should (equal (work-manager--sanitize-string "-Leading-Trailing-")
                "leading-trailing"))
  (should (equal (work-manager--sanitize-string "UPPERCASE")
                "uppercase")))

(ert-deftest test-work-manager/branch-name-from-jira ()
  "Test branch name generation from JIRA issue."
  :tags '(unit fast work-manager)
  (should (equal (work-manager--branch-name-from-jira
                 "PROJ-123" "Add User Login" "feature")
                "feature/PROJ-123-add-user-login"))

  (should (equal (work-manager--branch-name-from-jira
                 "BUG-456" "Fix Critical Bug!" "bugfix")
                "bugfix/BUG-456-fix-critical-bug"))

  (should (equal (work-manager--branch-name-from-jira
                 "HOTFIX-789" "Security Issue" "hotfix")
                "hotfix/HOTFIX-789-security-issue")))

(ert-deftest test-work-manager/branch-name-manual ()
  "Test manual branch name generation."
  :tags '(unit fast work-manager)
  (should (equal (work-manager--branch-name-manual
                 "feature" "Add User Login")
                "feature/add-user-login"))

  (should (equal (work-manager--branch-name-manual
                 "bugfix" "Fix Bug")
                "bugfix/fix-bug"))

  (should (equal (work-manager--branch-name-manual
                 "refactor" "Improve Performance")
                "refactor/improve-performance")))

;;; Path Generation Tests

(ert-deftest test-work-manager/worktree-path ()
  "Test worktree path generation."
  :tags '(unit fast work-manager)
  (let ((work-manager-base-directory "/base"))
    (should (equal (work-manager--worktree-path "feature/test")
                  "/base/feature-test"))

    (should (equal (work-manager--worktree-path "bugfix/fix-bug")
                  "/base/bugfix-fix-bug"))

    (should (equal (work-manager--worktree-path "hotfix/security")
                  "/base/hotfix-security"))))

;;; Configuration Tests

(ert-deftest test-work-manager/default-configuration ()
  "Test default configuration values."
  :tags '(unit fast work-manager smoke)
  (should work-manager-base-directory)
  (should work-manager-registry-file)
  (should work-manager-branch-types)
  (should (member "feature" work-manager-branch-types))
  (should (member "bugfix" work-manager-branch-types))
  (should (member "hotfix" work-manager-branch-types))
  (should (equal work-manager-default-branch-type "feature")))

;;; JIRA Integration Tests

(ert-deftest test-work-manager/jira-available-check ()
  "Test JIRA availability check."
  :tags '(unit fast work-manager)
  ;; Should return nil if org-jira not loaded or jiralib-url not set
  (let ((work-manager-jira-enabled t))
    (if (and (featurep 'org-jira)
             (boundp 'jiralib-url)
             jiralib-url)
        (should (work-manager--jira-available-p))
      (should-not (work-manager--jira-available-p)))))

;;; CLI Function Tests

(ert-deftest test-work-manager/cli-functions-exist ()
  "Test that CLI helper functions exist."
  :tags '(unit fast work-manager smoke)
  (should (fboundp 'work-manager-cli-new))
  (should (fboundp 'work-manager-cli-list))
  (should (fboundp 'work-manager-cli-remove)))

;;; Integration Tests
;; These tests require git and are slower

(ert-deftest test-work-manager/sync-registry-stale-entries ()
  "Test registry sync removes stale entries."
  :tags '(integration slow work-manager filesystem)
  (with-temp-registry
    (let ((temp-dir-1 (make-temp-file "worktree-1-" t))
          (temp-dir-2 (make-temp-file "worktree-2-" t)))
      (unwind-protect
          (progn
            ;; Add two entries
            (work-manager--add-to-registry
             `((path . ,temp-dir-1) (branch . "feature/one")))
            (work-manager--add-to-registry
             `((path . ,temp-dir-2) (branch . "feature/two")))

            ;; Verify both exist
            (should (equal (length (work-manager--get-registry)) 2))

            ;; Delete one directory
            (delete-directory temp-dir-1 t)

            ;; Sync should remove the stale entry
            ;; Note: This will fail if not in a git repo, so we skip it in that case
            (when (ignore-errors (magit-toplevel))
              (work-manager-sync-registry)
              (should (equal (length (work-manager--get-registry)) 1))
              (should (equal (alist-get 'path (car (work-manager--get-registry)))
                           temp-dir-2))))

        ;; Cleanup
        (when (file-exists-p temp-dir-1)
          (delete-directory temp-dir-1 t))
        (when (file-exists-p temp-dir-2)
          (delete-directory temp-dir-2 t))))))

(provide 'test-work-manager)

;;; test-work-manager.el ends here
