;;; test-worktree-manager.el --- Tests for worktree-manager -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jylhis

;;; Commentary:

;; Tests for the worktree-manager module.

;;; Code:

(require 'ert)
(require 'worktree-manager)
(require 'json)

;;; Helper Functions

(defmacro with-temp-registry (&rest body)
  "Execute BODY with a temporary registry file."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "worktree-test-" t))
          (worktree-manager-registry-file
           (expand-file-name "registry.json" temp-dir))
          (worktree-manager--registry-cache nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p temp-dir)
         (delete-directory temp-dir t)))))

;;; Registry Management Tests

(ert-deftest test-worktree-manager/registry-creation ()
  "Test registry file creation."
  :tags '(unit fast worktree-manager)
  (with-temp-registry
    (worktree-manager--ensure-registry-file)
    (should (file-exists-p worktree-manager-registry-file))
    (should (equal (worktree-manager--load-registry) '()))))

(ert-deftest test-worktree-manager/registry-save-load ()
  "Test saving and loading registry."
  :tags '(unit fast worktree-manager)
  (with-temp-registry
    (let ((test-data '(((path . "/path/to/worktree")
                       (branch . "feature/test")
                       (created . "2025-01-01")))))
      (worktree-manager--save-registry test-data)
      (setq worktree-manager--registry-cache nil)
      (let ((loaded (worktree-manager--load-registry)))
        (should (equal (length loaded) 1))
        (should (equal (alist-get 'path (car loaded)) "/path/to/worktree"))
        (should (equal (alist-get 'branch (car loaded)) "feature/test"))))))

(ert-deftest test-worktree-manager/registry-add ()
  "Test adding entry to registry."
  :tags '(unit fast worktree-manager)
  (with-temp-registry
    (worktree-manager--add-to-registry
     '((path . "/path/1") (branch . "feature/one")))
    (worktree-manager--add-to-registry
     '((path . "/path/2") (branch . "feature/two")))

    (let ((registry (worktree-manager--get-registry)))
      (should (equal (length registry) 2))
      ;; Most recent should be first
      (should (equal (alist-get 'path (car registry)) "/path/2")))))

(ert-deftest test-worktree-manager/registry-remove ()
  "Test removing entry from registry."
  :tags '(unit fast worktree-manager)
  (with-temp-registry
    (worktree-manager--add-to-registry
     '((path . "/path/1") (branch . "feature/one")))
    (worktree-manager--add-to-registry
     '((path . "/path/2") (branch . "feature/two")))

    (worktree-manager--remove-from-registry "/path/1")

    (let ((registry (worktree-manager--get-registry)))
      (should (equal (length registry) 1))
      (should (equal (alist-get 'path (car registry)) "/path/2")))))

(ert-deftest test-worktree-manager/registry-find ()
  "Test finding entry in registry."
  :tags '(unit fast worktree-manager)
  (with-temp-registry
    (worktree-manager--add-to-registry
     '((path . "/path/1") (branch . "feature/one")))
    (worktree-manager--add-to-registry
     '((path . "/path/2") (branch . "feature/two")))

    (let ((entry (worktree-manager--find-in-registry "/path/1")))
      (should entry)
      (should (equal (alist-get 'branch entry) "feature/one")))

    (should-not (worktree-manager--find-in-registry "/nonexistent"))))

;;; Branch Naming Tests

(ert-deftest test-worktree-manager/sanitize-string ()
  "Test string sanitization for branch names."
  :tags '(unit fast worktree-manager)
  (should (equal (worktree-manager--sanitize-string "Simple Test")
                "simple-test"))
  (should (equal (worktree-manager--sanitize-string "Test with   spaces")
                "test-with-spaces"))
  (should (equal (worktree-manager--sanitize-string "Test@#$%Special!Chars")
                "test-special-chars"))
  (should (equal (worktree-manager--sanitize-string "Multiple---Dashes")
                "multiple-dashes"))
  (should (equal (worktree-manager--sanitize-string "-Leading-Trailing-")
                "leading-trailing"))
  (should (equal (worktree-manager--sanitize-string "UPPERCASE")
                "uppercase")))

(ert-deftest test-worktree-manager/branch-name-from-jira ()
  "Test branch name generation from JIRA issue."
  :tags '(unit fast worktree-manager)
  (should (equal (worktree-manager--branch-name-from-jira
                 "PROJ-123" "Add User Login" "feature")
                "feature/PROJ-123-add-user-login"))

  (should (equal (worktree-manager--branch-name-from-jira
                 "BUG-456" "Fix Critical Bug!" "bugfix")
                "bugfix/BUG-456-fix-critical-bug"))

  (should (equal (worktree-manager--branch-name-from-jira
                 "HOTFIX-789" "Security Issue" "hotfix")
                "hotfix/HOTFIX-789-security-issue")))

(ert-deftest test-worktree-manager/branch-name-manual ()
  "Test manual branch name generation."
  :tags '(unit fast worktree-manager)
  (should (equal (worktree-manager--branch-name-manual
                 "feature" "Add User Login")
                "feature/add-user-login"))

  (should (equal (worktree-manager--branch-name-manual
                 "bugfix" "Fix Bug")
                "bugfix/fix-bug"))

  (should (equal (worktree-manager--branch-name-manual
                 "refactor" "Improve Performance")
                "refactor/improve-performance")))

;;; Path Generation Tests

(ert-deftest test-worktree-manager/worktree-path ()
  "Test worktree path generation."
  :tags '(unit fast worktree-manager)
  (let ((worktree-manager-base-directory "/base"))
    (should (equal (worktree-manager--worktree-path "feature/test")
                  "/base/feature-test"))

    (should (equal (worktree-manager--worktree-path "bugfix/fix-bug")
                  "/base/bugfix-fix-bug"))

    (should (equal (worktree-manager--worktree-path "hotfix/security")
                  "/base/hotfix-security"))))

;;; Configuration Tests

(ert-deftest test-worktree-manager/default-configuration ()
  "Test default configuration values."
  :tags '(unit fast worktree-manager smoke)
  (should worktree-manager-base-directory)
  (should worktree-manager-registry-file)
  (should worktree-manager-branch-types)
  (should (member "feature" worktree-manager-branch-types))
  (should (member "bugfix" worktree-manager-branch-types))
  (should (member "hotfix" worktree-manager-branch-types))
  (should (equal worktree-manager-default-branch-type "feature")))

;;; JIRA Integration Tests

(ert-deftest test-worktree-manager/jira-available-check ()
  "Test JIRA availability check."
  :tags '(unit fast worktree-manager)
  ;; Should return nil if org-jira not loaded or jiralib-url not set
  (let ((worktree-manager-jira-enabled t))
    (if (and (featurep 'org-jira)
             (boundp 'jiralib-url)
             jiralib-url)
        (should (worktree-manager--jira-available-p))
      (should-not (worktree-manager--jira-available-p)))))

;;; CLI Function Tests

(ert-deftest test-worktree-manager/cli-functions-exist ()
  "Test that CLI helper functions exist."
  :tags '(unit fast worktree-manager smoke)
  (should (fboundp 'worktree-manager-cli-new))
  (should (fboundp 'worktree-manager-cli-list))
  (should (fboundp 'worktree-manager-cli-remove)))

;;; Integration Tests
;; These tests require git and are slower

(ert-deftest test-worktree-manager/sync-registry-stale-entries ()
  "Test registry sync removes stale entries."
  :tags '(integration slow worktree-manager filesystem)
  (with-temp-registry
    (let ((temp-dir-1 (make-temp-file "worktree-1-" t))
          (temp-dir-2 (make-temp-file "worktree-2-" t)))
      (unwind-protect
          (progn
            ;; Add two entries
            (worktree-manager--add-to-registry
             `((path . ,temp-dir-1) (branch . "feature/one")))
            (worktree-manager--add-to-registry
             `((path . ,temp-dir-2) (branch . "feature/two")))

            ;; Verify both exist
            (should (equal (length (worktree-manager--get-registry)) 2))

            ;; Delete one directory
            (delete-directory temp-dir-1 t)

            ;; Sync should remove the stale entry
            ;; Note: This will fail if not in a git repo, so we skip it in that case
            (when (ignore-errors (magit-toplevel))
              (worktree-manager-sync-registry)
              (should (equal (length (worktree-manager--get-registry)) 1))
              (should (equal (alist-get 'path (car (worktree-manager--get-registry)))
                           temp-dir-2))))

        ;; Cleanup
        (when (file-exists-p temp-dir-1)
          (delete-directory temp-dir-1 t))
        (when (file-exists-p temp-dir-2)
          (delete-directory temp-dir-2 t))))))

(provide 'test-worktree-manager)

;;; test-worktree-manager.el ends here
