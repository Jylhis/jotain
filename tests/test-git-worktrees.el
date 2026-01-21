;;; test-git-worktrees.el --- Test Magit worktree configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that Magit is configured to show worktrees in the status buffer.

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Load the git module
(require 'git)

(ert-deftest test-magit-worktrees-hook ()
  "Test that magit-insert-worktrees is in magit-status-sections-hook."
  :tags '(integration)

  ;; Force load magit to trigger :config
  (require 'magit)

  ;; Verify the hook is present
  ;; We expect it to be at the end (append = t), but presence is enough for this test
  (should (memq 'magit-insert-worktrees magit-status-sections-hook)))

(provide 'test-git-worktrees)
;;; test-git-worktrees.el ends here
