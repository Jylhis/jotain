;;; test-runner.el --- Test runner for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Simple test runner with batch mode support and clean exit codes.
;; Handles test discovery and execution for CI/CD integration.

;;; Code:

(require 'ert)
(require 'test-helper)

;; Test discovery
(defvar jotain-test-files
  '("test-smoke.el")
  "List of test files to run.")

(defun jotain-test-discover ()
  "Discover and load all test files."
  (jotain-test-batch-message "Discovering tests...")
  (dolist (test-file jotain-test-files)
    (let ((test-path (expand-file-name test-file jotain-test-root-dir)))
      (if (file-exists-p test-path)
          (progn
            (jotain-test-batch-message "  Loading: %s" test-file)
            (load test-path nil t))
        (jotain-test-batch-message "  WARNING: Test file not found: %s" test-file))))
  (jotain-test-batch-message "Test discovery complete."))

(defun jotain-test-run-all ()
  "Run all discovered tests."
  (jotain-test-discover)
  (jotain-test-batch-message "")
  (jotain-test-batch-message "========================================")
  (jotain-test-batch-message "Running Jotain Test Suite")
  (jotain-test-batch-message "========================================")
  (jotain-test-batch-message "")
  (ert-run-tests-batch-and-exit))

(defun jotain-test-run-interactive ()
  "Run all tests interactively."
  (interactive)
  (jotain-test-discover)
  (ert-run-tests-interactively t))

(defun jotain-test-run-selector (selector)
  "Run tests matching SELECTOR.
SELECTOR can be:
  - t (run all tests)
  - :new (run tests that have not been run)
  - :failed (run tests that failed)
  - :passed (run tests that passed)
  - a string (run tests matching regexp)
  - a symbol (run specific test)"
  (interactive
   (list (read-string "Test selector (default: all): " nil nil "t")))
  (jotain-test-discover)
  (let ((selector-expr (if (stringp selector)
                           (if (string= selector "t")
                               t
                             selector)
                         selector)))
    (if noninteractive
        (ert-run-tests-batch selector-expr)
      (ert-run-tests-interactively selector-expr))))

;; Batch mode entry point
(defun jotain-test-batch ()
  "Entry point for batch mode testing."
  (jotain-test-start-timer)
  (jotain-test-batch-message "Jotain Test Suite")
  (jotain-test-batch-message "Platform: %s" system-type)
  (jotain-test-batch-message "Emacs: %s.%s"
                             emacs-major-version
                             emacs-minor-version)
  (jotain-test-batch-message "")
  (jotain-test-run-all))

;; Statistics and reporting
(defun jotain-test-stats ()
  "Display test statistics."
  (interactive)
  (jotain-test-discover)
  (let ((tests (ert-select-tests t t)))
    (message "Total tests defined: %d" (length tests))
    (dolist (test tests)
      (message "  - %s" (ert-test-name test)))))

(defun jotain-test-list ()
  "List all available tests."
  (interactive)
  (jotain-test-discover)
  (let ((tests (ert-select-tests t t)))
    (if tests
        (progn
          (message "Available tests (%d):" (length tests))
          (dolist (test tests)
            (let* ((test-name (ert-test-name test))
                   (doc (ert-test-documentation test)))
              (message "")
              (message "  %s" test-name)
              (when doc
                (message "    %s" doc)))))
      (message "No tests found."))))

;; Exit codes for CI/CD
(defun jotain-test-exit-code (stats)
  "Return appropriate exit code based on test STATS.
Returns 0 for success, 1 for failures."
  (if (zerop (ert-stats-completed-unexpected stats))
      0
    1))

;; Quick test runner commands
(defun jotain-test-smoke ()
  "Run only smoke tests."
  (interactive)
  (jotain-test-run-selector "^test-smoke/"))

(defun jotain-test-failed ()
  "Re-run failed tests."
  (interactive)
  (jotain-test-run-selector :failed))

;; Auto-run in batch mode if loaded with --batch
(when noninteractive
  (jotain-test-batch))

(provide 'test-runner)
;;; test-runner.el ends here
