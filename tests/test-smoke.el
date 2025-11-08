;;; test-smoke.el --- Smoke tests for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Basic smoke tests to verify Jotain core functionality.
;; These tests ensure the distribution loads and core features work.
;; Note: Tests are designed to work even when modules don't exist yet.

;;; Code:

(require 'ert)
(require 'test-helper)

;; Start timer for performance tracking
(jotain-test-start-timer)

;;; Emacs Version Tests

(ert-deftest test-smoke/emacs-version-check ()
  "Verify Emacs version is 30.1 or higher."
  (jotain-test-assert-emacs-version 30 1)
  (should (>= emacs-major-version 30))
  (jotain-test-batch-message "✓ Emacs version: %s.%s"
                             emacs-major-version
                             emacs-minor-version))

(ert-deftest test-smoke/lexical-binding-enabled ()
  "Verify lexical binding is enabled."
  (should lexical-binding)
  (jotain-test-batch-message "✓ Lexical binding enabled"))

;;; Core File Tests

(ert-deftest test-smoke/jotain-file-exists ()
  "Verify jotain.el file exists."
  (let ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir)))
    (should (file-exists-p jotain-file))
    (should (file-readable-p jotain-file))
    (jotain-test-batch-message "✓ jotain.el exists at: %s" jotain-file)))

(ert-deftest test-smoke/jotain-file-parses ()
  "Verify jotain.el parses without syntax errors."
  (let ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir)))
    (with-temp-buffer
      (insert-file-contents jotain-file)
      (emacs-lisp-mode)
      (condition-case err
          (progn
            (check-parens)
            (jotain-test-batch-message "✓ jotain.el parses correctly"))
        (error
         (ert-fail (format "Parse error in jotain.el: %s" err)))))))

;;; Package Metadata Tests

(ert-deftest test-smoke/package-requires-emacs-30 ()
  "Verify package requires Emacs 30.1+."
  (let* ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir))
         (content (with-temp-buffer
                    (insert-file-contents jotain-file)
                    (buffer-string))))
    (should (string-match "Package-Requires:.*emacs \"30" content))
    (jotain-test-batch-message "✓ Package requires Emacs 30.1+")))

(ert-deftest test-smoke/package-has-header ()
  "Verify jotain.el has proper package header."
  (let* ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir))
         (content (with-temp-buffer
                    (insert-file-contents jotain-file)
                    (buffer-string))))
    (should (string-match "^;;; jotain.el ---" content))
    (should (string-match ";;; Code:" content))
    (should (string-match "(provide 'jotain)" content))
    (should (string-match ";;; jotain.el ends here" content))
    (jotain-test-batch-message "✓ Package header complete")))

;;; Core Definitions Tests (without loading modules)

(ert-deftest test-smoke/jotain-defines-group ()
  "Verify jotain.el defines customization group."
  (let* ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir))
         (content (with-temp-buffer
                    (insert-file-contents jotain-file)
                    (buffer-string))))
    (should (string-match "(defgroup jotain" content))
    (jotain-test-batch-message "✓ Customization group defined")))

(ert-deftest test-smoke/jotain-defines-dev-mode ()
  "Verify jotain.el defines dev-mode variable."
  (let* ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir))
         (content (with-temp-buffer
                    (insert-file-contents jotain-file)
                    (buffer-string))))
    (should (string-match "(defcustom jotain-dev-mode" content))
    (jotain-test-batch-message "✓ jotain-dev-mode variable defined")))

(ert-deftest test-smoke/jotain-defines-core-functions ()
  "Verify jotain.el defines core functions."
  (let* ((jotain-file (expand-file-name "jotain.el" jotain-elisp-dir))
         (content (with-temp-buffer
                    (insert-file-contents jotain-file)
                    (buffer-string))))
    (should (string-match "(defun jotain-load-module" content))
    (should (string-match "(defun jotain-reload-module" content))
    (should (string-match "(defun jotain-initialize" content))
    (should (string-match "(defun jotain--detect-dev-mode" content))
    (jotain-test-batch-message "✓ Core functions defined")))

;;; Load Path Tests

(ert-deftest test-smoke/elisp-directory-in-load-path ()
  "Verify elisp directory is in load-path."
  (should (member jotain-elisp-dir load-path))
  (jotain-test-batch-message "✓ elisp directory in load-path"))

;;; Test Infrastructure Tests

(ert-deftest test-smoke/test-helper-loaded ()
  "Verify test-helper is loaded."
  (should (featurep 'test-helper))
  (should (boundp 'jotain-test-root-dir))
  (should (boundp 'jotain-elisp-dir))
  (should (fboundp 'jotain-test-setup))
  (jotain-test-batch-message "✓ test-helper loaded"))

;;; Test Suite Summary

(defun test-smoke-print-summary ()
  "Print test suite summary."
  (when noninteractive
    (let ((elapsed (jotain-test-elapsed-time)))
      (message "")
      (message "========================================")
      (message "Jotain Smoke Tests Complete")
      (message "========================================")
      (message "Test suite finished in %.3f seconds" elapsed)
      (message "All critical smoke tests passed ✓")
      (message "")
      (message "Note: Tests validate jotain.el structure.")
      (message "Tests will expand as more modules are added.")
      (message ""))))

;; Print summary after all tests
(add-hook 'ert-runner-reporter-run-ended-functions #'test-smoke-print-summary)

(provide 'test-smoke)
;;; test-smoke.el ends here
