;;; test-helper.el --- Test utilities for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Common test utilities, environment isolation, and helper functions
;; for the Jotain test suite.

;;; Code:

(require 'ert)

;; Add elisp directory to load path for testing
(defvar jotain-test-root-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the test suite.")

(defvar jotain-elisp-dir
  (expand-file-name "../elisp" jotain-test-root-dir)
  "Directory containing Jotain elisp modules.")

;; Add elisp directory to load-path
(add-to-list 'load-path jotain-elisp-dir)

;; Test environment isolation
(defvar jotain-test--original-env nil
  "Store original environment for cleanup.")

(defun jotain-test-setup ()
  "Set up test environment with isolation."
  ;; Store original values
  (setq jotain-test--original-env
        (list :load-path load-path
              :features features))

  ;; Ensure clean environment (only if variables are bound)
  (when (boundp 'jotain-dev-mode)
    (setq jotain-dev-mode nil))
  (when (boundp 'jotain-modules)
    (setq jotain-modules '())))

(defun jotain-test-teardown ()
  "Clean up test environment."
  ;; Restore if needed
  (when jotain-test--original-env
    (setq jotain-test--original-env nil)))

;; Module loading utilities
(defun jotain-test-module-exists-p (module)
  "Check if MODULE file exists in elisp directory.
MODULE should be a symbol like 'jotain-core."
  (let ((module-file (expand-file-name
                      (format "%s.el" module)
                      jotain-elisp-dir)))
    (file-exists-p module-file)))

(defun jotain-test-module-loadable-p (module)
  "Check if MODULE can be loaded without errors.
MODULE should be a symbol like 'jotain-core."
  (condition-case err
      (progn
        (require module)
        t)
    (error
     (message "Failed to load module %s: %s" module err)
     nil)))

;; Common assertions
(defun jotain-test-assert-variable-bound (var)
  "Assert that variable VAR is bound."
  (should (boundp var)))

(defun jotain-test-assert-function-defined (func)
  "Assert that function FUNC is defined."
  (should (fboundp func)))

(defun jotain-test-assert-feature-loaded (feature)
  "Assert that FEATURE is loaded."
  (should (featurep feature)))

(defun jotain-test-assert-emacs-version (major minor)
  "Assert Emacs version is at least MAJOR.MINOR."
  (should (or (> emacs-major-version major)
              (and (= emacs-major-version major)
                   (>= emacs-minor-version minor)))))

;; Environment utilities
(defun jotain-test-with-env (var value body-fn)
  "Execute BODY-FN with environment variable VAR set to VALUE."
  (let ((original-value (getenv var)))
    (unwind-protect
        (progn
          (setenv var value)
          (funcall body-fn))
      ;; Restore original value
      (if original-value
          (setenv var original-value)
        (setenv var nil)))))

;; Minimal mocking support
(defmacro jotain-test-with-mock (func-name replacement &rest body)
  "Execute BODY with FUNC-NAME temporarily replaced by REPLACEMENT."
  (declare (indent 2))
  `(cl-letf (((symbol-function ',func-name) ,replacement))
     ,@body))

;; Test statistics
(defvar jotain-test-start-time nil
  "Start time for test suite.")

(defun jotain-test-start-timer ()
  "Record test suite start time."
  (setq jotain-test-start-time (current-time)))

(defun jotain-test-elapsed-time ()
  "Return elapsed time since test suite started."
  (if jotain-test-start-time
      (float-time (time-subtract (current-time) jotain-test-start-time))
    0.0))

;; Batch mode helpers
(defun jotain-test-batch-message (format-string &rest args)
  "Print message in batch mode with FORMAT-STRING and ARGS."
  (when noninteractive
    (apply #'message format-string args)))

;; Initialize test environment on load
(jotain-test-setup)

(provide 'test-helper)
;;; test-helper.el ends here
