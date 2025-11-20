;;; test-smoke.el --- Ultra-fast smoke tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Critical smoke tests that must pass before running full test suite.
;; These tests should complete in under 1 second total.
;; No heavy loading, no filesystem operations, no package loading.

;;; Code:

(require 'ert)

;;; Emacs Environment

(ert-deftest test-smoke/emacs-version-minimum ()
  "Test that Emacs version meets minimum requirement (29.1+)."
  :tags '(smoke critical)
  (should (version<= "29.1" emacs-version)))

(ert-deftest test-smoke/lexical-binding ()
  "Test lexical binding is enabled."
  :tags '(smoke critical)
  (should lexical-binding))

;;; Directory Structure (no I/O, just checks)

(ert-deftest test-smoke/elisp-directory-exists ()
  "Test that elisp/ directory exists."
  :tags '(smoke critical)
  (let ((elisp-dir (expand-file-name "elisp" user-emacs-directory)))
    (should (file-directory-p elisp-dir))))

;;; Core Files Exist (stat calls only, no loading)

(ert-deftest test-smoke/init-exists ()
  "Test that init.el file exists."
  :tags '(smoke critical)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (should (file-exists-p init-file))))

(ert-deftest test-smoke/early-init-exists ()
  "Test that early-init.el file exists."
  :tags '(smoke critical)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (should (file-exists-p early-init-file))))

;;; Minimal Module Loading (lightweight only)

(ert-deftest test-smoke/platform-module-loads ()
  "Test that platform module loads (lightweight, no dependencies)."
  :tags '(smoke critical)
  (should (require 'platform nil t))
  ;; Test basic functionality without requiring heavy packages
  (should (booleanp platform-android-p))
  (should (booleanp platform-linux-p)))

(ert-deftest test-smoke/can-create-buffer ()
  "Test that basic Emacs buffer operations work."
  :tags '(smoke)
  (with-temp-buffer
    (should (bufferp (current-buffer)))
    (insert "test")
    (should (= (point-max) 5))))

(provide 'test-smoke)
;;; test-smoke.el ends here
