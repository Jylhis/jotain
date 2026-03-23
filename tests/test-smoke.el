;;; test-smoke.el --- Ultra-fast smoke tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Critical smoke tests that must pass before running full test suite.
;; These tests should complete in under 1 second total.
;; No heavy loading, no filesystem operations, no package loading.

;;; Code:

(require 'ert)

;;; Emacs Environment

(ert-deftest test-smoke/emacs-version-minimum ()
  "Test that Emacs version meets minimum requirement (30+)."
  :tags '(smoke critical)
  (should (version<= "30" emacs-version)))

(ert-deftest test-smoke/lexical-binding ()
  "Test lexical binding is enabled."
  :tags '(smoke critical)
  (should lexical-binding))

;;; Directory Structure (no I/O, just checks)

(ert-deftest test-smoke/lisp-directory-exists ()
  "Test that lisp/ directory exists."
  :tags '(smoke critical)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (should (file-directory-p lisp-dir))))

(ert-deftest test-smoke/modules-directory-exists ()
  "Test that modules/ directory exists."
  :tags '(smoke critical)
  (let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
    (should (file-directory-p modules-dir))))

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

(ert-deftest test-smoke/can-create-buffer ()
  "Test that basic Emacs buffer operations work."
  :tags '(smoke)
  (with-temp-buffer
    (should (bufferp (current-buffer)))
    (insert "test")
    (should (= (point-max) 5))))

;;; Module Features Present

(ert-deftest test-smoke/jotain-defaults-loadable ()
  "Test that jotain-defaults module can be loaded."
  :tags '(smoke)
  (require 'jotain-defaults)
  (should (featurep 'jotain-defaults)))

(ert-deftest test-smoke/jotain-platform-loadable ()
  "Test that jotain-platform module can be loaded."
  :tags '(smoke)
  (require 'jotain-platform)
  (should (featurep 'jotain-platform)))

;;; Runtime Dependencies (Critical)

(ert-deftest test-smoke/tree-sitter-dir-set ()
  "Test that TREE_SITTER_DIR is configured (critical for syntax highlighting)."
  :tags '(smoke critical)
  (let ((ts-dir (getenv "TREE_SITTER_DIR")))
    (should ts-dir)
    (should (not (string-empty-p ts-dir)))))

(ert-deftest test-smoke/ripgrep-available ()
  "Test that ripgrep is available (critical for search functionality)."
  :tags '(smoke critical)
  (should (executable-find "rg")))


(provide 'test-smoke)
;;; test-smoke.el ends here
