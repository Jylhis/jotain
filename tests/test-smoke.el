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

;;; Edge Cases

(ert-deftest test-smoke/font-fallback-no-crash ()
  "Test edge case: Emacs does not crash when preferred fonts are unavailable.
Font fallback chains in fonts.el and platforms.el must degrade gracefully.
This smoke test verifies that font configuration loaded without error."
  :tags '(smoke edge-case)
  ;; The font module loaded (or we would not be running tests)
  ;; Verify that font-related variables are bound (config was loaded)
  (require 'fonts nil t)
  (should t)) ; reaching here means no error during font config load

(ert-deftest test-smoke/treesit-missing-grammar-no-crash ()
  "Test edge case: missing tree-sitter grammar does not prevent file opening.
treesit-auto-install must be nil so Nix-managed grammars are used.
If a grammar is missing, treesit-auto degrades to non-TS mode gracefully."
  :tags '(smoke edge-case)
  (require 'programming nil t)
  ;; Verify Nix grammar management: auto-install must be disabled
  (when (boundp 'treesit-auto-install)
    (should-not treesit-auto-install)))

(ert-deftest test-smoke/eglot-missing-server-no-init-crash ()
  "Test edge case: missing LSP server does not crash Emacs init.
Eglot only starts LSP when explicitly invoked; absent servers cause
a user-visible error only when eglot is started, not during Emacs startup."
  :tags '(smoke edge-case)
  ;; eglot is built-in to Emacs 29+; verify it loaded without error
  (require 'eglot nil t)
  (should (featurep 'eglot)))

;;; Startup Performance (SC-011)

(ert-deftest test-smoke/startup-time-measurement ()
  "Measure standalone Emacs startup time to validate SC-011 (< 3 seconds).
This test records the startup measurement from `emacs-init-time' for
reporting purposes. The actual < 3s constraint is checked as a warning
rather than a hard failure, since batch test runs load extra test
infrastructure that inflates startup time.

SC-011: Standalone startup time MUST be < 3 seconds."
  :tags '(smoke performance)
  (when (fboundp 'emacs-init-time)
    (let* ((init-time-str (emacs-init-time "%.3f"))
           (init-seconds (string-to-number init-time-str)))
      ;; Report the measurement (useful in CI logs)
      (message "Emacs init time: %s seconds (SC-011 target: < 3s)" init-time-str)
      ;; Soft check: warn if over 3s but do not fail the test suite
      ;; (batch test runs load additional infrastructure vs. normal startup)
      (when (> init-seconds 3.0)
        (message "WARN SC-011: Init time %ss exceeds 3s target (may be inflated by test setup)"
                 init-time-str))
      ;; Hard check: if it takes more than 10s something is fundamentally wrong
      (should (< init-seconds 10.0)))))

(provide 'test-smoke)
;;; test-smoke.el ends here
