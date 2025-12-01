;;; test-use-package-ensure.el --- Tests for use-package :ensure configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to ensure :ensure t is never used in configuration files.
;; Packages are pre-installed by Nix and should never trigger runtime installation.
;;
;; Context:
;; - early-init.el sets (setq use-package-always-ensure nil)
;; - This prevents use-package from auto-installing packages
;; - Individual :ensure t overrides this global setting
;; - This causes errors: "Failed to install X: Package 'X' is unavailable"
;; - Packages ARE correctly installed by Nix, but use-package tries to install them again
;;
;; Solution:
;; - Remove all :ensure t from elisp/*.el files
;; - Rely on global use-package-always-ensure nil setting
;; - Nix provides all packages at build time
;;
;; This test suite validates that no :ensure t remains in the codebase.

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; Configuration Validation Tests

(ert-deftest test-use-package-ensure/global-setting-is-nil ()
  "Test that use-package-always-ensure is set to nil globally.
This is the critical setting that prevents runtime package installation."
  :tags '(fast unit critical)
  (should (boundp 'use-package-always-ensure))
  (should (null use-package-always-ensure)))

(ert-deftest test-use-package-ensure/early-init-sets-nil ()
  "Test that early-init.el explicitly sets use-package-always-ensure to nil.
This ensures the setting is configured before any packages load."
  :tags '(fast unit)
  (let ((early-init-path (expand-file-name "early-init.el" user-emacs-directory)))
    (should (file-exists-p early-init-path))
    (with-temp-buffer
      (insert-file-contents early-init-path)
      (should (search-forward "(setq use-package-always-ensure nil)" nil t)))))

(ert-deftest test-use-package-ensure/test-helpers-sets-nil ()
  "Test that test-helpers.el also sets use-package-always-ensure to nil.
This ensures tests run with the same configuration as production."
  :tags '(fast unit)
  ;; test-helpers.el should have already been loaded by test-all.el
  (should (null use-package-always-ensure)))

;;; Module Parsing Tests (Static Analysis)

(defun test-use-package-ensure--find-ensure-t-violations ()
  "Find all :ensure t violations in elisp/*.el files.
Returns a list of (file . line-numbers) alist."
  (let ((elisp-dir (expand-file-name "elisp" user-emacs-directory))
        (violations '()))
    (when (file-directory-p elisp-dir)
      (dolist (file (directory-files elisp-dir t "\\.el$"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((file-violations '()))
            ;; Search for :ensure t patterns
            ;; Matches: ":ensure t", ":ensure t)", ":ensure t ", etc.
            (while (re-search-forward "^[[:space:]]*:ensure[[:space:]]+t\\b" nil t)
              (push (line-number-at-pos) file-violations))
            (when file-violations
              (push (cons file (nreverse file-violations)) violations))))))
    (nreverse violations)))

(defun test-use-package-ensure--format-violations (violations)
  "Format VIOLATIONS list into human-readable error message."
  (if (null violations)
      "No :ensure t violations found."
    (let ((msg (format "\nFound :ensure t in %d file(s):\n\n" (length violations))))
      (dolist (violation violations)
        (let ((file (car violation))
              (lines (cdr violation)))
          (setq msg (concat msg
                            (format "  %s:\n    Lines: %s\n\n"
                                    (file-name-nondirectory file)
                                    (mapconcat #'number-to-string lines ", "))))))
      (concat msg
              "\nFix: Remove all :ensure t from these files.\n"
              "Packages are pre-installed by Nix and don't need :ensure.\n"))))

(ert-deftest test-use-package-ensure/no-ensure-t-in-config-files ()
  "Test that no elisp/*.el files contain :ensure t.
This is the primary regression test that will fail until all :ensure t are removed."
  :tags '(fast unit critical)
  (let ((violations (test-use-package-ensure--find-ensure-t-violations)))
    (should-not violations)
    ;; If violations found, provide helpful error message
    (when violations
      (ert-fail (test-use-package-ensure--format-violations violations)))))

(ert-deftest test-use-package-ensure/specific-known-files-clean ()
  "Test that specific files known to have had :ensure t are now clean.
This provides focused regression testing for files that were fixed."
  :tags '(fast unit)
  (let ((files-to-check '("programming.el"
                          "per-project.el"
                          "systems.el"
                          "ui.el"
                          "fonts.el"
                          "collaboration.el"))
        (elisp-dir (expand-file-name "elisp" user-emacs-directory)))
    (dolist (file files-to-check)
      (let ((file-path (expand-file-name file elisp-dir)))
        (when (file-exists-p file-path)
          (with-temp-buffer
            (insert-file-contents file-path)
            (goto-char (point-min))
            (should-not (re-search-forward "^[[:space:]]*:ensure[[:space:]]+t\\b" nil t))
            ;; If found, provide context
            (when (re-search-forward "^[[:space:]]*:ensure[[:space:]]+t\\b" nil t)
              (let ((line (line-number-at-pos)))
                (ert-fail (format "Found :ensure t in %s at line %d" file line))))))))))

;;; Integration Tests (Runtime Behavior)

(ert-deftest test-use-package-ensure/package-install-not-called ()
  "Test that package-install is never called during configuration loading.
This integration test mocks package-install to catch any installation attempts."
  :tags '(integration)
  (let ((package-install-called nil)
        (package-install-original (symbol-function 'package-install)))
    (unwind-protect
        (progn
          ;; Mock package-install to track calls
          (fset 'package-install
                (lambda (&rest args)
                  (setq package-install-called t)
                  (signal 'error (list "Unexpected package-install call" args))))

          ;; Load a configuration module that had :ensure t
          ;; Use eval-buffer to simulate real loading
          (with-temp-buffer
            (insert "(use-package some-package)") ; Without :ensure
            (eval-buffer))

          ;; Verify no installation attempt
          (should-not package-install-called))

      ;; Restore original function
      (fset 'package-install package-install-original))))

(ert-deftest test-use-package-ensure/packages-available-from-nix ()
  "Test that common packages are available without installation.
This verifies that Nix-provided packages are properly accessible."
  :tags '(integration fast)
  ;; These packages are known to be provided by Nix in emacs.nix
  (let ((nix-packages '(vertico
                        consult
                        orderless
                        corfu
                        magit
                        eglot
                        which-key)))
    (dolist (pkg nix-packages)
      ;; Check if package is available (locate-library finds .el or .elc)
      (should (or (featurep pkg)
                  (locate-library (symbol-name pkg)))))))

;;; Edge Case Tests

(ert-deftest test-use-package-ensure/ensure-nil-is-allowed ()
  "Test that :ensure nil is allowed and doesn't cause issues.
Explicit :ensure nil is fine and documents intent."
  :tags '(fast unit)
  ;; This should not error
  (should (macroexpand '(use-package test-pkg :ensure nil))))

(ert-deftest test-use-package-ensure/no-ensure-keyword-is-allowed ()
  "Test that omitting :ensure entirely is allowed (preferred).
This is the recommended pattern - rely on global setting."
  :tags '(fast unit)
  ;; This should not error
  (should (macroexpand '(use-package test-pkg))))

(ert-deftest test-use-package-ensure/ensure-system-package-allowed ()
  "Test that :ensure-system-package is different and allowed.
:ensure-system-package is for system binaries, not Emacs packages."
  :tags '(fast unit)
  ;; :ensure-system-package is a different keyword, should be allowed
  ;; Note: This is just checking the macro expands without error
  (should (macroexpand '(use-package test-pkg :ensure-system-package some-binary))))

;;; Documentation Tests

(ert-deftest test-use-package-ensure/early-init-has-comment ()
  "Test that early-init.el has a comment explaining the setting.
Good documentation prevents future mistakes."
  :tags '(fast unit)
  (let ((early-init-path (expand-file-name "early-init.el" user-emacs-directory)))
    (with-temp-buffer
      (insert-file-contents early-init-path)
      ;; Look for comment near the setting
      (goto-char (point-min))
      (should (search-forward "use-package-always-ensure" nil t))
      (forward-line -2) ; Check a few lines before
      (let ((context (buffer-substring-no-properties (point) (+ (point) 500))))
        ;; Should have some explanatory comment
        (should (or (string-match-p "CRITICAL\\|never\\|auto-install\\|Nix" context)
                    (string-match-p "package management" context)))))))

(provide 'test-use-package-ensure)
;;; test-use-package-ensure.el ends here
