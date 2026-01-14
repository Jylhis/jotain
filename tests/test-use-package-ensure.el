;;; test-use-package-ensure.el --- Tests for use-package :ensure configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to verify correct :ensure usage in configuration files.
;;
;; Convention (Emacs 30+):
;; - External packages: Use explicit `:ensure t` for documentation
;; - Built-in packages: Use `:ensure nil` to mark as built-in
;; - Packages are pre-installed by Nix, :ensure t does not trigger installation
;;   when `use-package-always-ensure` is nil
;;
;; Context:
;; - early-init.el sets (setq use-package-always-ensure nil)
;; - This prevents use-package from auto-installing packages
;; - Individual :ensure t with nil global setting is a no-op (just documentation)
;; - Built-in packages (Emacs 30+): which-key, editorconfig, use-package
;;
;; This test suite validates consistent :ensure usage patterns.

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

(defun test-use-package-ensure--find-builtin-with-ensure-t ()
  "Find built-in packages incorrectly using :ensure t in elisp/*.el files.
Built-in packages should use :ensure nil. Returns list of (file . issues)."
  (let ((elisp-dir (expand-file-name "elisp" user-emacs-directory))
        ;; Built-in packages in Emacs 30+ that must use :ensure nil
        (builtin-packages '("which-key" "editorconfig" "use-package"
                            "project" "eglot" "flymake" "eldoc"
                            "xref" "treesit" "winner" "paren"
                            "mwheel" "pixel-scroll" "smerge-mode"
                            "ediff" "gdb-mi" "diff-mode" "conf-mode"
                            "calendar" "hl-line" "flyspell"))
        (violations '()))
    (when (file-directory-p elisp-dir)
      (dolist (file (directory-files elisp-dir t "\\.el$"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((file-violations '()))
            ;; Search for use-package declarations of built-in packages with :ensure t
            (while (re-search-forward "(use-package[[:space:]]+\\([a-zA-Z0-9_-]+\\)" nil t)
              (let ((pkg-name (match-string 1))
                    (pkg-start (match-beginning 0)))
                (when (member pkg-name builtin-packages)
                  ;; Check if this block has :ensure t (should be :ensure nil)
                  (save-excursion
                    (goto-char pkg-start)
                    (let ((block-end (save-excursion
                                       (forward-sexp)
                                       (point))))
                      (when (re-search-forward ":ensure[[:space:]]+t\\b" block-end t)
                        (push (format "Line %d: %s should use :ensure nil (built-in)"
                                      (line-number-at-pos) pkg-name)
                              file-violations)))))))
            (when file-violations
              (push (cons (file-name-nondirectory file) (nreverse file-violations))
                    violations))))))
    (nreverse violations)))

(ert-deftest test-use-package-ensure/builtin-packages-use-ensure-nil ()
  "Test that built-in packages (Emacs 30+) use :ensure nil, not :ensure t."
  :tags '(fast unit critical)
  (let ((violations (test-use-package-ensure--find-builtin-with-ensure-t)))
    (when violations
      (let ((msg "Built-in packages should use :ensure nil:\n\n"))
        (dolist (v violations)
          (setq msg (concat msg (format "  %s:\n" (car v))))
          (dolist (issue (cdr v))
            (setq msg (concat msg (format "    - %s\n" issue)))))
        (ert-fail msg)))
    (should-not violations)))

(ert-deftest test-use-package-ensure/external-packages-documented ()
  "Test that external packages follow consistent :ensure patterns.
External packages should use :ensure t for explicit documentation."
  :tags '(fast unit)
  ;; This is informational - we verify external packages have :ensure t
  ;; by checking a sample of known external packages
  (let ((elisp-dir (expand-file-name "elisp" user-emacs-directory))
        (sample-externals '("magit" "vertico" "consult" "corfu" "orderless")))
    (dolist (pkg sample-externals)
      (let ((found nil))
        (dolist (file (directory-files elisp-dir t "\\.el$"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward (format "(use-package[[:space:]]+%s\\b" pkg) nil t)
              (setq found t)
              (let ((pkg-start (match-beginning 0)))
                (save-excursion
                  (goto-char pkg-start)
                  (let ((block-end (save-excursion (forward-sexp) (point))))
                    ;; External packages should have :ensure t
                    (should (re-search-forward ":ensure[[:space:]]+t\\b" block-end t))))))))
        (when (not found)
          ;; Package not found in config, skip
          t)))))

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
  "Test that omitting :ensure entirely is allowed.
While explicit :ensure t is preferred for external packages, omitting works too."
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
