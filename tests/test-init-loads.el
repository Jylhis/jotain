;;; test-init-loads.el --- Integration tests for configuration loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests that verify the full configuration loads correctly.
;; These tests are slower because they load the entire configuration.
;; For fast smoke tests, see test-smoke.el

;;; Code:

(require 'ert)

;;; Configuration Loading Tests (SLOW - loads everything)

(ert-deftest test-integration/early-init-loads ()
  "Test that early-init.el loads without errors."
  :tags '(integration slow critical)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (should-not
     (condition-case err
         (progn
           (load early-init-file nil t)
           nil)
       (error err)))
    (message "early-init.el loaded successfully")))

(ert-deftest test-integration/init-loads ()
  "Test that init.el loads without errors."
  :tags '(integration slow critical)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (should-not
     (condition-case err
         (progn
           (load init-file nil t)
           nil)
       (error err)))
    (message "init.el loaded successfully")))

(ert-deftest test-integration/all-config-modules-load ()
  "Test that all configuration modules can be loaded."
  :tags '(integration slow)
  (let ((modules '(platform core fonts ui completion programming
			    per-project writing git help ai systems platforms
			    app-launchers))
        (failed-modules nil))
    (dolist (module modules)
      (unless (require module nil t)
        (push module failed-modules)))
    (should (null failed-modules))
    (message "Loaded %d config modules successfully" (length modules))))

(ert-deftest test-integration/package-archives-disabled ()
  "Test that package archives are disabled after init.el loads.
This prevents package.el from attempting MELPA/ELPA downloads.
All packages must be pre-installed by Nix."
  :tags '(integration critical)
  ;; Save original state for cleanup
  (let ((original-archives package-archives)
        (init-file (expand-file-name "init.el" user-emacs-directory)))
    (unwind-protect
        (progn
          ;; Load init.el
          (load init-file nil t)
          ;; Verify package-archives is nil
          (should (null package-archives))
          ;; Verify common archives are NOT present
          (should-not (assoc "melpa" package-archives))
          (should-not (assoc "gnu" package-archives))
          (should-not (assoc "nongnu" package-archives))
          (message "Package archives correctly disabled (Nix-only mode)"))
      ;; Restore original state (test isolation)
      (setq package-archives original-archives))))

(ert-deftest test-integration/critical-packages-available ()
  "Test that critical packages are pre-installed and available.
These packages are installed by Nix and should be loadable
without any download attempts."
  :tags '(integration critical)
  (let ((critical-packages '(use-package magit vertico consult org-modern)))
    (dolist (pkg critical-packages)
      (should (package-installed-p pkg))
      (message "Package %s is available" pkg))))

(ert-deftest test-integration/package-metadata-works ()
  "Test that package.el metadata queries still function.
Even with archives disabled, package-installed-p and related
functions should work for querying Nix-installed packages."
  :tags '(integration)
  ;; Load init.el to set up package system
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (load init-file nil t))
  ;; Test metadata queries work
  (should (functionp 'package-installed-p))
  (should (package-installed-p 'use-package))
  (should (package-installed-p 'magit))
  ;; Verify package--initialized is true
  (should package--initialized)
  (message "Package metadata queries functional"))

;;; Feature Tests (require loaded config)

(ert-deftest test-integration/native-comp-available ()
  "Test and report native compilation availability."
  :tags '(integration info)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native compilation is available")
    (message "Native compilation is NOT available"))
  (should t))

(ert-deftest test-integration/treesit-available ()
  "Test and report tree-sitter availability."
  :tags '(integration info)
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
      (message "Tree-sitter is available")
    (message "Tree-sitter is NOT available"))
  (should t))

(provide 'test-init-loads)
;;; test-init-loads.el ends here
