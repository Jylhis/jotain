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
