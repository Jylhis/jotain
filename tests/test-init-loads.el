;;; test-init-loads.el --- Smoke tests for configuration loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Fast smoke tests that verify the basic configuration loads correctly.
;; These tests should run in under 1 second and catch obvious configuration errors.
;;
;; Smoke tests are meant to provide rapid feedback on whether the configuration
;; is fundamentally broken before running the full test suite.

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; Emacs Version Tests

(ert-deftest test-smoke/emacs-version-minimum ()
  "Test that Emacs version meets minimum requirement (29.1+)."
  :tags '(smoke critical fast)
  (should (version<= "29.1" emacs-version)
          (format "Emacs version %s is below minimum required 29.1" emacs-version)))

(ert-deftest test-smoke/emacs-version-info ()
  "Test that Emacs version info is available."
  :tags '(smoke fast)
  (should (stringp emacs-version))
  (should (> (length emacs-version) 0))
  (message "Running on Emacs %s" emacs-version))

;;; Early Init Tests

(ert-deftest test-smoke/early-init-exists ()
  "Test that early-init.el file exists."
  :tags '(smoke critical fast)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (should (file-exists-p early-init-file)
            (format "early-init.el not found at %s" early-init-file))))

(ert-deftest test-smoke/early-init-loads ()
  "Test that early-init.el loads without errors."
  :tags '(smoke critical)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (should-not
     (condition-case err
         (progn
           (load early-init-file nil t)
           nil)  ; Return nil if successful
       (error err)))  ; Return error if failed
    (message "early-init.el loaded successfully")))

;;; Init Tests

(ert-deftest test-smoke/init-exists ()
  "Test that init.el file exists."
  :tags '(smoke critical fast)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (should (file-exists-p init-file)
            (format "init.el not found at %s" init-file))))

(ert-deftest test-smoke/init-loads ()
  "Test that init.el loads without errors."
  :tags '(smoke critical)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (should-not
     (condition-case err
         (progn
           (load init-file nil t)
           nil)  ; Return nil if successful
       (error err)))  ; Return error if failed
    (message "init.el loaded successfully")))

;;; Module Loading Tests

(ert-deftest test-smoke/platform-module-loads ()
  "Test that platform module loads (must load first)."
  :tags '(smoke critical fast)
  (should (require 'platform nil t)
          "Failed to load platform module"))

(ert-deftest test-smoke/core-module-loads ()
  "Test that core module loads."
  :tags '(smoke critical fast)
  (should (require 'core nil t)
          "Failed to load core module"))

(ert-deftest test-smoke/all-config-modules-load ()
  "Test that all configuration modules can be loaded."
  :tags '(smoke)
  (let ((modules '(platform core fonts ui completion programming
                   per-project writing git help ai systems platforms
                   app-launchers))
        (failed-modules nil))
    (dolist (module modules)
      (unless (require module nil t)
        (push module failed-modules)))
    (should (null failed-modules)
            (format "Failed to load modules: %s"
                    (mapconcat #'symbol-name failed-modules ", ")))))

;;; Directory Structure Tests

(ert-deftest test-smoke/config-directory-exists ()
  "Test that config/ directory exists."
  :tags '(smoke fast)
  (let ((config-dir (expand-file-name "config" user-emacs-directory)))
    (should (file-directory-p config-dir)
            (format "config/ directory not found at %s" config-dir))))

(ert-deftest test-smoke/lisp-directory-exists ()
  "Test that lisp/ directory exists."
  :tags '(smoke fast)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (should (file-directory-p lisp-dir)
            (format "lisp/ directory not found at %s" lisp-dir))))

(ert-deftest test-smoke/load-path-configured ()
  "Test that load-path includes config and lisp directories."
  :tags '(smoke fast)
  (let ((config-dir (expand-file-name "config" user-emacs-directory))
        (lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (should (member config-dir load-path)
            "config/ directory not in load-path")
    (should (member lisp-dir load-path)
            "lisp/ directory not in load-path")))

;;; Feature Availability Tests

(ert-deftest test-smoke/native-comp-available ()
  "Test and report native compilation availability."
  :tags '(smoke fast)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native compilation is available")
    (message "Native compilation is NOT available"))
  ;; This is informational, not a hard requirement
  (should t))

(ert-deftest test-smoke/treesit-available ()
  "Test and report tree-sitter availability."
  :tags '(smoke fast)
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
      (message "Tree-sitter is available")
    (message "Tree-sitter is NOT available"))
  ;; This is informational, not a hard requirement
  (should t))

;;; Basic Functionality Tests

(ert-deftest test-smoke/can-create-buffer ()
  "Test that basic Emacs buffer operations work."
  :tags '(smoke fast)
  (with-temp-buffer
    (should (bufferp (current-buffer)))
    (insert "test")
    (should (= (point-max) 5))
    (should (string= (buffer-string) "test"))))

(ert-deftest test-smoke/can-require-cl-lib ()
  "Test that cl-lib is available."
  :tags '(smoke fast)
  (should (require 'cl-lib nil t)
          "cl-lib not available"))

(provide 'test-init-loads)
;;; test-init-loads.el ends here
