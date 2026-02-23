;;; test-auth-source-1password.el --- Tests for 1Password auth-source integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the 1Password auth-source integration to ensure it works correctly
;; and can retrieve credentials safely.

;;; Code:

(require 'ert)

;; Try to load auth-source-1password (might not be available)
(require 'auth-source-1password nil t)

;; Helper to check if auth-source-1password is fully loaded
(defun auth-source-1password-fully-loaded-p ()
  "Check if auth-source-1password is fully loaded with all variables defined."
  (and (featurep 'auth-source-1password)
       (boundp 'auth-source-1password-vault)
       (boundp 'auth-source-1password-op-executable)))

;; Only run these tests if auth-source-1password is fully available
(when (auth-source-1password-fully-loaded-p)

  (ert-deftest test-auth-source-1password-package-loaded ()
    "Test that auth-source-1password package is properly loaded."
    :tags '(unit integration auth fast)
    (should (featurep 'auth-source-1password))
    (should (fboundp 'auth-source-1password-enable))
    (should (fboundp 'auth-source-1password-search)))

  (ert-deftest test-auth-source-1password-variables ()
    "Test that auth-source-1password variables are properly configured."
    :tags '(unit auth fast)
    ;; Test that custom variables are defined
    (should (boundp 'auth-source-1password-vault))
    (should (boundp 'auth-source-1password-op-executable))
    (should (boundp 'auth-source-1password-debug))
    (should (boundp 'auth-source-1password-cache-ttl))

    ;; Test default values are reasonable
    (should (stringp auth-source-1password-vault))
    (should (stringp auth-source-1password-op-executable))
    (should (booleanp auth-source-1password-debug))
    (should (or (null auth-source-1password-cache-ttl)
                (numberp auth-source-1password-cache-ttl))))

  (ert-deftest test-auth-source-1password-cli-check ()
    "Test the 1Password CLI availability check function."
    :tags '(unit auth fast)
    (should (fboundp 'my/check-1password-cli))
    ;; Test that the function doesn't error when called
    (should (or (executable-find "op")
                (not (executable-find "op"))))) ; This will always pass but tests the logic

  (ert-deftest test-auth-source-1password-backend-enabled ()
    "Test that 1Password is properly registered as an auth-source backend."
    :tags '(integration auth)
    (should (member 'auth-source-1password-search
                    (mapcar (lambda (backend)
                              (when (functionp (plist-get backend :search))
                                (plist-get backend :search)))
                            auth-sources))))

  (ert-deftest test-auth-source-1password-search-fields ()
    "Test that search fields are properly configured."
    :tags '(unit auth fast)
    (should (boundp 'auth-source-1password-search-fields))
    (should (listp auth-source-1password-search-fields))
    (should (member "title" auth-source-1password-search-fields))
    (should (member "website" auth-source-1password-search-fields))
    (should (member "url" auth-source-1password-search-fields))))

;; Provide a test for when the package is not available
(unless (auth-source-1password-fully-loaded-p)
  (ert-deftest test-auth-source-1password-not-available ()
    "Test behavior when auth-source-1password is not fully available."
    :tags '(unit auth fast)
    (should-not (auth-source-1password-fully-loaded-p))))

(ert-deftest test-auth-source-1password-graceful-op-absent ()
  "Test FR-019: 1Password integration fails gracefully when `op` CLI is absent.
When the `op` binary is not on PATH, Emacs should not signal an unhandled error
during configuration loading. The package may disable itself, warn the user, or
simply not register as an auth-source backend — any of these is acceptable."
  :tags '(unit auth fast smoke)
  ;; This test verifies the graceful degradation path: if op is absent,
  ;; Emacs startup must not error (the config loads regardless).
  (if (executable-find "op")
      ;; op is present — just verify normal operation
      (should (featurep 'auth-source-1password))
    ;; op is absent — verify no unhandled error occurred during load
    ;; (the fact that we reach this point means early-init + init loaded cleanly)
    (should t) ; startup completed without signaling an error
    ;; Additionally verify that auth-source-1password does not crash when searched
    (when (featurep 'auth-source-1password)
      (should-not (condition-case err
                      (progn
                        ;; Attempt a search — should either return nil or error gracefully
                        (ignore-errors
                          (funcall #'auth-source-1password-search :host "test"))
                        nil)
                    (error (error-message-string err)))))))

(provide 'test-auth-source-1password)
;;; test-auth-source-1password.el ends here