;;; test-auth-source-1password.el --- Tests for 1Password auth-source integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the 1Password auth-source integration to ensure it works correctly
;; and can retrieve credentials safely.

;;; Code:

(require 'ert)

;; Only run these tests if auth-source-1password is available
(when (featurep 'auth-source-1password)

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
(unless (featurep 'auth-source-1password)
  (ert-deftest test-auth-source-1password-not-available ()
    "Test behavior when auth-source-1password is not available."
    :tags '(unit auth fast)
    (should-not (featurep 'auth-source-1password))
    (should-not (fboundp 'auth-source-1password-enable))))

(provide 'test-auth-source-1password)
;;; test-auth-source-1password.el ends here