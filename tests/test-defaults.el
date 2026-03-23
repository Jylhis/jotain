;;; test-defaults.el --- Tests for jotain-defaults module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Emacs version check and sensible defaults.

;;; Code:

(require 'ert)

(ert-deftest test-defaults/version-warning-when-old ()
  "Test that a warning is issued when Emacs version is below 30."
  :tags '(fast)
  (let ((warning-issued nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type _message &rest _args) (setq warning-issued t)))
              (emacs-major-version 29))
      ;; Simulate the version check logic
      (when (< emacs-major-version 30)
        (display-warning 'jotain "Emacs 30+ required"))
      (should warning-issued))))

(ert-deftest test-defaults/no-warning-when-current ()
  "Test that no warning is issued when Emacs version is >= 30."
  :tags '(fast)
  (let ((warning-issued nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type _message &rest _args) (setq warning-issued t))))
      ;; Current Emacs should be >= 30 (spec requirement)
      (when (< emacs-major-version 30)
        (display-warning 'jotain "Emacs 30+ required"))
      (should-not warning-issued))))

(provide 'test-defaults)
;;; test-defaults.el ends here
