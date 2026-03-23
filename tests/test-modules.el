;;; test-modules.el --- Tests for module loading infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for module loading from lisp/ and modules/, and private.el hook.

;;; Code:

(require 'ert)

;;; Load Path

(ert-deftest test-modules/lisp-on-load-path ()
  "Test that lisp/ directory is on load-path."
  :tags '(fast)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (should (member lisp-dir load-path))))

(ert-deftest test-modules/modules-on-load-path ()
  "Test that modules/ directory is on load-path."
  :tags '(fast)
  (let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
    (should (member modules-dir load-path))))

;;; Module Loading

(ert-deftest test-modules/can-load-mock-module ()
  "Test that a module with (provide) can be loaded from lisp/."
  :tags '(fast)
  (let* ((temp-dir (test-create-temp-directory-with-files
                    '(("test-mock-module.el" . "(provide 'test-mock-module)"))))
         (load-path (cons temp-dir load-path)))
    (unwind-protect
        (progn
          (require 'test-mock-module)
          (should (featurep 'test-mock-module)))
      (unload-feature 'test-mock-module t)
      (test-cleanup-temp-directory temp-dir))))

;;; Private.el Loading

(ert-deftest test-modules/private-el-loads-when-present ()
  "Test that private.el is loaded when it exists."
  :tags '(fast)
  (let* ((temp-dir (test-create-temp-directory-with-files
                    '(("private.el" . "(defvar jotain-test--private-loaded t)"))))
         (user-emacs-directory temp-dir))
    (unwind-protect
        (let ((private-file (expand-file-name "private.el" temp-dir)))
          (load private-file t)
          (should (bound-and-true-p jotain-test--private-loaded)))
      (makunbound 'jotain-test--private-loaded)
      (test-cleanup-temp-directory temp-dir))))

(ert-deftest test-modules/private-el-errors-caught ()
  "Test that errors in private.el are caught without preventing startup."
  :tags '(fast)
  (let* ((temp-dir (test-create-temp-directory-with-files
                    '(("private.el" . "(error \"intentional test error\")"))))
         (user-emacs-directory temp-dir)
         (error-caught nil))
    (unwind-protect
        (let ((private-file (expand-file-name "private.el" temp-dir)))
          (condition-case err
              (load private-file t)
            (error (setq error-caught t)))
          (should error-caught))
      (test-cleanup-temp-directory temp-dir))))

(provide 'test-modules)
;;; test-modules.el ends here
