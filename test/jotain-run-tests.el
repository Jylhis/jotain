;;; jotain-run-tests.el --- Aggregate ERT runner for jotain -*- lexical-binding:t; -*-

;;; Commentary:

;; Loads every `*-test.el' file under this directory and runs all ERT
;; tests in batch mode.  Used by the Justfile and CI to avoid having
;; to hand-list each test module.
;;
;; Usage:
;;   emacs --batch -L lisp -L test -l test/jotain-run-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(let* ((this-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory this-file)))
  (dolist (f (directory-files test-dir t "-test\\.el\\'"))
    (load f nil t)))

(provide 'jotain-run-tests)
;;; jotain-run-tests.el ends here
