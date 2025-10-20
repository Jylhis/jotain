;;; test-helpers.el --- Shared test utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal shared utilities for ERT tests.
;; Provides only actively-used test infrastructure.
;; Following YAGNI (You Aren't Gonna Need It) - add helpers when actually needed.

;;; Code:

(require 'ert)

;;; Temp Directory Management

(defun test-create-temp-directory-with-files (file-specs)
  "Create temporary directory with files from FILE-SPECS.
FILE-SPECS is an alist of (relative-path . content) pairs.
Paths ending with / are created as directories.
Returns the path to the temporary directory."
  (let ((temp-dir (make-temp-file "emacs-test-" t)))
    (dolist (spec file-specs)
      (let* ((path (car spec))
             (content (cdr spec))
             (full-path (expand-file-name path temp-dir)))
        (if (string-match-p "/\\'" path)
            ;; Create directory
            (make-directory full-path t)
          ;; Create file with content
          (make-directory (file-name-directory full-path) t)
          (with-temp-file full-path
            (when content
              (insert content))))))
    temp-dir))

(defun test-cleanup-temp-directory (temp-dir)
  "Clean up temporary directory TEMP-DIR and all its contents."
  (when (and temp-dir (file-exists-p temp-dir))
    (delete-directory temp-dir t)))

(provide 'test-helpers)
;;; test-helpers.el ends here
