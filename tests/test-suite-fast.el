;;; test-suite-fast.el --- Fast test suite runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads only fast tests for rapid development feedback.
;; Excludes slow filesystem operations and integration tests.
;; Expected completion time: < 5 seconds

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Load only fast test files
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-smoke.el" test-dir) nil t))

;; Run only tests tagged 'fast' or 'smoke'
(defun run-fast-tests ()
  "Run only fast unit tests."
  (interactive)
  (ert-run-tests-batch-and-exit '(or (tag smoke) (tag fast))))

(provide 'test-suite-fast)
;;; test-suite-fast.el ends here
