;;; test-suite-smoke.el --- Smoke test suite runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs only critical smoke tests for ultra-fast validation.
;; Expected completion time: < 1 second

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Load only smoke tests
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-smoke.el" test-dir) nil t))

(defun run-smoke-tests ()
  "Run only smoke tests."
  (interactive)
  (ert-run-tests-batch-and-exit '(tag smoke)))

(provide 'test-suite-smoke)
;;; test-suite-smoke.el ends here
