;;; test-all.el --- Minimal test loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Auto-loads all test files from the tests/ directory.
;; Use ERT's built-in functions for running tests:
;;   emacs --batch -l tests/test-all.el -f ert-run-tests-batch-and-exit
;;   emacs --batch -l tests/test-all.el --eval "(ert-run-tests-batch-and-exit '(tag smoke))"

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; Auto-discovery and Loading

;; Auto-load all test-*.el files in the current directory
;; (excluding test-all.el and test-helpers.el)
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (file (directory-files test-dir t "^test-.*\\.el$"))
    (unless (string-match-p "test-\\(all\\|helpers\\)\\.el$" file)
      (load file nil t))))

(provide 'test-all)
;;; test-all.el ends here
