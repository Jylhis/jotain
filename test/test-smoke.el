;;; test-smoke.el --- Prove the test runner globs test/*.el -*- lexical-binding: t; -*-

;;; Commentary:

;; A trivial test proving the elisp-test check globs test/*.el rather
;; than loading a fixed list.  If it vanishes from the batch output, new
;; test files are silently not run.

;;; Code:

(require 'ert)

(ert-deftest test-smoke-runner-globs-test-directory ()
  "This file is picked up by the test/*.el glob and executed."
  (should t))

(provide 'test-smoke)
;;; test-smoke.el ends here
