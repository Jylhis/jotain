;;; test-smoke.el --- Prove the test runner globs test/*.el -*- lexical-binding: t; -*-

;;; Commentary:

;; A deliberately trivial ERT test whose only job is to prove that the
;; elisp-test flake check discovers every file under test/ instead of
;; loading a hard-coded list.  If this test stops appearing in the
;; batch run output, test discovery has regressed and newly added test
;; files are silently never executed.

;;; Code:

(require 'ert)

(ert-deftest test-smoke-runner-globs-test-directory ()
  "This file is picked up by the test/*.el glob and executed."
  (should t))

(provide 'test-smoke)
;;; test-smoke.el ends here
