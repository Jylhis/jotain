;;; lang-vc-test.el --- Tests for the jj path helper in init-vc.el -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-vc.el' is read as data; only the pure path helper is evaluated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(defun lang-vc-test--load-helper (&optional name)
  "Evaluate the defun NAME out of the VC module.
NAME defaults to `jotain-vc--jj-summary-path'."
  (test-helper-load-defuns "lisp/init-vc.el" (or name 'jotain-vc--jj-summary-path)))

(ert-deftest lang-vc-test-jj-summary-path ()
  "Rename entries resolve to the new path; plain paths stay as they are."
  (lang-vc-test--load-helper)
  (should (equal (jotain-vc--jj-summary-path "src/a.el") "src/a.el"))
  (should (equal (jotain-vc--jj-summary-path "{old.el => new.el}") "new.el"))
  (should (equal (jotain-vc--jj-summary-path "lisp/{a => b}/x.el") "lisp/b/x.el"))
  (should (equal (jotain-vc--jj-summary-path "lisp/{ => sub}/x.el") "lisp/sub/x.el"))
  (should (equal (jotain-vc--jj-summary-path "lisp/{sub => }/x.el") "lisp/x.el")))

(ert-deftest lang-vc-test-dir-status-no-query ()
  "A buffer with a live status process dies without a kill prompt."
  (lang-vc-test--load-helper 'jotain-vc--dir-status-no-query)
  (let* ((buf (generate-new-buffer " *lang-vc-test*"))
         (proc (start-process "lang-vc-test" buf "sleep" "30"))
         (asked nil))
    (unwind-protect
        (with-current-buffer buf
          (should (process-query-on-exit-flag proc))
          (jotain-vc--dir-status-no-query)
          (should-not (process-query-on-exit-flag proc))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked t) nil))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (setq asked t) nil)))
            (should (kill-buffer buf)))
          (should-not asked))
      (when (process-live-p proc) (delete-process proc))
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))))

(ert-deftest lang-vc-test-parse-numstat ()
  "Added and deleted columns are summed; binary and malformed lines are skipped."
  (lang-vc-test--load-helper 'jotain-git-stats--parse-numstat)
  (dolist (case '(("" 0)
                  ("3\t2\ta.el\n" 5)
                  ("3\t2\ta.el\n10\t0\tb.el\n0\t7\tc.el\n" 22)
                  ("-\t-\timage.png\n4\t1\ta.el\n" 5)
                  ("3\t2\ta.el\n\nnot numstat\n5\tx\td.el\n" 5)))
    (ert-info ((format "%S" (car case)))
      (should (= (jotain-git-stats--parse-numstat (car case)) (cadr case))))))

(ert-deftest lang-vc-test-count-diff-churn ()
  "Changed lines count once; `+++' and `---' headers and context do not."
  (lang-vc-test--load-helper 'jotain-git-stats--count-diff-churn)
  (let ((diff (concat "diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1,3 +1,3 @@\n"
                      " ctx\n-old\n+new\n+added\n")))
    (should (= (jotain-git-stats--count-diff-churn diff) 3)))
  (should (= (jotain-git-stats--count-diff-churn "") 0))
  (should (= (jotain-git-stats--count-diff-churn "+\n-\n") 2)))

(provide 'lang-vc-test)
;;; lang-vc-test.el ends here
