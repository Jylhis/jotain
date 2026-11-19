;;; lang-vc-test.el --- Tests for the jj path helper in init-vc.el -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-vc.el' is read as data; only the pure path helper is evaluated.

;;; Code:

(require 'ert)

(defconst lang-vc-test--file
  (expand-file-name "lisp/init-vc.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))
  "Absolute path to the VC module under test.")

(defun lang-vc-test--load-helper (&optional name)
  "Evaluate the defun NAME out of the VC module.
NAME defaults to `jotain-vc--jj-summary-path'."
  (with-temp-buffer
    (insert-file-contents lang-vc-test--file)
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while (not form)
            (let ((f (read (current-buffer))))
              (when (and (eq (car-safe f) 'defun)
                         (eq (nth 1 f) (or name 'jotain-vc--jj-summary-path)))
                (setq form f))))
        (end-of-file nil))
      (should form)
      (eval form t))))

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

(provide 'lang-vc-test)
;;; lang-vc-test.el ends here
