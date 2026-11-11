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

(defun lang-vc-test--load-helper ()
  "Evaluate `my-vc--jj-summary-path' out of the VC module."
  (with-temp-buffer
    (insert-file-contents lang-vc-test--file)
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while (not form)
            (let ((f (read (current-buffer))))
              (when (and (eq (car-safe f) 'defun)
                         (eq (nth 1 f) 'my-vc--jj-summary-path))
                (setq form f))))
        (end-of-file nil))
      (should form)
      (eval form t))))

(ert-deftest lang-vc-test-jj-summary-path ()
  "Rename entries resolve to the new path; plain paths stay as they are."
  (lang-vc-test--load-helper)
  (should (equal (my-vc--jj-summary-path "src/a.el") "src/a.el"))
  (should (equal (my-vc--jj-summary-path "{old.el => new.el}") "new.el"))
  (should (equal (my-vc--jj-summary-path "lisp/{a => b}/x.el") "lisp/b/x.el"))
  (should (equal (my-vc--jj-summary-path "lisp/{ => sub}/x.el") "lisp/sub/x.el"))
  (should (equal (my-vc--jj-summary-path "lisp/{sub => }/x.el") "lisp/x.el")))

(provide 'lang-vc-test)
;;; lang-vc-test.el ends here
