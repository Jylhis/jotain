;;; vc-mergiraf-test.el --- Tests for my-vc-mergiraf-solve -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-vc.el' is read as data; only the two mergiraf helpers are
;; evaluated.  The solving test skips when `mergiraf' is not on `exec-path'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'smerge-mode)

(defconst vc-mergiraf-test--file
  (expand-file-name "lisp/init-vc.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))
  "Absolute path to the VC module under test.")

(defconst vc-mergiraf-test--conflict
  (concat "{\n<<<<<<< ours\n  \"a\": 1,\n  \"b\": 2\n"
          "||||||| base\n  \"a\": 1\n=======\n  \"a\": 1,\n  \"c\": 3\n"
          ">>>>>>> theirs\n}\n")
  "A JSON file whose two sides add different keys.")

(defun vc-mergiraf-test--load ()
  "Evaluate the mergiraf defuns out of the VC module."
  (with-temp-buffer
    (insert-file-contents vc-mergiraf-test--file)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((f (read (current-buffer))))
            (when (and (eq (car-safe f) 'defun)
                       (memq (nth 1 f)
                             '(my-vc--conflict-count my-vc-mergiraf-solve)))
              (eval f t))))
      (end-of-file nil))))

(defmacro vc-mergiraf-test--with-file (content &rest body)
  "Visit a temporary .json file holding CONTENT and run BODY there."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "vc-mergiraf" t))
          (file (expand-file-name "a.json" dir)))
     (unwind-protect
         (progn
           (write-region ,content nil file nil 'silent)
           (with-current-buffer (find-file-noselect file)
             (unwind-protect (progn ,@body)
               (set-buffer-modified-p nil)
               (kill-buffer))))
       (delete-directory dir t))))

(ert-deftest vc-mergiraf-test-counts-conflicts ()
  "The counter finds each opening marker."
  (vc-mergiraf-test--load)
  (with-temp-buffer
    (insert vc-mergiraf-test--conflict vc-mergiraf-test--conflict)
    (should (= 2 (my-vc--conflict-count)))))

(ert-deftest vc-mergiraf-test-refuses-without-binary ()
  "A missing mergiraf is a polite `user-error'."
  (vc-mergiraf-test--load)
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (let ((err (should-error (my-vc-mergiraf-solve) :type 'user-error)))
      (should (string-match-p "mergiraf" (cadr err))))))

(ert-deftest vc-mergiraf-test-refuses-without-file ()
  "A buffer with no file is refused."
  (vc-mergiraf-test--load)
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/mergiraf")))
    (with-temp-buffer
      (should-error (my-vc-mergiraf-solve) :type 'user-error))))

(ert-deftest vc-mergiraf-test-refuses-modified-buffer ()
  "A modified buffer is refused before mergiraf rewrites its file."
  (vc-mergiraf-test--load)
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/mergiraf")))
    (vc-mergiraf-test--with-file vc-mergiraf-test--conflict
      (insert " ")
      (should-error (my-vc-mergiraf-solve) :type 'user-error))))

(ert-deftest vc-mergiraf-test-refuses-without-conflict ()
  "A file without markers is refused."
  (vc-mergiraf-test--load)
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/mergiraf")))
    (vc-mergiraf-test--with-file "{}\n"
      (should-error (my-vc-mergiraf-solve) :type 'user-error))))

(ert-deftest vc-mergiraf-test-solves-json-conflict ()
  "mergiraf merges two added keys, and the buffer is reverted."
  (skip-unless (executable-find "mergiraf"))
  (vc-mergiraf-test--load)
  (vc-mergiraf-test--with-file vc-mergiraf-test--conflict
    (my-vc-mergiraf-solve)
    (should (= 0 (my-vc--conflict-count)))
    (should (string-match-p "\"b\": 2" (buffer-string)))
    (should (string-match-p "\"c\": 3" (buffer-string)))
    (should-not (bound-and-true-p smerge-mode))
    (should-not (file-exists-p (concat buffer-file-name ".orig")))))

(provide 'vc-mergiraf-test)
;;; vc-mergiraf-test.el ends here
