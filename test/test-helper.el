;;; test-helper.el --- Read config modules as data for ERT -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared by the tests that must not load a module (its `use-package'
;; forms would pull in packages and side effects).  A module is read as
;; data and only the forms under test are evaluated.  The test check loads
;; every test/*.el with test/ on `load-path', so this file defines no tests
;; and the other files `require' it.

;;; Code:

(require 'ert)
(require 'seq)

(defconst test-helper-root
  (locate-dominating-file
   (or load-file-name buffer-file-name default-directory) "init.el")
  "Repository root, the directory holding init.el.")

(defun test-helper-file (relative)
  "Return the absolute path of RELATIVE within the repository."
  (expand-file-name relative test-helper-root))

(defun test-helper-forms (relative)
  "Return the top-level forms of the repository file RELATIVE."
  (with-temp-buffer
    (insert-file-contents (test-helper-file relative))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun test-helper-form (relative head name)
  "Return the top-level `(HEAD NAME ...)' form of RELATIVE, or nil."
  (seq-find (lambda (f) (and (eq (car-safe f) head) (eq (nth 1 f) name)))
            (test-helper-forms relative)))

(defun test-helper-defun (relative name)
  "Return the `defun' form of NAME in RELATIVE, nested forms included.
Signals a failed assertion when there is none."
  (with-temp-buffer
    (insert-file-contents (test-helper-file relative))
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found)
                  (re-search-forward
                   (format "(defun %s\\_>" (regexp-quote (symbol-name name)))
                   nil t))
        (goto-char (match-beginning 0))
        (setq found (read (current-buffer))))
      (should found)
      found)))

(defun test-helper-load-defuns (relative &rest names)
  "Evaluate the `defun' forms of NAMES out of RELATIVE."
  (dolist (name names)
    (eval (test-helper-defun relative name) t)))

(provide 'test-helper)
;;; test-helper.el ends here
