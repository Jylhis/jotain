;;; lang-devops-test.el --- Syntax-table tests for init-lang-devops.el -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-lang-devops.el' is read as data, so no `use-package' form is
;; expanded.  Only the syntax table under test is evaluated.

;;; Code:

(require 'ert)

(defconst lang-devops-test--file
  (expand-file-name "lisp/init-lang-devops.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))
  "Absolute path to the DevOps module under test.")

(defun lang-devops-test--form (name)
  "Return the top-level `defvar' form for NAME in the DevOps module."
  (with-temp-buffer
    (insert-file-contents lang-devops-test--file)
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'defvar) (eq (nth 1 form) name))
                (throw 'found form))))
        (end-of-file nil)))))

(ert-deftest lang-devops-test-structurizr-hash-comment-ends-at-newline ()
  "A `#' comment ends at the newline, so the next line is code again."
  (let ((form (lang-devops-test--form 'my-structurizr-mode-syntax-table)))
    (should form)
    (with-temp-buffer
      (set-syntax-table (eval (nth 2 form) t))
      (insert "# note\nfoo")
      (should-not (nth 4 (syntax-ppss (point-max))))
      (goto-char (point-min))
      (should (nth 4 (syntax-ppss 3))))))

(provide 'lang-devops-test)
;;; lang-devops-test.el ends here
