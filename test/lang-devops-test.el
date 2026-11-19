;;; lang-devops-test.el --- Tests for the Structurizr and LikeC4 modes -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-lang-devops.el' is read as data, so no `use-package' form is
;; expanded.  Only the syntax tables and indent functions under test are
;; evaluated.

;;; Code:

(require 'ert)
(require 'test-helper)

(defvar jotain-structurizr-indent-offset 4)
(defvar jotain-likec4-indent-offset 2)
(defvar jotain-structurizr-mode-syntax-table)

(defun lang-devops-test--table (name)
  "Evaluate and return the syntax table `defvar' NAME of the DevOps module."
  (let ((form (test-helper-form "lisp/init-lang-devops.el" 'defvar name)))
    (should form)
    (eval (nth 2 form) t)))

(defun lang-devops-test--indent (indent-fn table-name text)
  "Return TEXT re-indented line by line with INDENT-FN under syntax table TABLE-NAME."
  (test-helper-load-defuns "lisp/init-lang-devops.el"
                           'jotain-devops--brace-indent-line indent-fn)
  (let ((structurizr (lang-devops-test--table 'jotain-structurizr-mode-syntax-table)))
    (with-temp-buffer
      (setq indent-tabs-mode nil)
      (set-syntax-table
       (if (eq table-name 'jotain-structurizr-mode-syntax-table)
           structurizr
         (let ((jotain-structurizr-mode-syntax-table structurizr))
           (lang-devops-test--table table-name))))
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (funcall indent-fn)
        (forward-line 1))
      (buffer-string))))

(ert-deftest lang-devops-test-structurizr-hash-comment-ends-at-newline ()
  "A `#' comment ends at the newline, so the next line is code again."
  (with-temp-buffer
    (set-syntax-table (lang-devops-test--table 'jotain-structurizr-mode-syntax-table))
    (insert "# note\nfoo")
    (should-not (nth 4 (syntax-ppss (point-max))))
    (goto-char (point-min))
    (should (nth 4 (syntax-ppss 3)))))

(ert-deftest lang-devops-test-likec4-hash-is-not-a-comment ()
  "LikeC4 only has the C-style comment forms; `#' is plain punctuation."
  (let ((jotain-structurizr-mode-syntax-table
         (lang-devops-test--table 'jotain-structurizr-mode-syntax-table)))
    (with-temp-buffer
      (set-syntax-table (lang-devops-test--table 'jotain-likec4-mode-syntax-table))
      (insert "# x\nfoo")
      (should-not (nth 4 (syntax-ppss (point-max))))
      (goto-char (point-min))
      (should-not (nth 4 (syntax-ppss 3))))))

(ert-deftest lang-devops-test-structurizr-indents-by-brace-depth ()
  "Each open brace adds the offset; a leading `}' closes its block."
  (should (equal (lang-devops-test--indent
                  'jotain-structurizr-indent-line
                  'jotain-structurizr-mode-syntax-table
                  "workspace {\nmodel {\nperson u\n}\n}\n")
                 "workspace {\n    model {\n        person u\n    }\n}\n")))

(ert-deftest lang-devops-test-likec4-indents-by-brace-depth ()
  "LikeC4 uses its own offset and the same brace rule."
  (should (equal (lang-devops-test--indent
                  'jotain-likec4-indent-line
                  'jotain-likec4-mode-syntax-table
                  "model {\n    a = system {\ntitle 'x'\n    }\n}\n")
                 "model {\n  a = system {\n    title 'x'\n  }\n}\n")))

(ert-deftest lang-devops-test-indent-ignores-braces-in-strings-and-comments ()
  "A brace inside a string or comment does not change the depth."
  (should (equal (lang-devops-test--indent
                  'jotain-structurizr-indent-line
                  'jotain-structurizr-mode-syntax-table
                  "a {\n  // {\nb \"{\"\n}\n")
                 "a {\n    // {\n    b \"{\"\n}\n")))

(provide 'lang-devops-test)
;;; lang-devops-test.el ends here
