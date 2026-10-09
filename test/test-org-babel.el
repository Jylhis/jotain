;;; test-org-babel.el --- Org Babel wiring regression tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Two things in `init-org.el' can rot silently:
;;
;; - `jotain-org-babel-languages': a typo, or a language moved out to
;;   org-contrib, passes byte-compilation and breaks `C-c C-c' later.
;;   The tests `require' every `ob-LANG'.
;;
;; - `jotain-org-babel-confirm-evaluate' is a security boundary: if it
;;   returned nil unconditionally, source blocks from any `.org' file
;;   would run without confirmation.
;;
;; Like `test-ui.el', these read `init-org.el' as data instead of loading
;; it (its `:custom' forms need `jotain-notes-directory' from
;; `init-writing'), and evaluate only the forms under test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'project)
(require 'seq)

(defconst test-org-babel--init-org-file
  (expand-file-name "lisp/init-org.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))
  "Absolute path to the Org module under test.")

(defun test-org-babel--top-level-form (head name)
  "Return the `(HEAD NAME ...)' form from the Org module, or nil.
HEAD is a symbol such as `defun' or `defconst'.  The module is read as
data, so no `use-package' form is expanded and no package is loaded."
  (with-temp-buffer
    (insert-file-contents test-org-babel--init-org-file)
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (eq (car form) head)
                         (eq (nth 1 form) name))
                (throw 'found form))))
        (end-of-file nil)))))

(defun test-org-babel--languages ()
  "Return the language list `init-org.el' enables."
  (let ((form (test-org-babel--top-level-form 'defconst 'jotain-org-babel-languages)))
    (should form)
    (eval (nth 2 form) t)))

(defun test-org-babel--define (name)
  "Evaluate the `defun' named NAME out of the Org module."
  (let ((form (test-org-babel--top-level-form 'defun name)))
    (should form)
    (eval form t)))

(ert-deftest test-org-babel-languages-are-distinct-symbols ()
  "The enabled-language list is a clean list of symbols."
  (let ((languages (test-org-babel--languages)))
    (should languages)
    (should (seq-every-p #'symbolp languages))
    (should (equal languages (seq-uniq languages)))))

(ert-deftest test-org-babel-languages-have-backends ()
  "Every enabled language is backed by an `ob-LANG' library Org ships.
Catches a language renamed or moved to org-contrib by an Org bump."
  (dolist (lang (test-org-babel--languages))
    (let ((feature (intern (format "ob-%s" lang))))
      (should (require feature nil t)))))

(ert-deftest test-org-babel-trusts-files-under-org-directory ()
  "A file in the notes tree needs no evaluation prompt."
  (test-org-babel--define 'jotain-org-babel-trusted-p)
  (let* ((notes (file-name-as-directory (make-temp-file "jotain-notes" t)))
         (org-directory notes)
         ;; No project anywhere, so only the notes-tree branch can match.
         (project-find-functions nil))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "notebook.org" notes))
          (should (jotain-org-babel-trusted-p)))
      (delete-directory notes t))))

(ert-deftest test-org-babel-trusts-files-inside-a-project ()
  "A file in the current project needs no evaluation prompt."
  (test-org-babel--define 'jotain-org-babel-trusted-p)
  (let* ((root (file-name-as-directory (make-temp-file "jotain-project" t)))
         (org-directory (expand-file-name "elsewhere" temporary-file-directory))
         (project-find-functions
          (list (lambda (dir) (and (file-in-directory-p dir root)
                                   (cons 'transient root))))))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "README.org" root))
          (should (jotain-org-babel-trusted-p)))
      (delete-directory root t))))

(ert-deftest test-org-babel-does-not-trust-foreign-files ()
  "An Org file from outside the notes tree and outside any project asks.
The security-relevant direction: a downloaded `.org' must not run code
on `C-c C-c' without confirmation."
  (test-org-babel--define 'jotain-org-babel-trusted-p)
  (let* ((elsewhere (file-name-as-directory (make-temp-file "jotain-foreign" t)))
         (org-directory (expand-file-name "notes" temporary-file-directory))
         (project-find-functions nil))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "downloaded.org" elsewhere))
          (should-not (jotain-org-babel-trusted-p)))
      (delete-directory elsewhere t))))

(ert-deftest test-org-babel-does-not-trust-buffers-without-a-file ()
  "A buffer with no backing file is not trusted."
  (test-org-babel--define 'jotain-org-babel-trusted-p)
  (let ((org-directory (expand-file-name "notes" temporary-file-directory))
        (project-find-functions nil))
    (with-temp-buffer
      (should-not (jotain-org-babel-trusted-p)))))

(ert-deftest test-org-babel-confirm-inverts-trust ()
  "`org-confirm-babel-evaluate' prompts exactly when the file is untrusted.
Also guards the arity: Org calls the predicate with two arguments."
  (test-org-babel--define 'jotain-org-babel-confirm-evaluate)
  (cl-letf (((symbol-function 'jotain-org-babel-trusted-p) (lambda () t)))
    (should-not (jotain-org-babel-confirm-evaluate "python" "print(1)")))
  (cl-letf (((symbol-function 'jotain-org-babel-trusted-p) (lambda () nil)))
    (should (jotain-org-babel-confirm-evaluate "python" "print(1)"))))

(provide 'test-org-babel)
;;; test-org-babel.el ends here
