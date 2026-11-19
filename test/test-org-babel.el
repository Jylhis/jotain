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
(require 'test-helper)

(defun test-org-babel--languages ()
  "Return the language list `init-org.el' enables."
  (let ((form (test-helper-form "lisp/init-org.el" 'defconst 'jotain-org-babel-languages)))
    (should form)
    (eval (nth 2 form) t)))

(defun test-org-babel--define (name)
  "Evaluate the `defun' named NAME out of the Org module."
  (let ((form (test-helper-form "lisp/init-org.el" 'defun name)))
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

(ert-deftest test-org-babel-restart-kills-buffer-session ()
  "Restart kills the old session when `org-babel-initiate-session' returns a buffer.
ob-python returns a buffer, not a name."
  (test-org-babel--define 'jotain-org-babel-restart-session-and-execute-buffer)
  (let ((session (generate-new-buffer " *test-session*"))
        (ran nil))
    (unwind-protect
        (cl-letf (((symbol-function 'org-babel-initiate-session)
                   (lambda (&rest _) session))
                  ((symbol-function 'org-babel-execute-buffer)
                   (lambda (&rest _) (setq ran t))))
          (jotain-org-babel-restart-session-and-execute-buffer)
          (should-not (buffer-live-p session))
          (should ran))
      (when (buffer-live-p session) (kill-buffer session)))))

(provide 'test-org-babel)
;;; test-org-babel.el ends here
