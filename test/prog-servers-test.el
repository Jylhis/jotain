;;; prog-servers-test.el --- Tests for the eglot server resolvers -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-prog.el' is read as data and only the resolver functions are
;; evaluated, with `executable-find' stubbed to a fixed set of programs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(defvar jotain-prog-enable-risky-js-lsp)

(defmacro prog-servers-test--with-programs (programs &rest body)
  "Run BODY with `executable-find' succeeding only for PROGRAMS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'executable-find)
              (lambda (program &rest _) (and (member program ,programs) program))))
     ,@body))

(defun prog-servers-test--load (&rest names)
  "Evaluate the resolver defuns NAMES out of the programming module."
  (apply #'test-helper-load-defuns "lisp/init-prog.el" names))

(ert-deftest prog-servers-test-ts-server-prefers-rass ()
  "rass wraps typescript-language-server only when both are on PATH."
  (prog-servers-test--load 'jotain-prog--ts-server 'jotain-prog--risky-js-extras)
  (let ((jotain-prog-enable-risky-js-lsp nil))
    (dolist (case '((("rass" "typescript-language-server")
                     ("rass" "--" "typescript-language-server" "--stdio"))
                    (("typescript-language-server")
                     ("typescript-language-server" "--stdio"))
                    (("rass") ("typescript-language-server" "--stdio"))
                    (nil ("typescript-language-server" "--stdio"))))
      (ert-info ((format "PATH has %S" (car case)))
        (prog-servers-test--with-programs (car case)
          (should (equal (jotain-prog--ts-server) (cadr case))))))))

(ert-deftest prog-servers-test-risky-js-extras-are-opt-in ()
  "ESLint and Tailwind join the rass session only when opted in and installed."
  (prog-servers-test--load 'jotain-prog--ts-server 'jotain-prog--risky-js-extras)
  (let ((programs '("rass" "typescript-language-server" "eslint-lsp")))
    (prog-servers-test--with-programs programs
      (let ((jotain-prog-enable-risky-js-lsp nil))
        (should-not (jotain-prog--risky-js-extras)))
      (let ((jotain-prog-enable-risky-js-lsp t))
        (should (equal (jotain-prog--risky-js-extras)
                       '("--" "eslint-lsp" "--stdio")))
        (should (equal (jotain-prog--ts-server)
                       '("rass" "--" "typescript-language-server" "--stdio"
                         "--" "eslint-lsp" "--stdio")))))))

(ert-deftest prog-servers-test-python-server-order ()
  "Python prefers the rass preset, then basedpyright, pyright, and pylsp."
  (prog-servers-test--load 'jotain-prog--python-server)
  (dolist (case '((("rass" "basedpyright" "ruff") ("rass" "python"))
                  (("rass" "basedpyright") ("basedpyright-langserver" "--stdio"))
                  (("basedpyright" "pyright-langserver")
                   ("basedpyright-langserver" "--stdio"))
                  (("pyright-langserver") ("pyright-langserver" "--stdio"))
                  (nil ("pylsp"))))
    (ert-info ((format "PATH has %S" (car case)))
      (prog-servers-test--with-programs (car case)
        (should (equal (jotain-prog--python-server) (cadr case)))))))

(ert-deftest prog-servers-test-likec4-and-robot-fall-back ()
  "The standalone server wins; otherwise the CLI subcommand is used."
  (prog-servers-test--load 'jotain-prog--likec4-server 'jotain-prog--robot-server)
  (prog-servers-test--with-programs '("likec4-lsp" "robotcode")
    (should (equal (jotain-prog--likec4-server) '("likec4-lsp" "--stdio")))
    (should (equal (jotain-prog--robot-server) '("robotcode" "language-server"))))
  (prog-servers-test--with-programs nil
    (should (equal (jotain-prog--likec4-server) '("likec4" "lsp" "--stdio")))
    (should (equal (jotain-prog--robot-server) '("robotframework_ls")))))

(provide 'prog-servers-test)
;;; prog-servers-test.el ends here
