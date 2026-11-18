;;; lang-kotlin-test.el --- Tests for the Kotlin helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-lang-kotlin.el' and `init-prog.el' are read as data; only the
;; pure helpers are evaluated.  No Kotlin tool and no grammar is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst lang-kotlin-test--root
  (locate-dominating-file
   (or load-file-name buffer-file-name default-directory) "init.el")
  "Root of the configuration under test.")

(defun lang-kotlin-test--defun (file name)
  "Return the `defun' form of NAME from FILE, searching nested forms too."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file lang-kotlin-test--root))
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

(defun lang-kotlin-test--load (file &rest names)
  "Evaluate the `defun' forms of NAMES from FILE."
  (dolist (name names)
    (eval (lang-kotlin-test--defun file name) t)))

(ert-deftest lang-kotlin-test-parse-ktlint-line ()
  "A ktlint plain-reporter line yields line, column and message."
  (lang-kotlin-test--load "lisp/init-lang-kotlin.el" 'my-kotlin--parse-lint-line)
  (should (equal (my-kotlin--parse-lint-line
                  "<stdin>:3:5: Unused import (standard:no-unused-imports)")
                 '(3 5 "Unused import (standard:no-unused-imports)"))))

(ert-deftest lang-kotlin-test-parse-detekt-line ()
  "A detekt console line with an absolute path parses the same way."
  (lang-kotlin-test--load "lisp/init-lang-kotlin.el" 'my-kotlin--parse-lint-line)
  (should (equal (my-kotlin--parse-lint-line
                  "/src/app/Main.kt:12:1: Function is too long. [LongMethod]")
                 '(12 1 "Function is too long. [LongMethod]"))))

(ert-deftest lang-kotlin-test-parse-ignores-noise ()
  "Summary and blank lines are not findings."
  (lang-kotlin-test--load "lisp/init-lang-kotlin.el" 'my-kotlin--parse-lint-line)
  (should-not (my-kotlin--parse-lint-line ""))
  (should-not (my-kotlin--parse-lint-line "Summary error count (descending) by rule:"))
  (should-not (my-kotlin--parse-lint-line "detekt finished in 812 ms")))

(ert-deftest lang-kotlin-test-gradle-prefers-wrapper ()
  "The project's wrapper wins; without one the command is plain gradle."
  (lang-kotlin-test--load "lisp/init-lang-kotlin.el" 'my-kotlin--gradle)
  (let ((root (make-temp-file "lang-kotlin-test" t)))
    (unwind-protect
        (progn
          (should (equal (my-kotlin--gradle root) "gradle"))
          (write-region "" nil (expand-file-name "gradlew" root))
          (should (equal (my-kotlin--gradle root) "./gradlew")))
      (delete-directory root t))))

(ert-deftest lang-kotlin-test-detekt-command-uses-project-config ()
  "detekt gets the project's config file only when it exists."
  (lang-kotlin-test--load "lisp/init-lang-kotlin.el" 'my-kotlin--detekt-command)
  (let ((root (make-temp-file "lang-kotlin-test" t)))
    (unwind-protect
        (progn
          (should (equal (my-kotlin--detekt-command "A.kt" root)
                         '("detekt" "--input" "A.kt")))
          (make-directory (expand-file-name "config/detekt" root) t)
          (write-region "" nil (expand-file-name "config/detekt/detekt.yml" root))
          (should (member "--config" (my-kotlin--detekt-command "A.kt" root))))
      (delete-directory root t))))

(ert-deftest lang-kotlin-test-server-prefers-kotlin-lsp ()
  "The resolver picks kotlin-lsp when present, else kotlin-language-server."
  (lang-kotlin-test--load "lisp/init-prog.el" 'my-prog--kotlin-server)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program &rest _) (equal program "kotlin-lsp"))))
    (should (equal (my-prog--kotlin-server) '("kotlin-lsp" "--stdio"))))
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should (equal (my-prog--kotlin-server) '("kotlin-language-server")))))

(ert-deftest lang-kotlin-test-main-class-guess ()
  "The debugger's main-class guess is PACKAGE.FILEKt."
  (lang-kotlin-test--load "lisp/init-prog.el" 'my-prog--kotlin-main-class)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/App.kt")
    (insert "package com.example.demo\n\nfun main() {}\n")
    (should (equal (my-prog--kotlin-main-class) "com.example.demo.AppKt"))
    (erase-buffer)
    (insert "fun main() {}\n")
    (should (equal (my-prog--kotlin-main-class) "AppKt"))
    (set-buffer-modified-p nil)
    (setq buffer-file-name nil)))

(provide 'lang-kotlin-test)
;;; lang-kotlin-test.el ends here
