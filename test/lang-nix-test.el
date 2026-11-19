;;; lang-nix-test.el --- Tests for the Nix blank-line indent rule -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-lang-nix.el' is read as data; only the indent helpers are
;; evaluated, in a buffer with Nix's `#' line-comment syntax.  The
;; tree-sitter node type is stubbed, so no grammar is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(defvar nix-ts-mode-indent-offset)
(defvar treesit-simple-indent-rules)

(defun lang-nix-test--load ()
  "Evaluate the indent helpers out of the Nix module."
  (test-helper-load-defuns "lisp/init-lang-nix.el"
                           'jotain-nix-ts--line-opens-p
                           'jotain-nix-ts--blank-line-indent
                           'jotain-nix-ts--install-blank-line-rule))

(defmacro lang-nix-test--with-buffer (text &rest body)
  "Run BODY in a Nix-syntax buffer holding TEXT, point at its end."
  (declare (indent 1))
  `(with-temp-buffer
     (set-syntax-table (copy-syntax-table))
     (modify-syntax-entry ?# "<")
     (modify-syntax-entry ?\n ">")
     (insert ,text)
     ,@body))

(defun lang-nix-test--opens-p (line)
  "Return non-nil when LINE ends in a block opener, per the module."
  (lang-nix-test--load)
  (lang-nix-test--with-buffer line
    (jotain-nix-ts--line-opens-p)))

(defun lang-nix-test--blank-indent (text &optional node parent-type)
  "Return the rule's result for a blank last line of TEXT.
NODE is the node the rule sees, PARENT-TYPE the type of its parent."
  (lang-nix-test--load)
  (let ((nix-ts-mode-indent-offset 2))
    (lang-nix-test--with-buffer text
      (cl-letf (((symbol-function 'treesit-node-type)
                 (lambda (_) (or parent-type "source_code"))))
        (let ((res (jotain-nix-ts--blank-line-indent node 'parent (point))))
          (and res (cons (save-excursion (goto-char (car res)) (current-column))
                         (cdr res))))))))

(ert-deftest lang-nix-test-opener-detected ()
  "A line ending in a bracket, `=', `:' or let/in opens a block."
  (dolist (line '("foo = {" "foo = [" "foo = (" "foo =" "f = x:" "let" "  in"))
    (ert-info (line)
      (should (lang-nix-test--opens-p line)))))

(ert-deftest lang-nix-test-opener-ignores-trailing-comment ()
  "A comment after the opener does not hide it; a comment ending in one is no opener."
  (should (lang-nix-test--opens-p "foo = { # attrs"))
  (should-not (lang-nix-test--opens-p "foo = 1; # see {")))

(ert-deftest lang-nix-test-non-openers ()
  "Complete statements and identifiers merely ending in `in' do not open."
  (dolist (line '("foo = 1;" "main" "}" ""))
    (ert-info (line)
      (should-not (lang-nix-test--opens-p line)))))

(ert-deftest lang-nix-test-blank-line-indents-after-opener ()
  "A blank line after an opener indents one offset past that line."
  (should (equal (lang-nix-test--blank-indent "  foo = {\n\n") '(2 . 2))))

(ert-deftest lang-nix-test-blank-line-keeps-indent-after-statement ()
  "After a complete line the blank line keeps that line's indentation."
  (should (equal (lang-nix-test--blank-indent "  foo = 1;\n\n") '(2 . 0))))

(ert-deftest lang-nix-test-blank-line-looks-past-blank-lines ()
  "Earlier blank lines are skipped when looking for the previous line."
  (should (equal (lang-nix-test--blank-indent "foo = {\n\n\n") '(0 . 2))))

(ert-deftest lang-nix-test-blank-line-rule-declines ()
  "The rule yields to the mode's rules for a real node, other parents, buffer start."
  (should-not (lang-nix-test--blank-indent "foo = {\n\n" 'a-node))
  (should-not (lang-nix-test--blank-indent "foo = {\n\n" nil "attrset_expression"))
  (should-not (lang-nix-test--blank-indent "\n")))

(ert-deftest lang-nix-test-blank-line-rule-fires-for-all-three-parents ()
  "source_code, binding_set and ERROR parents all get the rule."
  (dolist (type '("source_code" "binding_set" "ERROR"))
    (ert-info (type)
      (should (equal (lang-nix-test--blank-indent "foo = {\n\n" nil type)
                     '(0 . 2))))))

(ert-deftest lang-nix-test-install-prepends-without-mutating-default ()
  "The rule becomes the head of the nix entry in a copy of the rule list."
  (lang-nix-test--load)
  (let* ((rules '((nix . (existing)) (other . (x))))
         (treesit-simple-indent-rules rules))
    (with-temp-buffer
      (jotain-nix-ts--install-blank-line-rule)
      (should (eq (cadr (assq 'nix treesit-simple-indent-rules))
                  #'jotain-nix-ts--blank-line-indent))
      (should (equal (assq 'other treesit-simple-indent-rules) '(other . (x)))))
    (should (equal (assq 'nix rules) '(nix . (existing))))))

(provide 'lang-nix-test)
;;; lang-nix-test.el ends here
