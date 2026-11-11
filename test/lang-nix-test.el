;;; lang-nix-test.el --- Tests for the Nix blank-line indent helper -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-lang-nix.el' is read as data; only the opener predicate is
;; evaluated, in a buffer with Nix's `#' line-comment syntax.

;;; Code:

(require 'ert)

(defconst lang-nix-test--file
  (expand-file-name "lisp/init-lang-nix.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))
  "Absolute path to the Nix module under test.")

(defun lang-nix-test--opens-p (line)
  "Return non-nil when LINE ends in a block opener, per the module."
  (with-temp-buffer
    (insert-file-contents lang-nix-test--file)
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while (not form)
            (let ((f (read (current-buffer))))
              (when (and (eq (car-safe f) 'defun)
                         (eq (nth 1 f) 'my-nix-ts--line-opens-p))
                (setq form f))))
        (end-of-file nil))
      (should form)
      (eval form t)))
  (with-temp-buffer
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    (insert line)
    (my-nix-ts--line-opens-p)))

(ert-deftest lang-nix-test-opener-detected ()
  "A line ending in a bracket opens a block."
  (should (lang-nix-test--opens-p "foo = {")))

(ert-deftest lang-nix-test-opener-ignores-trailing-comment ()
  "A comment after the opener does not hide it; a comment ending in one is no opener."
  (should (lang-nix-test--opens-p "foo = { # attrs"))
  (should-not (lang-nix-test--opens-p "foo = 1; # see {")))

(provide 'lang-nix-test)
;;; lang-nix-test.el ends here
