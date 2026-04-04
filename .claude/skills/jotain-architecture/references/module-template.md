# Jotain Module Template

## Elisp Module: `lisp/jotain-<name>.el`

```elisp
;;; jotain-<name>.el --- <One-line description> -*- lexical-binding:t; -*-

;; Author: Markus Jylhänkangas
;; URL: https://github.com/jylhis/jotain

;;; Commentary:

;; <What this module does and why>

;;; Code:

;;;; Customization

(defgroup jotain-<name> nil
  "<Description>."
  :group 'jotain
  :prefix "jotain-<name>-")

(defcustom jotain-<name>-enabled t
  "When non-nil, <name> is active."
  :type 'boolean)

;;;; Internal variables

(defvar jotain-<name>--state nil
  "Internal state for <name>.")

;;;; Functions

(defun jotain-<name>--internal-helper ()
  "Internal helper function."
  ...)

(defun jotain-<name>-do-thing ()
  "Public interactive command."
  (interactive)
  ...)

;;;; Minor mode (if applicable)

;;;###autoload
(define-minor-mode jotain-<name>-mode
  "Global minor mode for <description>."
  :global t
  :lighter " <Abbrev>"
  (if jotain-<name>-mode
      (jotain-<name>--setup)
    (jotain-<name>--teardown)))

(provide 'jotain-<name>)
;;; jotain-<name>.el ends here
```

## Test File: `test/jotain-<name>-test.el`

```elisp
;;; jotain-<name>-test.el --- Tests for jotain-<name> -*- lexical-binding:t; -*-

;;; Commentary:
;; ERT test suite for the <name> module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jotain-<name>)

;;;; State isolation

(defmacro jotain-<name>-test--with-clean-state (&rest body)
  "Run BODY with all state variables locally bound to clean defaults."
  (declare (indent 0) (debug t))
  `(let ((jotain-<name>-enabled t)
         (jotain-<name>--state nil))
     ,@body))

;;;; Tests

(ert-deftest jotain-<name>-test-example ()
  "Example test."
  (jotain-<name>-test--with-clean-state
    (should (eq jotain-<name>-enabled t))))

(provide 'jotain-<name>-test)
;;; jotain-<name>-test.el ends here
```

## init.el Wiring

```elisp
(use-package jotain-<name>
  :custom
  (jotain-<name>-enabled t)
  :config
  (jotain-<name>-mode 1))
```

## Justfile Test Entry

Add to `Justfile` or extend the existing test recipe:
```just
test:
  emacs --batch -L lisp -L test -l test/jotain-<name>-test.el -f ert-run-tests-batch-and-exit
```
