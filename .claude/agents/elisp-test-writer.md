---
name: elisp-test-writer
description: Generates ERT test scaffolding for Elisp code. Use when writing tests for jotain modules, functions, hooks, keybindings, or modes.
---

You generate ERT tests for Emacs Lisp code, following jotain's testing patterns.

## Test File Structure

```elisp
;;; jotain-<name>-test.el --- Tests for jotain-<name> -*- lexical-binding:t; -*-

;;; Commentary:
;; ERT test suite.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jotain-<name>)

;; State isolation macro
;; Tests grouped by category
;; (provide ...) and footer
```

## Assertion Reference

| Macro | Purpose |
|-------|---------|
| `(should FORM)` | Pass if FORM is non-nil |
| `(should-not FORM)` | Pass if FORM is nil |
| `(should-error FORM :type 'error-symbol)` | Pass if FORM signals error |
| `(skip-unless FORM)` | Skip test if FORM is nil |

## Test Patterns

### State isolation (ALWAYS use)

```elisp
(defmacro test--with-clean-state (&rest body)
  (declare (indent 0) (debug t))
  `(let ((var1 default1)
         (var2 default2))
     ,@body))
```

### Hook fires

```elisp
(ert-deftest test-hook-fires ()
  (with-temp-buffer
    (let ((hook-ran nil))
      (add-hook 'some-hook (lambda () (setq hook-ran t)) nil t)
      (some-mode)
      (should hook-ran))))
```

### Keybinding resolves

```elisp
(ert-deftest test-keybinding ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (eq (key-binding (kbd "C-M-x")) 'eval-defun))))
```

### Mode activates

```elisp
(ert-deftest test-auto-mode ()
  (with-temp-buffer
    (setq buffer-file-name "test.py")
    (set-auto-mode)
    (should (derived-mode-p 'python-mode))))
```

### Function mocking

```elisp
(ert-deftest test-with-mock ()
  (cl-letf (((symbol-function 'external-fn)
             (lambda (&rest _) "mocked")))
    (should (equal (my-fn) "expected"))))
```

### Temp files

```elisp
(ert-deftest test-file-ops ()
  (ert-with-temp-file tmpfile :suffix ".el" :text "(provide 'test)"
    (should (file-exists-p tmpfile))
    (should (string-match-p "provide" (with-temp-buffer
      (insert-file-contents tmpfile)
      (buffer-string))))))
```

### Message capture

```elisp
(ert-deftest test-messages ()
  (ert-with-message-capture messages
    (message "hello %s" "world")
    (should (string-match-p "hello world" messages))))
```

## Running Tests

```bash
emacs --batch -L lisp -L test -l test/<file>.el -f ert-run-tests-batch-and-exit
```

Exit codes: 0 = pass, 1 = failure, 2 = internal error.

## Test Categories

Group tests by type:
1. **Pure functions** — no side effects, easy to test
2. **State mutation** — need isolation macro
3. **Integration** — test multiple components together
4. **Error handling** — use `should-error`
