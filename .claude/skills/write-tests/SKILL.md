---
name: write-tests
description: Write ERT tests for jotain Emacs modules. Use when adding test coverage, regression tests, or implementing TDD. Triggers on: "write test", "add test", "test coverage", "ert-deftest", "regression test", "test fails", "TDD", "unit test", "missing test".
argument-hint: "[module-name or function-name]"
---

# Write ERT Tests

Write tests for the specified target. $ARGUMENTS

## Project Test Structure

```
tests/
├── test-all.el              # Auto-loads all test-*.el files
├── test-helpers.el          # Shared utilities (test-create-temp-directory-with-files, etc.)
├── test-suite-smoke.el      # Smoke test suite runner
├── test-suite-fast.el       # Fast test suite runner
├── test-smoke.el            # Ultra-fast smoke tests (< 1s, no heavy loading)
├── test-*.el                # Module-specific tests
```

## Test Conventions

### Naming
```elisp
;; Pattern: test-[module]/[feature]-[description]
(ert-deftest test-utils/find-org-files-recursively ()
  "Test recursive org file discovery in directory tree."
  :tags '(unit)
  ...)
```

### Tags
Every test MUST have a `:tags` property:
- `smoke` - Ultra-fast, no I/O, no package loading (< 1s total)
- `fast` - Quick unit tests (< 5s total)
- `unit` - Standard unit tests
- `integration` - Cross-module or package interaction tests
- `slow` - Long-running tests (CI only)

### File Header
```elisp
;;; test-module.el --- Tests for module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for elisp/module.el functionality.

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Tests here...

(provide 'test-module)
;;; test-module.el ends here
```

### Isolation
Always clean up with `unwind-protect`:
```elisp
(ert-deftest test-with-cleanup ()
  "Test with guaranteed cleanup."
  :tags '(unit)
  (let ((temp-dir (test-create-temp-directory-with-files
                   '(("test.org" . "* Heading")))))
    (unwind-protect
        (should (file-exists-p (expand-file-name "test.org" temp-dir)))
      (test-cleanup-temp-directory temp-dir))))
```

### Mocking
```elisp
(cl-letf (((symbol-function 'external-api-call)
           (lambda (&rest _) '(:status "success"))))
  (should (equal (function-using-api) expected)))
```

## Test Helpers Available

From `test-helpers.el`:
- `(test-create-temp-directory-with-files FILE-SPECS)` - Create temp dir with files from alist
- `(test-cleanup-temp-directory TEMP-DIR)` - Clean up temp directory

## Running Tests

```bash
just test-smoke              # Smoke tests only (< 1s)
just test-fast               # Fast unit tests (< 5s)
just test                    # Full ERT test suite via Nix
just test-tag TAG            # Run tests by tag
just test-all                # All checks (formatting + smoke + fast + NMT)
```

## Checklist

- [ ] Tests have descriptive names following `test-module/feature-description` pattern
- [ ] Every test has `:tags`
- [ ] Tests are isolated (no interdependencies between tests)
- [ ] Temp files cleaned up with `unwind-protect`
- [ ] Tests check both success and failure cases
- [ ] `just test-smoke` passes after adding tests
