# Jotain Test Suite

Smoke test infrastructure for the Jotain Emacs distribution.

## Overview

The test suite consists of:

- **test-helper.el** - Test utilities, environment isolation, and common assertions
- **test-smoke.el** - 11 smoke tests validating core functionality
- **test-runner.el** - Test runner with batch mode support for CI/CD

## Running Tests

### Batch Mode (for CI/CD)

```bash
# Run with test-runner
emacs --batch -L elisp -L tests -l test-runner.el

# Run directly with ERT
emacs --batch -L elisp -L tests \
  -l test-helper.el \
  -l test-smoke.el \
  -f ert-run-tests-batch-and-exit
```

### Using Justfile

```bash
# Run tests in development mode
just test-dev

# Run tests via Nix checks
just test
```

### Interactive Mode

```bash
# Run all tests interactively
emacs -Q -L elisp -L tests \
  -l test-helper.el \
  -l test-smoke.el \
  --eval "(ert-run-tests-interactively t)"
```

## Adding New Tests

### Create a New Test File

```elisp
;;; test-mymodule.el --- Tests for mymodule -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest test-smoke/mymodule-loads ()
  "Verify mymodule loads correctly."
  (should (jotain-test-module-loadable-p 'jotain-mymodule))
  (jotain-test-batch-message "✓ mymodule loaded"))

(provide 'test-mymodule)
;;; test-mymodule.el ends here
```

### Register in test-runner.el

Add to `jotain-test-files` list:

```elisp
(defvar jotain-test-files
  '("test-smoke.el"
    "test-mymodule.el")
  "List of test files to run.")
```

## Test Design Guidelines

### Smoke Tests

- Keep tests SIMPLE - just verify things load
- Test basic variable definitions and function existence
- Don't test complex behavior (that's for integration tests)
- Tests should run fast (< 5 seconds total)
- Use clear test names with `test-smoke/` prefix

### Test Isolation

- Don't modify global state without cleanup
- Use `unwind-protect` for guaranteed cleanup
- Each test should be independent
- Don't rely on test execution order

### Naming Convention

```
test-[category]/[feature]-[description]

Examples:
- test-smoke/jotain-loads
- test-smoke/emacs-version-check
- test-smoke/dev-mode-detection-enabled
```

## Exit Codes

- **0** - All tests passed
- **1** - One or more tests failed

## Dependencies

- Emacs 30.1+
- ERT (built-in)
- No external dependencies required

## CI/CD Integration

Tests are designed for batch mode execution in CI/CD pipelines:

```yaml
# Example GitHub Actions
- name: Run tests
  run: |
    emacs --batch -L elisp -L tests -l test-runner.el
```
