# Testing Quick Reference

## Run All Tests

```bash
# Using test-runner (recommended)
emacs --batch -L elisp -L tests -l test-runner.el

# Using ERT directly
emacs --batch -L elisp -L tests \
  -l test-helper.el \
  -l test-smoke.el \
  -f ert-run-tests-batch-and-exit

# Using justfile
just test-dev
```

## Run Specific Tests

```bash
# Run only smoke tests matching pattern
emacs --batch -L elisp -L tests \
  -l test-helper.el \
  -l test-smoke.el \
  --eval '(ert-run-tests-batch "test-smoke/emacs")'

# Run a single test
emacs --batch -L elisp -L tests \
  -l test-helper.el \
  -l test-smoke.el \
  --eval "(ert-run-tests-batch 'test-smoke/emacs-version-check)"
```

## Interactive Testing

```bash
# Run tests with UI
emacs -Q -L elisp -L tests \
  -l test-helper.el \
  -l test-smoke.el \
  --eval "(ert-run-tests-interactively t)"

# List all tests
emacs --batch -L elisp -L tests -l test-runner.el \
  --eval "(jotain-test-list)"
```

## Test Statistics

```bash
# Show test count and names
emacs --batch -L elisp -L tests \
  --eval "(progn
           (require 'test-helper)
           (require 'test-smoke)
           (message \"Tests: %d\" (length (ert-select-tests t t))))"
```

## Exit Codes

- `0` = All tests passed
- `1` = One or more tests failed

## Common Workflows

### After Code Changes

```bash
# Quick validation
emacs --batch -L elisp -L tests -l test-runner.el

# Expect output like:
# Ran 11 tests, 11 results as expected, 0 unexpected (0.002 sec)
```

### Before Committing

```bash
# Run full test suite
just test-dev

# Should see: "All critical smoke tests passed ✓"
```

### Adding New Tests

1. Create test file: `tests/test-myfeature.el`
2. Add to `test-runner.el`: Update `jotain-test-files`
3. Run: `emacs --batch -L elisp -L tests -l test-runner.el`

## Test File Structure

```elisp
;;; test-myfeature.el --- Tests for myfeature -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest test-smoke/myfeature-basic ()
  "Test basic myfeature functionality."
  (should (featurep 'myfeature))
  (jotain-test-batch-message "✓ myfeature loaded"))

(provide 'test-myfeature)
;;; test-myfeature.el ends here
```

## Troubleshooting

### Tests Not Found

```bash
# Verify load paths
emacs --batch -L elisp -L tests \
  --eval "(message \"Load path: %s\" load-path)"
```

### Module Load Errors

```bash
# Check module existence
emacs --batch -L elisp -L tests -l test-helper.el \
  --eval "(message \"Module exists: %s\" 
           (jotain-test-module-exists-p 'jotain-core))"
```

### Timing Issues

Tests run in < 0.005 seconds. If slower:
- Check for network calls
- Look for unnecessary file I/O
- Verify no interactive prompts

## Performance Benchmarks

Current benchmarks (Emacs 30.2):

- **Total suite**: < 0.005 seconds
- **Per test**: < 0.0005 seconds average
- **Memory**: < 1MB overhead

## CI/CD Integration

GitHub Actions example:

```yaml
- name: Run Jotain tests
  run: |
    emacs --batch -L elisp -L tests -l test-runner.el
    if [ $? -eq 0 ]; then
      echo "✓ All tests passed"
    else
      echo "✗ Tests failed"
      exit 1
    fi
```
