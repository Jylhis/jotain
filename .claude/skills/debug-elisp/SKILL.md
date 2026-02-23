---
name: debug-elisp
description: Diagnose and fix Elisp errors, performance issues, slow startup, memory problems, or byte-compilation warnings. Use when encountering errors, backtraces, or performance regressions. Triggers on: "backtrace", "wrong-type-argument", "void-function", "void-variable", "byte-compile error", "performance regression", "memory leak", "slow", "debug elisp", "elisp error".
---

# Debug Elisp Issue

Diagnose and fix the reported issue using these procedures. $ARGUMENTS

## Diagnostic Workflow

1. **Reproduce** - Identify the exact error or symptom
2. **Measure baseline** - Record current metrics before changes
3. **Diagnose** - Use profiler/debugger to find root cause
4. **Fix** - Make targeted changes
5. **Verify** - Measure improvement, run `just test-smoke`

## Error Diagnosis

For errors and backtraces:

```elisp
;; Enable debugging
(setq debug-on-error t)
(setq debug-on-quit t)
```

- Analyze stack frames to identify error origin
- Check function arguments and variable states
- Use `edebug-defun` to step through suspicious functions
- Check `*Messages*` buffer for clues

## Startup Performance

```bash
# Measure startup time
emacs -Q --eval "(benchmark-run 1 (load \"~/.config/emacs/init.el\"))"

# Profile with use-package statistics
;; Add to early-init.el temporarily:
(setq use-package-compute-statistics t)
;; Then check: M-x use-package-report
```

Targets: excellent < 0.5s, good < 1s, acceptable < 2s.

## CPU Profiling

```elisp
(profiler-start 'cpu)
;; Reproduce the slow operation
(profiler-report)
(profiler-stop)
```

## Memory Profiling

```elisp
(profiler-start 'mem)
;; Reproduce the issue
(profiler-report)
(garbage-collect)
```

Common memory leak sources: timer accumulation, buffer accumulation, large kill-ring, undo-tree growth, unbounded caches.

## Byte-Compilation Warnings

```bash
just compile  # Byte-compile all .el files
```

Common fixes:
- Add `lexical-binding: t` to file header
- `(declare-function fn "source-file")` for external functions
- `(defvar external-var)` for forward declarations
- Replace obsolete functions: check with `(fboundp 'new-function)`

## Performance Anti-Patterns

- `add-hook 'post-command-hook` with expensive functions (runs constantly)
- Rebuilding expensive data structures in loops instead of caching
- Missing `:defer t` on use-package forms
- Heavy computation in `find-file-hook` without guards

## Project-Specific Validation

After any fix:
```bash
just test-smoke    # Ultra-fast smoke tests
just compile       # Check for byte-compilation warnings
just test-fast     # Fast unit tests
```
