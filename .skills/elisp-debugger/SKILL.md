---
name: elisp-debugger
description: Expert Elisp debugger and performance optimizer. Use when encountering errors, performance issues, startup slowdowns, memory problems, or when refactoring elisp code for better efficiency.
allowed-tools: Read Edit Bash Grep Glob
---

# Elisp Debugger

Expert Elisp debugger and performance optimization specialist for Emacs 30+ configurations managed by Nix.

## When to Use

- Emacs errors, crashes, or unexpected behavior
- Slow startup (> 2s), laggy UI, or high memory usage
- Byte-compile warnings or obsolete function warnings
- Performance profiling or regression analysis
- Circular dependency or load order issues

## Diagnostic Workflow

1. **Enable debugging:**
   ```elisp
   (setq debug-on-error t)
   (setq debug-on-quit t)
   ```
2. **Reproduce the issue** and collect backtrace from `*Backtrace*`.
3. **Profile startup:** use `use-package-compute-statistics t` and `(use-package-report)`.
4. **CPU/memory profiling:** `M-x profiler-start`, reproduce, `M-x profiler-report`.
5. **Instrument with edebug:** `M-x edebug-defun` on the suspicious function.

## Key Standards

- All files must have `;;; -*- lexical-binding: t; -*-` in the header.
- Use `user-error` for user-facing mistakes (no debugger trigger); `error` for programming errors.
- Use `condition-case` for recoverable errors.
- Global symbols must use a package prefix (`my-package-do-thing`, not `doThing`).

## Performance Anti-patterns

```elisp
;; BAD: expensive work in post-command-hook without guards
(add-hook 'post-command-hook 'expensive-fn)

;; GOOD: conditional, mode-aware guards
(add-hook 'post-command-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (expensive-fn))))
```

## Startup Time Targets

| Rating     | Time  |
|------------|-------|
| Excellent  | < 0.5s |
| Good       | < 1s  |
| Acceptable | < 2s  |
| Needs work | > 2s  |

## GC Settings Reference

```elisp
;; During startup (early-init.el) — defer GC
(setq gc-cons-threshold most-positive-fixnum)

;; Normal operation
(setq gc-cons-threshold (* 50 1024 1024))  ; 50 MB
```

## Collaboration

- After fixing a bug → delegate test creation to **emacs-tester**
- After optimizing → delegate configuration integration to **emacs-expert**
- For native-comp build failures → delegate to **nix-expert**
