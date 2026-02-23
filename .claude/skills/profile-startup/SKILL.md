---
name: profile-startup
description: Profile Emacs startup time and identify slow packages.
disable-model-invocation: true
context: fork
allowed-tools: Bash, Read, Grep, Glob
---

# Profile Startup

Analyze jotain Emacs startup performance and identify optimization opportunities.

## Procedure

1. **Measure baseline startup time**:
   ```bash
   just emacs-dev --eval '(message "Init time: %s" (emacs-init-time))' --eval '(kill-emacs)' 2>&1 | grep "Init time"
   ```

2. **Check use-package statistics** (if enabled):
   ```bash
   just emacs-dev --eval '(progn (use-package-report) (kill-emacs))' 2>&1
   ```

3. **Identify packages without lazy loading** - Grep for use-package forms missing `:defer`, `:hook`, `:commands`, or `:after`:
   - Search `elisp/*.el` for `(use-package` declarations
   - Flag any that lack deferral mechanisms

4. **Check early-init.el** for heavy operations that should be deferred

5. **Report findings**:
   - Current startup time
   - Top slow packages (if measurable)
   - Packages without lazy loading
   - Specific optimization recommendations

## Performance Targets

- Excellent: < 0.5s
- Good: < 1s
- Acceptable: < 2s
- Needs work: > 2s
