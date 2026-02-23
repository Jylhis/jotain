---
name: check-config
description: Validate jotain Emacs configuration - byte-compilation, load order, and common issues. Triggers on: "validate config", "check config", "lint config", "broken config", "config broken", "before commit", "is config valid", "verify setup".
disable-model-invocation: true
allowed-tools: Bash, Read, Grep, Glob
---

# Check Configuration

Run a comprehensive validation of the jotain Emacs configuration.

## Checks

1. **Byte-compilation**:
   ```bash
   just compile
   ```
   Report any warnings (missing declarations, obsolete functions, unbound variables).

2. **Smoke tests**:
   ```bash
   just test-smoke
   ```

3. **Fast unit tests**:
   ```bash
   just test-fast
   ```

4. **Nix syntax check**:
   ```bash
   just check
   ```

5. **Formatting**:
   ```bash
   just format
   ```

6. **Manual checks**:
   - Verify all `elisp/*.el` files have `lexical-binding: t` in the header
   - Check for `use-package` forms missing `:defer`/`:hook`/`:commands`/`:after`
   - Check for potential circular `:after` dependencies

## Report Format

Summarize results as:
- Passed checks
- Warnings (non-blocking)
- Errors (must fix)
- Recommendations
