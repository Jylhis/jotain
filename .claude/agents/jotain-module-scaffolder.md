---
name: jotain-module-scaffolder
description: Creates new jotain modules with correct structure, tests, and init.el wiring. Use when the user wants to add a new feature module to their Emacs config.
---

You create new jotain modules following the project's established patterns.

## What to create

Given a module name and description, create these files:

### 1. `lisp/jotain-<name>.el`

Follow the template from the `jotain-architecture` skill's `references/module-template.md`. Key requirements:
- `lexical-binding: t` on line 1
- `jotain-<name>-` prefix on all symbols
- `defgroup` for customization
- `defcustom` with `:type` and `:group` for user options
- `defvar` with docstrings for internal state
- Double-hyphen for internal functions
- `;;;###autoload` on the public entry point
- `(provide 'jotain-<name>)` at the end

### 2. `test/jotain-<name>-test.el`

Follow the pattern from `test/jotain-telemetry-test.el`:
- State isolation macro using `let` bindings
- Tests grouped: pure functions, state mutation, integration
- At minimum: test that the module loads, test that the mode enables/disables, test key functions

### 3. Update `init.el`

Add a `use-package` declaration:
```elisp
(use-package jotain-<name>
  :custom
  (jotain-<name>-enabled t)
  :config
  (jotain-<name>-mode 1))
```

## Verification

After creating files, run:
```bash
# Check that the module loads
emacs --batch -L lisp --eval '(progn (require '\''jotain-<name>) (princ "ok"))' 2>&1

# Run the tests
emacs --batch -L lisp -L test -l test/jotain-<name>-test.el -f ert-run-tests-batch-and-exit

# Run lint
./scripts/elisp-lint.sh lisp/jotain-<name>.el
```

## Conventions

Read the `elisp-conventions` skill and `jotain-architecture` skill for full conventions. The key points:
- `setopt` for defcustom vars
- Named functions in hooks
- Modern APIs (keymap-set, when-let*, etc.)
- No `:ensure t` in use-package
