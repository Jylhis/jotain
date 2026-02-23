---
name: create-module
description: Scaffold a new elisp/*.el module for the jotain Emacs distribution with proper headers, structure, and test stub. Triggers on: "new module", "create module", "new elisp file", "add module", "scaffold module", "new el file", "new domain", "create elisp".
argument-hint: "[module-name and domain description]"
---

# Create New Emacs Module

Scaffold a new `elisp/*.el` module for jotain. $ARGUMENTS

## Step 1: Decide — Extend Existing vs. New Module

**Extend an existing module** when the new functionality fits naturally into an existing domain:

| Domain | File | Add here if... |
|--------|------|----------------|
| Editor settings, keybindings | `core.el` | Basic Emacs behavior |
| Themes, fonts, UI tweaks | `ui.el` or `fonts.el` | Visual appearance |
| Completion, minibuffer | `completion.el` | Vertico/Consult/Corfu ecosystem |
| Language support, LSP | `programming.el` | Coding tools, tree-sitter |
| Version control | `git.el` | Magit or VC-related |
| Notes, documentation | `writing.el` | Org-mode ecosystem |
| OS/platform differences | `platforms.el` | Linux/macOS/Android |
| Project configuration | `per-project.el` | Dir-locals patterns |

**Create a new module** only when functionality genuinely doesn't fit any existing domain and warrants its own file (e.g., a new major subsystem like `ai.el` or `mail.el`).

## Step 2: Create the Module File

File: `elisp/MODULE-NAME.el`

```elisp
;;; MODULE-NAME.el --- BRIEF DESCRIPTION -*- lexical-binding: t; -*-

;; Copyright (C) YEAR  AUTHOR

;;; Commentary:
;; DESCRIPTION OF WHAT THIS MODULE DOES AND WHY IT EXISTS.
;; Cross-references: depends on core.el, related to X.

;;; Code:

(require 'some-dependency-if-needed)

;; Configuration

(defgroup jotain-MODULE-NAME nil
  "DESCRIPTION for `MODULE-NAME'."
  :group 'jotain)

;; Main content: use-package forms and configuration

(use-package some-package
  :ensure
  :defer t
  :config
  (setq some-package-setting t))

;; Public API (use jotain- prefix)

(defun jotain-MODULE-NAME-do-thing ()
  "Do the thing."
  (interactive)
  ...)

(provide 'MODULE-NAME)
;;; MODULE-NAME.el ends here
```

Key requirements:
- `lexical-binding: t` in the first line — mandatory
- All public symbols use `jotain-` prefix
- Private/internal symbols use `jotain-MODULE-NAME--` (double dash)
- Every `use-package` form must have a deferral mechanism: `:defer t`, `:hook`, `:commands`, or `:after`
- `(provide 'MODULE-NAME)` at the end

## Step 3: Wire into init.el

Add a `(require 'MODULE-NAME)` line to `init.el` in load order position. Order matters — load after dependencies:

```elisp
;; In init.el, after the modules it depends on:
(require 'MODULE-NAME)
```

Check `init.el` to find the right position based on dependencies.

## Step 4: Handle Nix Package Dependencies

If the module adds new packages with `:ensure`:

- `nix/lib/dependencies.nix` **auto-extracts** them — no action needed if the name maps directly to nixpkgs
- If the package name differs from the nixpkgs attribute, add a manual mapping in `nix/lib/dependencies.nix`
- If the module needs runtime tools (LSP servers, CLI tools, fonts), add to `nix/lib/runtime-deps.nix`

Verify with:
```bash
just check    # Nix dry-run to confirm packages resolve
just build    # Full build (slower)
```

## Step 5: Create Test Stub

File: `tests/test-MODULE-NAME.el`

```elisp
;;; test-MODULE-NAME.el --- Tests for MODULE-NAME -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for elisp/MODULE-NAME.el

;;; Code:

(require 'ert)
(require 'test-helpers)

(ert-deftest test-MODULE-NAME/basic-sanity ()
  "MODULE-NAME module loads without errors."
  :tags '(smoke)
  (should t))  ; Replace with actual assertions

;; Add meaningful tests here following the write-tests skill

(provide 'test-MODULE-NAME)
;;; test-MODULE-NAME.el ends here
```

## Step 6: Validate

```bash
just compile       # Byte-compile — fix all warnings
just test-smoke    # Must pass (< 1s)
just test-fast     # Should pass (< 5s)
```

## Checklist

- [ ] File has `lexical-binding: t` header
- [ ] Module name in `provide` matches filename
- [ ] All public symbols use `jotain-` prefix
- [ ] All `use-package` forms have lazy loading
- [ ] `require` added to `init.el` in correct position
- [ ] Test stub created in `tests/`
- [ ] `just compile` passes with no warnings
- [ ] `just test-smoke` passes

## See Also

- `/add-package` — adding a package to an existing module
- `/write-tests` — writing comprehensive ERT tests
- `/emacs-expert` — coding conventions (naming, docstrings, error handling)
