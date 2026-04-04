---
name: elisp-conventions
description: >
  Enforces modern, idiomatic Emacs Lisp conventions in all generated code. Use this
  skill whenever writing, reviewing, or modifying Elisp — including use-package
  declarations, mode hooks, defcustom definitions, autoloads, keybindings, advice,
  minor-mode definitions, or any .el file. Also trigger when the user asks about
  Elisp best practices, code style, or says something looks "wrong" in their config.
  If you are about to write even a single line of Elisp, consult this skill first.
---

# Elisp Conventions

Enforces modern, idiomatic Emacs Lisp for Emacs 30+. All code must follow these conventions. When reviewing existing code, flag violations and suggest corrections.

## File Structure

Every `.el` file must have:

1. **Line 1:** `;;; filename.el --- Description -*- lexical-binding:t; -*-`
2. **Header:** Author, URL (optional), `;;; Commentary:` section
3. **Code section:** `;;; Code:` marker
4. **Footer:** `(provide 'feature-name)` followed by `;;; filename.el ends here`

The feature name in `provide` must match the filename (without `.el`).

## Modern APIs (Emacs 30+)

### Keybindings: `keymap-set` not `define-key`

```elisp
;; WRONG — legacy API, old key syntax
(define-key my-map "\C-c\C-a" #'my-func)
(global-set-key (kbd "C-c a") #'my-func)

;; RIGHT — modern API, key-valid-p strings
(keymap-set my-map "C-c C-a" #'my-func)
(keymap-global-set "C-c a" #'my-func)
```

Key syntax must satisfy `key-valid-p`. Modifier order: `A-C-H-M-S-s`.

Use `defvar-keymap` for mode keymaps:
```elisp
(defvar-keymap jotain-mode-map
  :doc "Keymap for jotain mode."
  "C-c j a" #'jotain-action
  "C-c j s" #'jotain-search)
```

### Options: `setopt` not `setq` for defcustom variables

```elisp
;; WRONG — doesn't call :set function, no type validation
(setq fill-column 100)

;; RIGHT — calls :set, validates :type
(setopt fill-column 100)
```

Use `setq` only for `defvar` variables and local bindings. Use `setopt` for anything declared with `defcustom`.

### Loading: `with-eval-after-load` not `eval-after-load`

```elisp
;; WRONG — function, needs quoting
(eval-after-load 'org '(setq org-startup-indented t))

;; RIGHT — macro, implicit progn
(with-eval-after-load 'org
  (setopt org-startup-indented t))
```

### Built-in without require (Emacs 30)

These are available without `(require ...)`:
- `when-let*`, `if-let*`, `and-let*` — conditional binding
- `string-trim`, `string-join`, `string-pad` — string utilities
- `named-let` — named let for recursion
- `seq-*` — sequence operations (map, filter, reduce, find, etc.)
- `use-package` — package configuration macro

These still need require:
- `(require 'subr-x)` for `thread-first`, `thread-last`
- `(require 'map)` for `map-elt`, `map-let`, `map-keys`
- `(require 'cl-lib)` for `cl-loop`, `cl-defstruct`, `cl-destructuring-bind`

Never use `(require 'cl)` — it's in `obsolete/` and will be removed.

## Hooks

Always use named functions, never lambdas:

```elisp
;; WRONG — can't remove, can't debug, duplicates on re-eval
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; RIGHT — removable, debuggable, advisable, no duplicates
(defun jotain-enable-line-numbers ()
  "Enable line numbers in programming modes."
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook #'jotain-enable-line-numbers)
```

The DEPTH parameter (Emacs 28+) controls ordering: -100 (earliest) to 100 (latest), default 0.

## Naming

- All symbols use `jotain-` prefix (matching the project)
- Internal/private functions: double-hyphen (`jotain-telemetry--flush-queue`)
- Interactive commands: single-hyphen for verb (`jotain-telemetry-flush`)
- Predicates: `-p` suffix (`jotain-telemetry--active-p`)
- Hooks: `-hook` suffix, functions: `-function`/`-functions` suffix

## defcustom

Every user-facing option must be a `defcustom` with `:type` and `:group`:

```elisp
(defcustom jotain-feature-enabled t
  "When non-nil, feature is active."
  :type 'boolean
  :group 'jotain-feature)
```

Common types: `boolean`, `integer`, `natnum`, `string`, `regexp`, `file`, `directory`, `symbol`, `function`, `face`, `(choice ...)`, `(repeat TYPE)`, `(alist ...)`.

## use-package Patterns

```elisp
;; Built-in package (no :ensure)
(use-package emacs
  :custom
  (fill-column 100)
  :bind
  ("M-o" . other-window))

;; External package from elpa (Nix manages installation, no :ensure t)
(use-package nix-mode
  :mode "\\.nix\\'")

;; Custom jotain module
(use-package jotain-telemetry
  :custom
  (jotain-telemetry-enabled t)
  :config
  (jotain-telemetry-mode 1))
```

In jotain's Nix-managed setup, never use `:ensure t` — Nix handles package installation.

## Tree-sitter Modes

Use `*-ts-mode` variants and `major-mode-remap-alist`:
```elisp
(setopt major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)))
```

## Common Mistakes

See `references/antipatterns.md` for a complete before/after reference.
