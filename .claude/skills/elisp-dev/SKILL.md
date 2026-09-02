---
name: elisp-dev
description: Modern Emacs Lisp development practices (Emacs 30/31) — coding and naming conventions, lexical binding, macro hygiene, hooks vs. advice, defcustom/setopt, use-package patterns, modern libraries (pcase/seq/map/cl-lib/rx), debugging, and ERT testing. Use when writing or reviewing any Elisp in this repo (lisp/init-*.el, early-init.el, init.el, test/).
---

# Emacs Lisp development

Practices distilled from the GNU Elisp Reference Manual "Tips and Conventions"
appendix, the official style guidance, and this repo's own conventions
(CLAUDE.md / AGENTS.md). Repo rules win where they overlap.

## Hard rules for this repo

1. Every file starts with `;;; file.el --- desc -*- lexical-binding: t; -*-`
   and modules end with `(provide 'init-<concern>)`.
2. `setopt` for `defcustom` variables, `setq` only for plain `defvar`s.
3. `use-package` blocks: built-ins and Nix-provided packages get
   `:ensure nil` (because `use-package-always-ensure` is `t`).
4. No builtins/third-party split — a package enhancing a built-in lives in
   the same `lisp/init-*.el` module as the built-in.
5. Byte-compilation is a gate: the `elisp-compile` flake check runs with
   `byte-compile-error-on-warn`, so any warning fails CI (run it via
   `just check`, or alone with
   `nix build --no-link .#checks.x86_64-linux.elisp-compile`). Write
   warning-clean code (declare functions, require at compile time, no
   obsolete APIs).
6. LSP (`eglot-ensure` hooks) and formatters (apheleia) are centralised in
   `lisp/init-prog.el` — don't scatter them into language modules.

## Reference files

Read the one(s) relevant to the task:

| File | Covers |
|------|--------|
| `references/conventions.md` | Naming (`foo-` public, `foo--` private), library headers, autoload cookies, docstring conventions, key-binding conventions, comment style |
| `references/lexical-binding-and-macros.md` | Lexical vs. dynamic binding, closures, special variables; macro hygiene (gensym, evaluation-count pitfalls, `declare indent/debug`), `eval-when-compile` vs `eval-and-compile` |
| `references/hooks-advice-loading.md` | Hooks (`add-hook` depth, mode hooks, derived-mode conventions), advice combinators and why hooks beat advice, autoloads, `with-eval-after-load` pitfalls, load-order reasoning |
| `references/customization.md` | `defcustom` `:type`/`:set`/`:initialize`, why `setopt` matters, buffer-local options, themes vs. custom |
| `references/modern-libraries.md` | `pcase`/`pcase-let`, `seq`, `map`, `cl-lib`, `rx`, threading macros, `named-let`, generalized variables (`setf`), string utilities |
| `references/debugging-and-testing.md` | edebug, `debug-on-error`, `M-x profiler`, benchmarking, ERT patterns (fixtures, `should` forms, `ert-deftest` naming, running via `just test`) |
| `references/emacs-30-31-changes.md` | Emacs 30/31 NEWS filtered for config authors — new APIs to adopt, obsoletions to avoid, version-gating |

## Workflow in this repo

- New module: create `lisp/init-<concern>.el` with the file shape above, add
  `(require 'init-<concern>)` to `init.el` at the right load point.
- Validate (no Emacs in the dev shell): `just check` runs every
  flake check; individually, `elisp-lint` is the paren/syntax check,
  `elisp-compile` the warnings-as-errors byte-compile, and `just test`
  builds `elisp-test` — ERT over every `*.el` under `test/` (any name,
  not just `test-*.el`), loaded with `-L lisp`.
- Run isolated: `just run-built` (builds Emacs via Nix, uses
  `--init-directory`, never touches `~/.emacs.d`);
  `just run-built-debug` for `--debug-init`.
