---
name: jotain-architecture
description: >
  Encodes jotain's module architecture and conventions for generating correctly
  structured configuration code. Use this skill whenever creating new jotain modules,
  modifying existing ones, adding Emacs packages to jotain, or discussing jotain's
  file layout. Also trigger when the user mentions jotain by name, references their
  Emacs config, or asks to scaffold a new feature module. Produces code that fits
  jotain's directory structure, Nix wiring, and Elisp conventions.
---

# Jotain Architecture

Jotain is a custom GNU Emacs configuration built from scratch with Nix. The name is Finnish for "something."

## Project Structure

```
jotain/
├── early-init.el          # Emacs 30+ early init (before packages/GUI)
├── init.el                # Main entry point (use-package declarations)
├── custom.el              # Emacs Customize output (auto-generated)
├── lisp/                  # Custom Elisp modules
│   └── jotain-*.el        # Each module is a separate file
├── test/                  # ERT tests
│   └── jotain-*-test.el   # One test file per module
├── default.nix            # Distribution: Emacs + packages + 275 tree-sitter grammars
├── emacs.nix              # Emacs build from source (5 variants, ~30 flags)
├── nix/                   # Auxiliary Nix expressions
├── Justfile               # Task runner (build, run, clean, test)
└── docs/                  # Documentation and research
```

## How Modules Work

### Creating a new module

1. Create `lisp/jotain-<name>.el` following the template in `references/module-template.md`
2. Create `test/jotain-<name>-test.el` with ERT tests
3. Add `(use-package jotain-<name> ...)` to `init.el`
4. Run `just test` to verify

The `load-path` is set in `init.el`:
```elisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
```

### Adding an external package

1. **Check if built-in** (Emacs 30 has use-package, eglot, which-key, etc.)
2. **Add to `default.nix`** in the `withPackages` list:
   ```nix
   (pkgs.emacsPackagesFor emacs).withPackages (epkgs: [
     (allTreeSitterGrammars epkgs)
     epkgs.magit          # ← add here
   ])
   ```
3. **Configure in `init.el`** with `use-package` (no `:ensure t`)
4. **Rebuild:** `just run` (builds then launches)

### The Nix↔Elisp boundary

**Nix handles:** package installation, `load-path` setup, native deps, tree-sitter grammars, build configuration, `site-start.el` generation

**Elisp handles:** runtime configuration, hooks, keybindings, themes, mode activation, advice

Never use `package-install`, `straight.el`, `:ensure t`, or `treesit-install-language-grammar` — Nix manages all of this.

## Build Variants

`emacs.nix` supports: `mainline` (30.2, default), `git` (master), `unstable` (latest tag from git), `macport` (macOS), `igc` (incremental GC). See `Justfile` for platform-specific targets.

## Testing

```bash
just test  # runs: emacs --batch -L lisp -L test -l test/<file>.el -f ert-run-tests-batch-and-exit
```

Tests use state isolation macros (see `test/jotain-telemetry-test.el` for the pattern).

## Conventions

- `lexical-binding: t` on every `.el` file
- `jotain-` prefix on all custom symbols
- Double-hyphen for internal functions
- Named functions in hooks (never lambdas)
- `defcustom` with `:type` and `:group` for user options
- `setopt` for defcustom vars
- `use-package` for all package configuration
- `next` branch for development, `main` for stable
