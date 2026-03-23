# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Jotain is a modular Emacs configuration managed with Nix Flakes. All Emacs packages are installed via Nix — never via MELPA/ELPA at runtime. The project exposes a `homeModules.default` for Home Manager integration and a `nixosModules.default` for NixOS.

**Current state:** Skeleton for rewrite. Two-directory split active: `lisp/` for built-in Emacs features (named `jotain-<feature>.el`), `modules/` for third-party package configurations.

## Commands

All commands are run via `just` (see `justfile`). The dev shell is managed by [devenv](https://devenv.sh/) — enter it via `nix develop` or direnv.

### Testing

```bash
just test-tag TAG        # Fastest: run ERT tests by tag directly (smoke, fast, unit)
just test-smoke          # Nix-sandboxed smoke tests (< 1s)
just test-fast           # Nix-sandboxed fast tests (< 5s)
just test                # Full ERT test suite via Nix
just test-all            # All checks (formatting + all ERT)
```

### Building & Checking

```bash
just compile             # Byte-compile all .el files directly (fastest feedback)
just build               # Build Emacs package with Nix
just check               # Syntax check via Nix dry-run
just format              # Format all files via treefmt-nix (nix fmt)
just check-instant       # Formatting + smoke only
just check-fast          # Above + fast unit tests
just check-parallel      # All checks in parallel (fastest on multi-core)
```

### Development

```bash
just emacs-dev           # Run Emacs with project config in isolated .dev-home (safe)
just emacs-clean         # Run without isolation (use with caution)
just clean               # Remove .elc, caches, and .dev-home
```

### Nix

```bash
nix develop              # Enter dev shell
nix run                  # Try Jotain in an isolated temp environment
nix flake update         # Update all flake inputs
```

## Architecture

### Elisp modules

Two directories, each file covers a functional domain:

- **`lisp/`** — Built-in Emacs features, named `jotain-<feature>.el`. All `lisp/` modules may depend on each other.
- **`modules/`** — Third-party packages only. May depend on `lisp/` but not on other `modules/` files.

**Module conventions:** Each module must `(provide 'module-name)` at the end. Load via `(require 'module-name)` in `init.el`. Load order matters. Dependencies must be documented.

### Nix build system

```
flake.nix                  # Entry point, uses flake-parts + emacs-overlay + devenv
├── devenv.nix             # Dev shell config (packages, scripts, env)
├── emacs.nix              # Builds Emacs 30 (PGTK) with all packages + runtime deps
├── nix/
│   ├── package.nix        # jotain package (copies init.el + early-init.el, defines test targets)
│   ├── lib/
│   │   ├── default.nix    # Lib entry point (exposes dependencies + runtimeDeps)
│   │   ├── dependencies.nix   # Auto-extracts packages from use-package declarations
│   │   └── runtime-deps.nix   # LSP servers, fonts, CLI tools, tree-sitter grammars
│   ├── overlays/default.nix   # Overlay: exposes jotain + jotainEmacs on pkgs
│   └── modules/
│       ├── home/default.nix   # Home Manager module (programs.jotain options)
│       └── nixos/default.nix  # NixOS module (system-wide installation)
```

**Dependency auto-extraction:** `nix/lib/dependencies.nix` scans all `.el` files for `(use-package PACKAGE-NAME` declarations and maps them to nixpkgs emacsPackages. To exclude a package from auto-extraction:
- `:ensure nil` — package is provided by another mechanism
- `:nodep` — custom marker, package should not be Nix-installed
- Add to `packageNameMap` with `null` value — for Emacs built-ins (e.g., `project`, `diff-mode`)

**Test targets** are defined as `passthru` attributes on the jotain package in `nix/package.nix` (not in `nix/checks/`). The flake wires them as `checks.<system>.{smoke-test,fast-tests,tests}`.

### Adding packages

1. Add a `use-package` configuration in the appropriate `lisp/*.el` or `modules/*.el` file (auto-extracted if name matches)
2. For name mismatches, add a manual mapping in `nix/lib/dependencies.nix` `packageNameMap`
3. For packages not in nixpkgs, add a `trivialBuild` in `nix/overlays/default.nix`
4. For runtime tools (LSP servers, CLI tools, fonts), add to `nix/lib/runtime-deps.nix`

### Formatting

`treefmt-nix` configured in `flake.nix`:
- `nixpkgs-fmt` for `.nix` files
- `shfmt` + `shellcheck` for shell scripts
- Emacs built-in `indent-region` for `.el` files

### Testing architecture

- ERT tests live in `tests/`, auto-discovered by `tests/test-all.el`
- Tests are tagged: `smoke`, `fast`, `unit`, `integration`, `slow`
- Tests run in an isolated home (`HOME=.dev-home`)
- `test-helpers.el` mirrors `early-init.el` settings (use-package, tree-sitter)
- `just test-tag TAG` runs tests directly using Emacs from the dev shell — fastest loop

### Home Manager module options

| Option | Default | Description |
|--------|---------|-------------|
| `programs.jotain.enable` | `false` | Enable Jotain |
| `programs.jotain.package` | `pkgs.jotain` | The Jotain configuration package |
| `programs.jotain.enableDaemon` | `true` | Run Emacs as systemd/launchd service |
| `programs.jotain.includeRuntimeDeps` | `true` | Install LSP servers, fonts, CLI tools |
| `programs.jotain.extraPackages` | `epkgs: []` | Add extra Emacs packages |
