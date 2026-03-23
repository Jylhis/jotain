# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Jotain is a modular Emacs distribution managed with Nix Flakes. All Emacs packages are installed via Nix — never via MELPA/ELPA at runtime. The project exposes a `homeModules.default` for Home Manager integration.

**Current state:** Skeleton for rewrite. `elisp/` is empty; modules are being written fresh.

## Commands

All commands are run via `just` (see `justfile`). Enter the dev shell with `nix develop` to get all project tools.

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

### Elisp modules (`elisp/`)

Each file covers a functional domain. Load order in `init.el` matters.

Add new modules here as `elisp/<name>.el`.

**Module conventions:** Each module must `(provide 'module-name)` at the end. Load via `(require 'module-name)` in `init.el`. Modules can depend on earlier ones.

### Nix build system

```
flake.nix                  # Entry point, uses flake-parts + emacs-overlay
├── emacs.nix              # Builds Emacs 30 (PGTK) with all packages + runtime deps
│   ├── nix/lib/dependencies.nix   # Auto-extracts packages from use-package declarations
│   └── nix/lib/runtime-deps.nix   # LSP servers, fonts, CLI tools, tree-sitter grammars
├── default.nix            # jotain package (copies init.el + early-init.el)
├── shell.nix              # Dev shell (emacs-dev wrapper, LSP servers, formatters)
├── nix/overlays/default.nix  # Overlay: custom Emacs package overrides
├── nix/modules/home/default.nix  # Home Manager module (programs.jotain options)
└── nix/modules/nixos/             # NixOS module (nixosModules.default)
```

**Dependency auto-extraction:** `nix/lib/dependencies.nix` scans all `.el` files for `(use-package PACKAGE-NAME` declarations and maps them to nixpkgs emacsPackages.

### Adding packages

1. Add a `use-package` configuration in the appropriate `elisp/*.el` file (auto-extracted if name matches)
2. For name mismatches, add a manual mapping in `nix/lib/dependencies.nix`
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
