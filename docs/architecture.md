# Jotain Architecture

## Overview

Jotain is a modular Emacs 30 configuration built on two principles: prefer Emacs built-ins, and let Nix handle package management. The result is a configuration that works reproducibly on NixOS and degrades gracefully on non-Nix systems.

## Module System

### Directory Layout

- **`lisp/`** — Built-in Emacs features extended via `use-package` with `:ensure nil`. Files are named `jotain-<feature>.el` after the Emacs domain they extend. Everything under `lisp/` is assumed always available.
- **`modules/`** — Third-party package configurations using `use-package` with `:ensure t`. Each package must have documented justification for why the built-in alternative is insufficient.

### Naming Convention

Following the pattern from [emacs-solo](https://github.com/LionyxML/emacs-solo), each module is named after what it extends:

| File | Extends |
|------|---------|
| `jotain-defaults.el` | Emacs default settings |
| `jotain-platform.el` | Environment/platform detection |

### Dependencies

- `lisp/` modules may depend on any other `lisp/` module. Dependencies must be documented via `(require 'dependency)` with a comment.
- `modules/` may depend on anything under `lisp/` but must not depend on other `modules/` files.
- Dependencies should fail gracefully if missing (use `require` with error handling where appropriate).

### Feature Evaluation Order

When adding a new capability:

1. Is there a built-in Emacs feature? Use it.
2. Can we extend the existing built-in? Add to the appropriate `lisp/jotain-<feature>.el`.
3. Should we write our own module? Create a new `lisp/jotain-<feature>.el`.
4. Is there a justified third-party package? Add to `modules/`.

## Nix Integration

### Dual-Mode Package Management

The variable `jotain/nix-managed-p` (set by checking `NIX_PROFILES`) controls package sourcing:

- **Nix-managed** (`t`): All packages come from the Nix store. No `package-archives` configured. `use-package-always-ensure` is nil. `use-package-ensure-function` is `ignore`.
- **Non-Nix** (`nil`): MELPA + GNU ELPA configured. `use-package-always-ensure` set to `t`. Packages auto-install on first launch.

### Automatic Dependency Extraction

`nix/lib/dependencies.nix` scans all `.el` files in `lisp/` and `modules/` for `(use-package PACKAGE-NAME` declarations and maps them to nixpkgs emacsPackages. Packages with `:ensure nil` or `:nodep` are excluded. Name mismatches are handled via `packageNameMap`.

### Flake and Non-Flake Support

The project exposes a Nix flake interface but also supports non-flake usage. The devenv development shell works without flakes enabled.

**Flake outputs:**
- `packages.<system>.jotain` — Configuration package
- `packages.<system>.emacs` — Emacs with all packages and runtime deps
- `homeModules.default` — Home Manager module
- `nixosModules.default` — NixOS module (system-wide)
- `apps.<system>.default` — Try jotain in an isolated temp environment

### Home Manager Options

| Option | Default | Description |
|--------|---------|-------------|
| `programs.jotain.enable` | `false` | Enable jotain |
| `programs.jotain.package` | `pkgs.jotain` | Override config package |
| `programs.jotain.enableDaemon` | `true` | Run as systemd service |
| `programs.jotain.includeRuntimeDeps` | `true` | Install LSP servers, fonts, CLI tools |
| `programs.jotain.extraPackages` | `epkgs: []` | Additional Emacs packages |

## Testing

Tests use ERT (Emacs Lisp Regression Testing) with a tagging system:

| Tag | Time | Purpose |
|-----|------|---------|
| `smoke` | < 1s | Critical sanity checks (files exist, version, basic ops) |
| `fast` | < 5s | Unit tests (Nix detection, platform, module loading) |
| `unit` | varies | Detailed unit tests |
| `integration` | varies | Cross-module and system integration |

**Running tests:**

```bash
just test-tag smoke     # Direct (fastest, dev shell)
just test-tag fast      # Direct
just test-smoke         # Nix-sandboxed
just test-fast          # Nix-sandboxed
just test               # Full suite (Nix-sandboxed)
```

Test sources are included in the Nix package via `lib.fileset` for efficient caching.

## Supported Platforms

| Platform | Status |
|----------|--------|
| Linux x86_64 | Primary |
| Linux aarch64 | Supported |
| Android aarch64 (Termux) | Supported (terminal-only) |

Platform detection via `jotain/platform` variable. Unsupported platforms load without errors; platform-specific features degrade gracefully.
