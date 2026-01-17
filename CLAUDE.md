# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

```bash
# Enter development shell (required - provides isolation)
nix develop

# Quick feedback loop
just test-smoke       # Ultra-fast smoke tests (< 1 second)
just test-fast        # Fast unit tests (< 5 seconds)
just test             # Full ERT test suite
just test-all         # ERT + NMT module tests (excludes VM)

# Run tests by tag
just test-tag smoke   # Only smoke tests
just test-tag unit    # Only unit tests
just test-tag integration  # Only integration tests

# Build and validate
just build            # Build Emacs package
just check            # Nix dry-run check
nix flake check       # All checks (same as just test-all)

# Interactive development
just emacs-dev        # Run Emacs with project config (isolated, safe)
just compile          # Byte-compile all .el files

# Maintenance
just clean            # Remove .elc files and .dev-home/
nix fmt               # Format Nix files
just update           # Update all flake inputs
```

## Architecture Overview

Modular Emacs 30+ configuration using Nix for reproducible builds.

### Directory Structure
- **elisp/**: Feature modules (core, ui, completion, programming, git, etc.)
- **tests/**: ERT unit tests (test-*.el)
- **nmt-tests/**: Nix module integration tests
- **nix/lib/**: Runtime dependencies and helpers
- **nix/modules/**: Home-manager and NixOS modules

### Module Loading Order (init.el)
1. `platform` - OS detection (must be first)
2. `core` - Fundamental Emacs settings
3. `fonts`, `ui` - Appearance
4. `completion` - Vertico, Consult, Corfu
5. `programming` - Eglot, treesit, development tools
6. `per-project`, `writing`, `git`, `help`, `ai`, `systems`
7. `platforms` - Platform-specific adaptations
8. `android` - Conditional, only on Android
9. `app-launchers` - Application launcher utilities

### Key Design Patterns

**Platform Detection**: `elisp/platform.el` sets flags like `platform-android-p`, `platform-macos-p`. Must load first.

**use-package Convention**: All modules use `use-package` with `:init`, `:config`, `:custom`, `:bind`, `:hook` sections.

**Nix Integration**:
- `default.nix` - Builds Emacs with packages via `emacsWithPackages`
- `emacs.nix` - Wraps Emacs with runtime deps (LSP, CLI tools, tree-sitter)
- `config.nix` - Creates deployment package, filters out dev files
- `nix/lib/runtime-deps.nix` - Centralized runtime dependency definitions

### Flake Outputs
- `packages.emacs` - Emacs with all packages and runtime deps
- `packages.jotain` - Configuration files only
- `homeModules.default` - Home-manager module
- `devShells.default` - Development environment with isolation

## Adding Packages

1. Add package to `epkgs` list in `default.nix`
2. Add `use-package` configuration in appropriate `elisp/*.el` file
3. Run `just test` to verify

For runtime dependencies (LSP servers, CLI tools, fonts, tree-sitter grammars), add to `nix/lib/runtime-deps.nix`.

## Testing

### Test Tiers
- **smoke**: Must pass, < 1 second, no I/O - `:tags '(smoke critical)`
- **fast**: Unit tests, minimal I/O - `:tags '(fast unit)`
- **unit**: Standard unit tests - `:tags '(unit)`
- **integration**: Loads packages/config - `:tags '(integration slow)`

### Adding Tests
1. Create `tests/test-feature.el` with appropriate tags
2. Tests auto-load via `tests/test-all.el`
3. Naming: `(ert-deftest test-module/feature () ...)`

### NMT Tests (nmt-tests/)
Test home-manager module behavior. Run with `just test-nmt`.

### Runtime Validation
`just test-runtime` - Starts VM, validates daemon/client. Slow but comprehensive.

## Core Keybindings

**Navigation**: `C-x j` toggle split, `C-c u/r` winner undo/redo, `C-x t t/k` tabs
**Files**: `C-x C-f` find file, `C-x b` switch buffer, `C-x p f` project file
**Git**: `C-x g` magit status, `C-c g` git commands
**Themes**: `C-c t` toggle light/dark modus themes

## Development Environment Isolation

`nix develop` sets `HOME=$PWD/.dev-home` to protect your personal Emacs config. All `just` commands respect this isolation. Use `just emacs-dev` for safe interactive testing.

## System Prerequisites

**git** is required but NOT bundled (configure separately via `programs.git`). Other tools (ripgrep, fd, direnv, LSP servers) are provided by `includeRuntimeDeps` option.

## Home-manager Installation

```nix
{
  inputs.jotain.url = "github:Jylhis/jotain";
  # In home-manager config:
  imports = [ inputs.jotain.homeModules.default ];
  programs.jotain = {
    enable = true;
    # enableDaemon = true;        # Default: systemd service
    # includeRuntimeDeps = true;  # Default: LSP, fonts, CLI tools
  };
}
```