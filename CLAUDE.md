# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

Build and test Emacs configuration:
```bash
# Check configuration syntax (Nix dry-run)
just check

# Run ERT unit tests via Nix
just test

# Run all tests (ERT + NMT home-manager module tests)
just test-all

# Run NMT tests only (validates home-manager module)
just test-nmt

# Run tests with verbose output (direct execution)
just test-verbose

# Build Emacs package with Nix
just build

# Byte-compile all Emacs Lisp files (for development)
just compile

# Clean compiled files and cache
just clean

# Start Emacs with clean config (for testing)
just emacs-clean
```

Development workflow:
```bash
# Enter Nix development shell (recommended)
nix develop

# Run tests after changes
just test              # ERT unit tests
just test-nmt          # Home-manager module tests
just test-all          # All tests

# Nix operations
nix build .#default    # Build Emacs package
nix flake check        # Run all checks
nix fmt                # Format Nix files

# Update dependencies
just update            # Update all flake inputs
just update-input nixpkgs  # Update specific input
```

## Architecture Overview

This is a modular Emacs configuration using Nix for reproducible builds. The configuration follows a feature-based module system where each aspect is isolated in its own file.

### Module System
The configuration is split into logical modules loaded via `require` in init.el:
- **Core modules** (config/): Each handles a specific feature domain (completion, git, programming, etc.)
- **Utility libraries** (lisp/): Shared functionality and platform detection
- **Platform adaptations**: Automatic OS-specific configurations via platform.el detection

### Key Design Patterns
1. **Platform Detection**: The `platform.el` library detects the OS/environment and sets flags like `platform-android-p`, `platform-macos-p`, etc. Platform-specific code checks these flags.

2. **Lazy Loading**: Modules use `with-eval-after-load` and autoloads to defer package loading until needed.

3. **Feature Modules**: Each config/*.el file is self-contained and can be loaded independently. They all `(provide 'module-name)` at the end.

4. **Nix Integration**: The flake.nix and default.nix define the development environment with all required Emacs packages pre-installed.

### Module Dependencies
- `platform.el` must load first (provides OS detection)
- `core.el` sets fundamental Emacs defaults
- Other modules can load in any order but may have soft dependencies (e.g., completion enhances programming)

### Home Manager Module (module.nix)
The home-manager module handles deployment of the Emacs configuration:
- Uses `lib.fileset.toSource` to deploy configuration files to `~/.config/emacs`
- Configures systemd service for Emacs daemon with socket activation
- Sets up shell aliases (emc, emcg, emqg, emq)
- Installs Nerd Fonts and other font packages
- Controlled via `programs.emacs.enable` option
- Configuration path specified via `programs.emacs.userConfig` option

## Testing Approach

This project uses two testing frameworks:

### ERT (Emacs Lisp Regression Testing)
Unit tests for Emacs Lisp code:
- Test files in `tests/` directory
- Named `test-*.el`
- Run with `just test` (via Nix) or `just test-verbose` (direct execution)
- Tests are built into the Nix package and run during `nix flake check`

When adding ERT tests:
1. Add tests in tests/test-feature.el
2. Update default.nix passthru.tests if new test file created
3. Ensure tests pass with `just test`

### NMT (Nix Module Tests)
Integration tests for the home-manager module:
- Test files in `nmt-tests/` directory
- Validates module behavior by building actual home-manager configurations
- Run with `just test-nmt` or individual tests with `nix build .#checks.x86_64-linux.test-<name>`
- Available tests:
  - test-emacs-config-files: Validates file linking to ~/.config/emacs
  - test-shell-aliases: Validates shell alias configuration
  - test-emacs-service: Validates systemd service setup
  - test-font-packages: Validates font package installation
  - test-module-disabled: Validates behavior when disabled
  - test-fileset-source: Validates directory structure

When adding NMT tests:
1. Add test definition in nmt-tests/default.nix
2. Update justfile test-nmt command
3. Update nmt-tests/README.md with test description
4. Ensure tests pass with `just test-nmt`

### Running All Tests
Use `just test-all` or `nix flake check` to run both ERT and NMT tests.