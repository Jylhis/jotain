# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

Build and test Emacs configuration:
```bash
# Check configuration syntax (fast validation)
just check

# Run all unit tests
just test

# Run tests with verbose output for debugging
just test-verbose

# Byte-compile all Emacs Lisp files
just compile

# Clean compiled files and cache
just clean

# Start Emacs with clean config (for testing)
just emacs-clean
```

Development workflow:
```bash
# Enter Nix development shell (required)
nix develop

# Run tests after changes
just test

# Check specific file syntax
emacs -Q --batch -l <file.el>
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

## Testing Approach

Tests use ERT (Emacs Lisp Regression Testing) framework:
- Test files in `tests/` directory
- Named `test-*.el`
- Run with `just test` or individual test files with `emacs -Q --batch -l tests/test-file.el -f ert-run-tests-batch-and-exit`

When adding features:
1. Add tests in tests/test-feature.el
2. Update justfile test command if new test file created
3. Ensure tests pass with `just test`