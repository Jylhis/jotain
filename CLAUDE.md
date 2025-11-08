# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Jotain is a NixOS-native Emacs distribution with **automatic dependency management**. The key innovation is that Nix automatically extracts Emacs package dependencies from `use-package` declarations in elisp files, eliminating manual dependency maintenance.

## Core Architecture

### 1. Automatic Dependency Extraction System

The heart of Jotain is `nix/lib/dependencies.nix`, which:
- Scans all `.el` files in `elisp/` for `use-package` declarations
- Uses Perl regex to extract package names
- Filters out packages marked with `:ensure nil` or `:nodep`
- Maps elisp package names to nixpkgs emacsPackages names
- Returns a list of Nix package derivations

**Critical**: When adding new elisp modules with `use-package`, dependencies are automatically detected. To mark a package as built-in or not needed, use `:ensure nil` or add `:nodep` to the use-package declaration.

Package name mappings are in `packageNameMap` in `dependencies.nix`. If a package has a different name in nixpkgs vs elisp, add a mapping there.

### 2. Development Environment Isolation

The dev shell (`nix/dev/shell.nix`) creates complete isolation:
- All XDG directories point to `.dev-home/` (gitignored)
- `JOTAIN_DEV_MODE=1` environment variable enables debug output
- `emacs-dev` wrapper script automatically sets up isolated environment
- Changes to elisp files take effect immediately (no rebuild needed)

**Critical**: The dev shell provides two commands:
- `emacs-dev`: Launches Emacs with local elisp sources and isolated config
- `jot`: CLI tool for running commands (when cli/ is ready)

### 3. Module Loading System

`elisp/jotain.el` implements a module system with strict loading order:
1. Core modules (lib, paths, packages, gc, core)
2. UI modules (themes, fonts, modeline)
3. Completion stack (orderless → vertico → marginalia → consult → corfu → embark)
4. Editor modules
5. Programming language modules

**Critical**: Completion stack order matters! Orderless must load before vertico, marginalia before consult, etc. This ensures proper integration between packages.

Each module uses `jotain-load-module` which tracks loaded modules and enables hot-reloading in dev mode via `jotain-reload-module`.

### 4. Dual-Mode Operation

Jotain operates in two modes:

**Development mode** (JOTAIN_DEV_MODE=1):
- Loads elisp from local source tree
- Enables debug messages for module loading
- Uses isolated .dev-home/ directory
- Allows module hot-reloading

**Production mode**:
- Loads elisp from /nix/store/
- No debug output
- Uses user's normal XDG directories
- Installed via home-manager or NixOS modules

## Essential Commands

### Development Workflow
```bash
# Enter dev shell (automatic with direnv)
nix develop

# Launch isolated Emacs with local sources
emacs-dev

# Run tests (11 smoke tests verify core functionality)
just test-dev

# Build the package
just build

# Run all flake checks (includes smoke tests)
just check

# Show extracted dependencies from elisp files
just show-deps
```

### Testing
```bash
# Run smoke tests in development mode
just test-dev

# Run via Nix checks (more thorough)
just test

# Run specific test interactively
emacs -L elisp -L tests -l test-helper.el -l test-smoke.el -f ert

# Tests are in tests/test-smoke.el (11 tests checking basic functionality)
```

### CLI Commands
```bash
# Health check (verifies Emacs, Jotain, LSP servers)
just doctor

# Evaluate elisp expression
nix develop --command jot eval '(+ 1 2 3)'

# Launch Emacs via CLI
nix develop --command jot run
```

## Adding New Features

### Adding a New Elisp Module

1. Create file in appropriate subdirectory: `elisp/category/jotain-category-feature.el`
2. Add header with `lexical-binding: t` and metadata
3. Use `use-package` for external packages (dependencies auto-detected by Nix)
4. Add `(provide 'jotain-category-feature)` at end
5. Load in `jotain.el` via `(jotain-load-module 'jotain-category-feature)`

Example structure:
```elisp
;;; jotain-category-feature.el --- Feature description -*- lexical-binding: t; -*-

(use-package some-package
  :defer t
  :config
  (setq some-setting value))

(provide 'jotain-category-feature)
;;; jotain-category-feature.el ends here
```

**No manual Nix dependency management needed!** The dependency extraction will find `some-package` automatically.

### Adding Package Name Mappings

If elisp package name differs from nixpkgs name, add to `nix/lib/dependencies.nix`:

```nix
packageNameMap = {
  "elisp-name" = "nixpkgs-name";
  "project" = null;  # Built-in to Emacs, skip
};
```

### Adding a New Programming Language

1. Create `elisp/programming/jotain-programming-LANG.el`
2. Configure language mode, LSP, formatting, etc.
3. Add LSP server to `devTools` in `nix/dev/shell.nix` if needed
4. Load in `jotain.el` initialization

## File Organization Principles

- **nix/**: All Nix infrastructure (packages, modules, lib, dev shell)
- **elisp/**: Emacs Lisp modules organized by category
- **cli/**: Bash CLI with lib/ for shared utilities, commands/ for subcommands
- **templates/**: early-init.el and init.el that users symlink to ~/.config/emacs/
- **tests/**: ERT smoke tests ensuring basic functionality

## Important Implementation Details

### Nix Flake Structure
Uses `flake-parts` for organization. Exports:
- Packages: `jotain`, `emacs`, `emacs-dev`
- Modules: `nixosModules.jotain`, `homeManagerModules.jotain`
- DevShells: `default` (isolated development environment)
- Checks: `build`, `smoke-tests`

### Load Path Setup
In dev mode, all elisp subdirectories are added to load-path:
- elisp/
- elisp/core/
- elisp/ui/
- elisp/completion/
- elisp/editor/
- elisp/programming/

This allows `(require 'jotain-module-name)` to work from any subdirectory.

### Performance Optimizations
- `templates/early-init.el`: Defers GC during startup, configures native-comp
- `elisp/core/jotain-gc.el`: Idle GC every 5s, pauses GC during minibuffer use
- All modules use `:defer t` or `:hook` for lazy loading where appropriate

### XDG Directory Usage
Jotain uses XDG Base Directory specification:
- Cache: `$XDG_CACHE_HOME/jotain/` (compiled files, temp data)
- Data: `$XDG_DATA_HOME/jotain/` (persistent data)
- State: `$XDG_STATE_HOME/jotain/` (session state, history)
- Config: `$XDG_CONFIG_HOME/jotain/` (user config, custom.el)

Paths are defined in `elisp/core/jotain-paths.el` and created on load.

## Troubleshooting Common Issues

### "Package not found in emacsPackages"
1. Check if package name in nixpkgs differs from elisp name
2. Add mapping to `packageNameMap` in `nix/lib/dependencies.nix`
3. Run `just show-deps` to see what was extracted

### "Cannot load module"
1. Verify file exists in correct location
2. Check that subdirectory is added to load-path
3. Ensure `(provide 'module-name)` matches filename
4. In dev mode, check `jotain-modules` variable for loaded modules

### Module Loading Order Issues
Completion stack dependencies must load in order:
orderless → vertico → marginalia → consult → corfu → embark

If changing order, update `jotain-initialize` in `elisp/jotain.el`.

### Tests Failing
Run `just test-dev` for faster iteration. Tests verify:
- Emacs version 30.1+
- Lexical binding enabled
- jotain.el parses without errors
- Core variables and functions defined
- Load paths configured correctly

## Installation Methods

### Home Manager
```nix
programs.jotain.enable = true;
```
Installs Emacs with Jotain, sets EDITOR/VISUAL, creates ~/.config/emacs/init.el

### NixOS
```nix
programs.jotain.enable = true;
```
Installs system-wide, includes fonts (FiraCode, JetBrainsMono, Nerd Fonts)

Both modules are in `nix/modules/` with basic enable option only (MVP).
